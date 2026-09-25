# Runtime/repository release-checklist regressions for Task 5.
if (!exists("contracts_harness_loaded", mode = "function")) {
  source(file.path(if (basename(getwd()) == "tests") "." else "tests",
                   "helper-contracts.R"))
}

test_that("Git wrapper is directory-independent and distinguishes launch failure", {
  repo_root <- repo_root_path()
  release_env <- new.env(parent = globalenv())
  sys.source(file.path(repo_root, "R", "release_checklist.R"), envir = release_env)

  outside_repo <- withr::local_tempdir()
  withr::local_dir(outside_repo)
  result <- release_env$release_git_output(
    c("rev-parse", "--show-toplevel"),
    repo_root
  )

  expect_identical(result$status, 0L)
  expect_equal(
    normalizePath(result$output[[1]], winslash = "/", mustWork = TRUE),
    normalizePath(repo_root, winslash = "/", mustWork = TRUE)
  )
  expect_identical(getwd(), normalizePath(outside_repo, winslash = "/"))

  launch_failure <- release_env$release_git_output(
    c("status", "--short"),
    repo_root,
    system2_runner = function(...) stop("Git executable unavailable")
  )
  expect_identical(launch_failure$status, 127L)
  expect_true(isTRUE(launch_failure$launch_error))
  expect_match(paste(launch_failure$output, collapse = " "),
               "Git executable unavailable", fixed = TRUE)
})

test_that("runtime context makes zero Git calls and reports repository checks unavailable", {
  repo_root <- repo_root_path()
  release_env <- new.env(parent = globalenv())
  sys.source(file.path(repo_root, "R", "data_vintage.R"), envir = release_env)
  sys.source(file.path(repo_root, "R", "pipeline_contracts.R"), envir = release_env)
  sys.source(file.path(repo_root, "R", "release_checklist.R"), envir = release_env)

  git_calls <- 0L
  unavailable_git <- function(args, repo_root) {
    git_calls <<- git_calls + 1L
    list(
      status = 127L,
      output = "Git executable unavailable",
      launch_error = TRUE
    )
  }

  repository_checks <- release_env$release_checklist(
    repo_root = repo_root,
    data_dir = file.path(repo_root, "data"),
    context = "repository",
    git_runner = unavailable_git
  )
  expect_gt(git_calls, 0L)
  repository_git_rows <- repository_checks[
    repository_checks$check_id %in% c(
      "deployment_manifest_data_files",
      "hygiene_no_tracked_ignored_files",
      "hygiene_no_local_artifacts_staged"
    ),
    , drop = FALSE
  ]
  expect_true(all(repository_git_rows$status == "fail"))
  expect_true(any(grepl("unavailable", repository_git_rows$detail,
                        ignore.case = TRUE)))
  expect_error(
    release_env$validate_release_checklist(
      repo_root = repo_root,
      data_dir = file.path(repo_root, "data"),
      git_runner = unavailable_git
    ),
    "Git is unavailable",
    fixed = TRUE
  )

  git_calls <- 0L
  runtime_checks <- release_env$release_checklist(
    repo_root = repo_root,
    data_dir = file.path(repo_root, "data"),
    context = "runtime",
    git_runner = unavailable_git
  )
  expect_identical(git_calls, 0L)
  runtime_git_rows <- runtime_checks[
    runtime_checks$check_id %in% c(
      "hygiene_no_tracked_ignored_files",
      "hygiene_no_local_artifacts_staged"
    ),
    , drop = FALSE
  ]
  expect_true(all(runtime_git_rows$status == "warn"))
  expect_false(any(runtime_git_rows$status == "pass"))
  expect_true(all(grepl("unavailable", runtime_git_rows$detail,
                        ignore.case = TRUE)))
  runtime_summary <- release_env$release_confidence_summary(
    repo_root = repo_root,
    data_dir = file.path(repo_root, "data"),
    context = "runtime",
    git_runner = unavailable_git
  )
  expect_identical(git_calls, 0L)
  expect_match(
    runtime_summary$detail[runtime_summary$label == "Release checks"],
    "remain enforced by CI",
    fixed = TRUE
  )
})

test_that("runtime context still fails missing data and manifest members", {
  skip_if_not_installed("jsonlite")
  repo_root <- repo_root_path()
  release_env <- new.env(parent = globalenv())
  sys.source(file.path(repo_root, "R", "data_vintage.R"), envir = release_env)
  sys.source(file.path(repo_root, "R", "pipeline_contracts.R"), envir = release_env)
  sys.source(file.path(repo_root, "R", "release_checklist.R"), envir = release_env)

  git_calls <- 0L
  counting_git <- function(args, repo_root) {
    git_calls <<- git_calls + 1L
    list(status = 0L, output = character(), launch_error = FALSE)
  }

  data_fixture <- file.path(withr::local_tempdir(), "data")
  dir.create(data_fixture)
  data_files <- list.files(file.path(repo_root, "data"), pattern = "\\.csv$",
                           full.names = TRUE)
  expect_true(all(file.copy(data_files, data_fixture)))
  unlink(file.path(data_fixture, "abs_timeseries.csv"))

  missing_data <- release_env$release_checklist(
    repo_root = repo_root,
    data_dir = data_fixture,
    context = "runtime",
    git_runner = counting_git
  )
  missing_data_row <- missing_data[
    missing_data$check_id == "data_abs_timeseries_exists",
    , drop = FALSE
  ]
  expect_identical(missing_data_row$status, "fail")
  expect_identical(git_calls, 0L)

  manifest <- jsonlite::fromJSON(file.path(repo_root, "manifest.json"),
                                 simplifyVector = FALSE)
  bundle_root <- withr::local_tempdir()
  manifest_files <- names(manifest$files)
  expect_false("renv/activate.R" %in% manifest_files)
  for (relative_path in manifest_files) {
    source_path <- file.path(repo_root, relative_path)
    target_path <- file.path(bundle_root, relative_path)
    dir.create(dirname(target_path), recursive = TRUE, showWarnings = FALSE)
    expect_true(file.copy(source_path, target_path, overwrite = TRUE),
                info = relative_path)
  }
  expect_true(file.exists(file.path(bundle_root, "manifest.json")))
  expect_false(file.exists(file.path(bundle_root, ".git")))

  bundle_runtime <- release_env$release_checklist(
    repo_root = bundle_root,
    data_dir = file.path(bundle_root, "data"),
    context = "runtime",
    git_runner = counting_git
  )
  runtime_renv <- bundle_runtime[
    bundle_runtime$check_id == "reproducibility_renv_activate",
    , drop = FALSE
  ]
  expect_identical(runtime_renv$status, "warn")
  expect_match(runtime_renv$detail, "unavailable", ignore.case = TRUE)
  expect_identical(git_calls, 0L)

  bundle_repository <- release_env$release_checklist(
    repo_root = bundle_root,
    data_dir = file.path(bundle_root, "data"),
    context = "repository",
    git_runner = counting_git
  )
  repository_renv <- bundle_repository[
    bundle_repository$check_id == "reproducibility_renv_activate",
    , drop = FALSE
  ]
  expect_identical(repository_renv$status, "fail")

  git_calls <- 0L
  manifest$files[["R/methodology_module.R"]] <- NULL
  jsonlite::write_json(manifest, file.path(bundle_root, "manifest.json"),
                       auto_unbox = TRUE, pretty = TRUE)

  missing_manifest <- release_env$release_checklist(
    repo_root = bundle_root,
    data_dir = file.path(bundle_root, "data"),
    context = "runtime",
    git_runner = counting_git
  )
  missing_manifest_row <- missing_manifest[
    missing_manifest$check_id == "deployment_manifest_sourced_files",
    , drop = FALSE
  ]
  expect_identical(missing_manifest_row$status, "fail")
  expect_match(missing_manifest_row$detail, "R/methodology_module.R", fixed = TRUE)
  expect_identical(git_calls, 0L)
})

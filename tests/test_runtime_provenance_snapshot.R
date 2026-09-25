# Process-scoped Methodology runtime snapshot regressions for Task 5.
if (!exists("contracts_harness_loaded", mode = "function")) {
  source(file.path(if (basename(getwd()) == "tests") "." else "tests",
                   "helper-contracts.R"))
}

runtime_snapshot_test_env <- function(repo_root) {
  env <- new.env(parent = globalenv())
  for (file in c(
    "project_paths.R", "data_vintage.R", "pipeline_contracts.R",
    "indicator_registry.R", "indicator_context.R", "source_audit_registry.R",
    "release_checklist.R", "provenance_report.R"
  )) {
    sys.source(file.path(repo_root, "R", file), envir = env)
  }
  env
}

test_that("supplied confidence prevents provenance checklist recomputation", {
  repo_root <- repo_root_path()
  env <- runtime_snapshot_test_env(repo_root)
  env$release_confidence_summary <- function(...) {
    stop("confidence summary must not be recomputed")
  }
  supplied <- data.frame(
    label = "Snapshot marker",
    value = "Prepared once",
    status = "warn",
    detail = "Supplied process-scoped confidence",
    stringsAsFactors = FALSE
  )

  report <- env$methodology_provenance_report(
    generated_at = as.POSIXct("2026-09-16 01:02:03", tz = "UTC"),
    data_dir = file.path(repo_root, "data"),
    confidence_summary = supplied
  )

  expect_match(report, "Snapshot marker", fixed = TRUE)
  expect_match(report, "Prepared once", fixed = TRUE)
})

test_that("one runtime snapshot serves two sessions and repeated downloads", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("scales")
  repo_root <- repo_root_path()
  env <- runtime_snapshot_test_env(repo_root)

  inventory_calls <- 0L
  git_calls <- 0L
  original_inventory <- env$dashboard_data_inventory
  original_git <- env$release_git_output
  env$dashboard_data_inventory <- function(...) {
    inventory_calls <<- inventory_calls + 1L
    original_inventory(...)
  }
  env$release_git_output <- function(...) {
    git_calls <<- git_calls + 1L
    original_git(...)
  }

  snapshot <- env$methodology_runtime_snapshot(
    repo_root = repo_root,
    data_dir = file.path(repo_root, "data"),
    generated_at = as.POSIXct("2026-09-16 01:02:03", tz = "UTC")
  )
  expect_identical(inventory_calls, 1L)
  expect_identical(git_calls, 0L)
  expect_named(snapshot, c("confidence", "provenance"), ignore.order = FALSE)
  expect_match(snapshot$provenance, "data/abs_timeseries.csv", fixed = TRUE)

  suppressPackageStartupMessages({
    library(shiny)
    library(bslib)
    library(scales)
  })
  sys.source(file.path(repo_root, "R", "dashboard_formatting.R"), envir = env)
  sys.source(file.path(repo_root, "R", "app_ui_helpers.R"), envir = env)
  sys.source(file.path(repo_root, "R", "methodology_module.R"), envir = env)

  for (session_number in 1:2) {
    shiny::testServer(
      env$methodologyPageServer,
      args = list(runtime_snapshot = snapshot),
      {
        rendered <- output$release_confidence_table
        expect_true(length(rendered) > 0L)
        session$setInputs(main_nav = "Methodology")
        session$setInputs(main_nav = "Overview")
        session$flushReact()
        expect_identical(output$release_confidence_table, rendered)
      }
    )
  }

  first_download <- tempfile(fileext = ".md")
  second_download <- tempfile(fileext = ".md")
  env$write_methodology_provenance_snapshot(snapshot, first_download)
  env$write_methodology_provenance_snapshot(snapshot, second_download)
  expect_identical(readLines(first_download, warn = FALSE),
                   readLines(second_download, warn = FALSE))
  expect_identical(inventory_calls, 1L)
  expect_identical(git_calls, 0L)
})

test_that("suspending and resuming a static observer does not rerun it", {
  skip_if_not_installed("shiny")
  session <- shiny::MockShinySession$new()
  on.exit(session$close(), add = TRUE)
  runs <- 0L
  observer <- shiny::withReactiveDomain(session, {
    shiny::observe({
      runs <<- runs + 1L
    })
  })
  on.exit(observer$destroy(), add = TRUE)

  session$flushReact()
  expect_identical(runs, 1L)
  for (i in seq_len(20L)) {
    observer$suspend()
    observer$resume()
    session$flushReact()
  }
  expect_identical(runs, 1L)
})

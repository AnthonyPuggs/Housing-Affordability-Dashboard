# Runs standalone via `Rscript tests/test_rba_raw_cache_hygiene.R` (repo root) and under
# testthat::test_dir("tests"); see tests/helper-contracts.R.
if (!exists("contracts_harness_loaded", mode = "function")) {
  source(file.path(if (basename(getwd()) == "tests") "." else "tests",
                   "helper-contracts.R"))
}

test_that("rba_raw_cache_hygiene contracts", {
  repo_root <- repo_root_path()


  source(file.path(repo_root, "pipeline", "00_config.R"), local = TRUE)

  required_helpers <- c(
    "normalise_rba_csv_cache",
    "rba_csv_parse_problem_count"
  )
  for (helper in required_helpers) {
    check(exists(helper, mode = "function"),
          paste("Missing RBA cache helper:", helper))
  }

  if (all(exists_in(required_helpers, environment()))) {
    fixture <- tempfile(fileext = ".csv")
    writeLines(
      c(
        "Title,A,B,C",
        "Description,Only one description",
        "Series ID,FIRMMCRTD,,",
        "Units,Per cent,Per cent,Per cent",
        "01-Jan-2024,1.00,2.00",
        "02-Jan-2024,1.10,2.10,3.10"
      ),
      fixture
    )

    before_count <- rba_csv_parse_problem_count(fixture)
    check(before_count > 0,
          "Ragged RBA fixture should have readr parse problems before normalisation")

    normalise_rba_csv_cache(fixture)
    after_count <- rba_csv_parse_problem_count(fixture)
    check(identical(after_count, 0L),
          "normalise_rba_csv_cache() should remove readr parse problems")

    normalised_once <- readLines(fixture, warn = FALSE)
    normalise_rba_csv_cache(fixture)
    normalised_twice <- readLines(fixture, warn = FALSE)
    check(identical(normalised_once, normalised_twice),
          "normalise_rba_csv_cache() should be idempotent")
  }

  # Raw caches are download artefacts and must NOT be tracked: committed caches
  # froze CI refreshes because actions/checkout resets file mtimes, so the 24h
  # cache-validity check always passed and the download branch never ran.
  # No glob pathspec: system2() goes through sh on unix, which would expand
  # the pattern in the runner's working directory before git sees it.
  tracked_files <- system2(
    "git",
    c("-C", repo_root, "ls-files"),
    stdout = TRUE,
    stderr = TRUE
  )
  tracked_cache_files <- grep("^data/rba_.*_raw\\.(csv|xlsx)$",
                              tracked_files, value = TRUE)
  check(length(tracked_cache_files) == 0,
        paste("RBA raw caches must not be git-tracked:",
              paste(tracked_cache_files, collapse = ", ")))

  # Locally present caches (downloaded by a pipeline run) must stay rectangular.
  local_cache_files <- Sys.glob(file.path(repo_root, "data", "rba_*_raw.csv"))
  if (exists("rba_csv_parse_problem_count", mode = "function")) {
    for (cache_file in local_cache_files) {
      problem_count <- rba_csv_parse_problem_count(cache_file)
      check(identical(problem_count, 0L),
            paste(basename(cache_file), "has", problem_count,
                  "readr parse problems"))
    }
  }

  rba_stage_text <- paste(
    readLines(file.path(repo_root, "pipeline", "03_fetch_rba.R"), warn = FALSE),
    collapse = "\n"
  )
  required_stage_text <- c(
    'source(project_path("pipeline", "00_config.R"))',
    'normalise_rba_csv_cache(file)'
  )
  for (needle in required_stage_text) {
    check(grepl(needle, rba_stage_text, fixed = TRUE),
          paste("pipeline/03_fetch_rba.R missing required text:", needle))
  }

  description_lines <- readLines(file.path(repo_root, "DESCRIPTION"), warn = FALSE)
  check(!any(grepl("^\\s*data\\.table\\s*,?\\s*$", description_lines)),
        "DESCRIPTION must not add data.table for RBA cache hygiene")
  check(!any(grepl("^\\s*vroom\\s*,?\\s*$", description_lines)),
        "DESCRIPTION must not add vroom for RBA cache hygiene")
})
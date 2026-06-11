# Shared contract-test harness.
#
# Loaded automatically by testthat::test_dir("tests") (helper-*.R convention)
# and bootstrapped by each test file when run standalone via
# `Rscript tests/test_<name>.R` from the repository root. Both invocation
# styles are supported: standalone runs error on the first failing
# expectation (non-zero exit for CI loops), test_dir reports every failure.

library(testthat)

# This suite never runs on CRAN; without this, shinytest2::AppDriver's
# internal skip_on_cran() silently skips the app smoke test under test_dir.
if (!nzchar(Sys.getenv("NOT_CRAN"))) {
  Sys.setenv(NOT_CRAN = "true")
}

contracts_harness_loaded <- function() TRUE

# Repository root regardless of runner: standalone scripts run from the repo
# root; testthat::test_dir() runs with the working directory set to tests/.
repo_root_path <- function() {
  root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  if (basename(root) == "tests") {
    root <- dirname(root)
  }
  root
}

# Drop-in replacement for the previous hand-rolled per-file harness:
# check(cond, msg) was collect-and-stop; it is now a testthat expectation,
# so test_dir reports each failure with its message and keeps going.
check <- function(condition, message) {
  expect_true(isTRUE(condition), info = message)
}

# Module files carry defensive source() guards whose fallback paths are
# repo-root-relative; under test_dir the working directory is tests/, so the
# real project_path() must be available before any module is sourced.
if (!exists("project_path", mode = "function")) {
  source(file.path(repo_root_path(), "R", "project_paths.R"))
}

# exists() called through vapply() evaluates in vapply's frame and cannot see
# objects sourced into the test environment; tests use this wrapper instead.
exists_in <- function(names, envir, mode = "function") {
  vapply(names, exists, logical(1), mode = mode, envir = envir,
         inherits = TRUE)
}

# Point plot_setup.R at the frozen fixture set in tests/fixtures/data for the
# duration of the calling test, so unit/module tests that boot the app data
# layer are immune to scheduled data refreshes (review TEST-04). Live-data
# contract tests do not call this and keep reading data/ directly.
use_fixture_data <- function(envir = parent.frame()) {
  fixture_dir <- file.path(repo_root_path(), "tests", "fixtures", "data")
  if (!dir.exists(fixture_dir)) {
    stop("Frozen fixture data missing; run: ",
         "Rscript tests/fixtures/generate_fixtures.R")
  }
  withr::local_envvar(
    c(HOUSING_DASHBOARD_DATA_DIR = fixture_dir),
    .local_envir = envir
  )
  invisible(fixture_dir)
}

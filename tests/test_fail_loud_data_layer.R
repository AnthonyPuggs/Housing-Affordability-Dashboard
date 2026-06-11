repo_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
failures <- character()

check <- function(condition, message) {
  if (!isTRUE(condition)) {
    failures <<- c(failures, message)
  }
}

suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(readr)
})

source(file.path(repo_root, "R", "project_paths.R"))
source(file.path(repo_root, "R", "pipeline_contracts.R"))
source(file.path(repo_root, "R", "data_loader.R"))

# --- App-side loader: typed empties, warnings, startup assertion --------------

broken_dir <- tempfile("fail-loud-data-")
dir.create(broken_dir)
write.csv(
  data.frame(
    date = "2026-01-01", value = 1, indicator = "X", geography = "National",
    unit = "Index", frequency = "Quarter"
  ),
  file.path(broken_dir, "affordability_indices.csv"),
  row.names = FALSE
)
# Column-broken file: right name, wrong columns.
write.csv(
  data.frame(wrong = 1),
  file.path(broken_dir, "rba_rates.csv"),
  row.names = FALSE
)

missing_result <- withCallingHandlers(
  load_dashboard_csv("abs_timeseries.csv", data_dir = broken_dir),
  warning = function(w) {
    check(grepl("data/abs_timeseries.csv is missing", conditionMessage(w),
                fixed = TRUE),
          "Missing-file warning must name the file")
    invokeRestart("muffleWarning")
  }
)
check(nrow(missing_result) == 0,
      "Missing file must load as an empty placeholder")
check(all(c("date", "value", "series", "series_id") %in% names(missing_result)),
      "Empty placeholder must carry the contract columns (typed empty tibble)")
check(inherits(missing_result$date, "Date"),
      "Empty placeholder date column must be Date-typed")
check(grepl("is missing", attr(missing_result, "data_problem"), fixed = TRUE),
      "Missing file must attach a data_problem attribute")

broken_result <- suppressWarnings(
  load_dashboard_csv("rba_rates.csv", data_dir = broken_dir)
)
check(grepl("missing columns", attr(broken_result, "data_problem"),
            fixed = TRUE),
      "Column-broken file must attach a missing-columns problem")
check(nrow(broken_result) == 0,
      "Column-broken file must load as an empty placeholder")

ok_result <- load_dashboard_csv("affordability_indices.csv",
                                data_dir = broken_dir)
check(nrow(ok_result) == 1 && is.null(attr(ok_result, "data_problem")),
      "A valid file must load without a problem attribute")

all_loaded <- suppressWarnings(load_dashboard_csvs(data_dir = broken_dir))
problems <- attr(all_loaded, "data_problems")
check(length(problems) >= 12,
      "load_dashboard_csvs() must collect a problem per broken/missing file")
check(any(grepl("data/sih_costs_2020.csv is missing", problems, fixed = TRUE)),
      "Collected problems must name each missing file")

startup_error <- tryCatch({
  assert_dashboard_data(all_loaded)
  NULL
}, error = function(e) conditionMessage(e))
check(!is.null(startup_error),
      "assert_dashboard_data() must stop on broken inputs")
check(grepl("Dashboard startup failed", startup_error, fixed = TRUE) &&
        grepl("data/abs_timeseries.csv is missing", startup_error,
              fixed = TRUE) &&
        grepl("05_driver.R", startup_error, fixed = TRUE),
      "Startup failure must be named, list the broken files and say how to fix")

real_loaded <- load_dashboard_csvs(data_dir = file.path(repo_root, "data"))
check(length(attr(real_loaded, "data_problems")) == 0,
      "Saved data/ directory must load with zero problems")
check(isTRUE(tryCatch(assert_dashboard_data(real_loaded),
                      error = function(e) FALSE)),
      "assert_dashboard_data() must pass on the saved data directory")

# --- Pipeline selection helpers (00_config) ------------------------------------

source(file.path(repo_root, "pipeline", "00_config.R"))

empty_selection_error <- tryCatch({
  assert_selection_nonempty(tibble(), "Building approvals total")
  NULL
}, error = function(e) conditionMessage(e))
check(grepl("Building approvals total", empty_selection_error, fixed = TRUE),
      "assert_selection_nonempty() must name the failed selection")
check(isTRUE(tryCatch({
  assert_selection_nonempty(tibble(x = 1), "ok")
  TRUE
}, error = function(e) FALSE)),
      "assert_selection_nonempty() must pass through non-empty selections")

collision <- list(
  a = tibble(date = as.Date("2026-01-01"), value = 1, series = "Same name",
             series_id = "ID1", category = "X", unit = "U", frequency = "Q"),
  b = tibble(date = as.Date("2026-01-01"), value = 2, series = "Same name",
             series_id = "ID2", category = "X", unit = "U", frequency = "Q")
)
collision_error <- tryCatch({
  combine_series_unique(collision, "test_dataset")
  NULL
}, error = function(e) conditionMessage(e))
check(grepl("duplicate (date, series)", collision_error, fixed = TRUE) &&
        grepl("Same name", collision_error, fixed = TRUE),
      "combine_series_unique() must fail loudly on cross-source name collisions")

clean <- combine_series_unique(
  list(collision$a,
       tibble(date = as.Date("2026-01-01"), value = 3, series = "Other",
              series_id = "ID3", category = "X", unit = "U", frequency = "Q")),
  "test_dataset"
)
check(nrow(clean) == 2,
      "combine_series_unique() must pass collision-free combinations")

typed <- tibble(
  date = as.Date(rep("2026-01-01", 3)),
  value = c(1, 2, 3),
  series = "Approvals",
  series_type = c("Original", "Trend", "Seasonally Adjusted")
)
preferred <- prefer_series_type(typed)
check(nrow(preferred) == 1 &&
        preferred$series_type == "Seasonally Adjusted",
      "prefer_series_type() must keep the most-preferred variant only")
original_only <- prefer_series_type(
  tibble(date = as.Date("2026-01-01"), value = 1, series = "S",
         series_type = "Original")
)
check(nrow(original_only) == 1,
      "prefer_series_type() must keep series that only publish one variant")

# pipeline_problem(): warning in lax mode, error in strict mode.
PIPELINE_STRICT <- FALSE
lax <- tryCatch({
  withCallingHandlers(
    pipeline_problem("lax-mode problem"),
    warning = function(w) invokeRestart("muffleWarning")
  )
  "warned"
}, error = function(e) "stopped")
check(identical(lax, "warned"),
      "pipeline_problem() must warn when PIPELINE_STRICT is FALSE")
PIPELINE_STRICT <- TRUE
strict <- tryCatch({
  pipeline_problem("strict-mode problem")
  "warned"
}, error = function(e) conditionMessage(e))
check(identical(strict, "strict-mode problem"),
      "pipeline_problem() must stop when PIPELINE_STRICT is TRUE")

# --- Run-freshness contract -----------------------------------------------------

fresh_dir <- tempfile("freshness-")
dir.create(fresh_dir)
write.csv(data.frame(x = 1), file.path(fresh_dir, "rba_rates.csv"),
          row.names = FALSE)

fresh_pass <- validate_pipeline_stage_freshness(
  "rba", run_started_at = Sys.time() - 60, data_dir = fresh_dir, fail = FALSE
)
check(length(fresh_pass) == 0,
      "Freshness gate must pass for files written after run start")

stale_failures <- validate_pipeline_stage_freshness(
  "rba", run_started_at = Sys.time() + 3600, data_dir = fresh_dir, fail = FALSE
)
check(length(stale_failures) == 1 &&
        grepl("was not rewritten by this run", stale_failures[[1]],
              fixed = TRUE),
      "Freshness gate must flag outputs older than the run start")

missing_failures <- validate_pipeline_stage_freshness(
  "indicators", run_started_at = Sys.time(), data_dir = fresh_dir, fail = FALSE
)
check(length(missing_failures) == 1 &&
        grepl("is missing", missing_failures[[1]], fixed = TRUE),
      "Freshness gate must flag missing outputs")

stale_stop <- tryCatch({
  validate_pipeline_stage_freshness("rba", run_started_at = Sys.time() + 3600,
                                    data_dir = fresh_dir)
  NULL
}, error = function(e) conditionMessage(e))
check(grepl("outputs are stale for this run", stale_stop, fixed = TRUE),
      "Freshness gate must stop by default on stale outputs")

if (length(failures) > 0) {
  stop(
    paste(c("Fail-loud data layer checks failed:", paste0("- ", failures)),
          collapse = "\n"),
    call. = FALSE
  )
}

cat("Fail-loud data layer checks passed.\n")

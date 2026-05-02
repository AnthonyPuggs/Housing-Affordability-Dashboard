# ==============================================================================
# 05_driver.R — Master pipeline script
# ==============================================================================
# Run: Rscript pipeline/05_driver.R
#
# Executes the full data pipeline in order:
#   00_config.R           → shared config, paths, helpers
#   01_process_sih.R      → parse SIH Excel workbooks → data/sih_*.csv
#   02_fetch_abs_timeseries.R → live ABS series → data/abs_timeseries.csv
#   03_fetch_rba.R        → RBA statistical tables → data/rba_rates.csv
#   04_derive_indicators.R → affordability indices → data/affordability_indices.csv
# ==============================================================================

start_time <- Sys.time()

.load_pipeline_driver_project_paths <- function(envir = parent.frame()) {
  source_file <- NULL
  frames <- sys.frames()
  for (i in rev(seq_along(frames))) {
    frame <- frames[[i]]
    if (exists("ofile", envir = frame, inherits = FALSE)) {
      source_file <- get("ofile", envir = frame, inherits = FALSE)
      break
    }
  }

  starts <- unique(c(
    if (!is.null(source_file)) dirname(normalizePath(source_file, winslash = "/", mustWork = TRUE)),
    getwd()
  ))
  candidates <- unique(c(
    file.path(starts, "R", "project_paths.R"),
    file.path(dirname(starts), "R", "project_paths.R")
  ))
  candidates <- candidates[file.exists(candidates)]
  if (length(candidates) == 0) {
    stop("Could not locate R/project_paths.R for pipeline driver.", call. = FALSE)
  }
  source(candidates[[1]], local = envir)
}

.load_pipeline_driver_project_paths()
rm(.load_pipeline_driver_project_paths)

PROJECT_ROOT <- project_root()

cat("========================================\n")
cat("  Household Affordability Data Pipeline\n")
cat("========================================\n")
cat("Project root:", PROJECT_ROOT, "\n")
cat("Started:", format(start_time), "\n\n")

run_step <- function(label, path) {
  tryCatch(
    source(path, local = parent.frame(), chdir = FALSE),
    error = function(e) {
      stop(label, " failed: ", conditionMessage(e), call. = FALSE)
    }
  )
}

# --- Step 0: Config ---
cat("=== Step 0: Loading configuration ===\n")
run_step("Step 0", project_path("pipeline", "00_config.R"))
source(project_path("R", "pipeline_contracts.R"), local = TRUE)
cat("\n")

# --- Step 1: SIH ---
cat("=== Step 1: Processing SIH workbooks ===\n")
run_step("Step 1", project_path("pipeline", "01_process_sih.R"))
invisible(validate_pipeline_stage_outputs("sih", fail = TRUE))
cat("\n")

# --- Step 2: ABS time series ---
cat("=== Step 2: Fetching ABS time series ===\n")
run_step("Step 2", project_path("pipeline", "02_fetch_abs_timeseries.R"))
invisible(validate_pipeline_stage_outputs("abs_timeseries", fail = TRUE))
cat("\n")

# --- Step 2b: ABS supply & demand ---
cat("=== Step 2b: Fetching ABS supply & demand ===\n")
run_step("Step 2b", project_path("pipeline", "02b_fetch_abs_supply.R"))
invisible(validate_pipeline_stage_outputs("abs_supply", fail = TRUE))
cat("\n")

# --- Step 3: RBA ---
cat("=== Step 3: Fetching RBA data ===\n")
run_step("Step 3", project_path("pipeline", "03_fetch_rba.R"))
invisible(validate_pipeline_stage_outputs("rba", fail = TRUE))
cat("\n")

# --- Step 4: Derived indicators ---
cat("=== Step 4: Deriving affordability indicators ===\n")
run_step("Step 4", project_path("pipeline", "04_derive_indicators.R"))
invisible(validate_pipeline_stage_outputs("indicators", fail = TRUE))
cat("\n")

# --- Step 5: Validation ---
cat("=== Step 5: Validating pipeline outputs ===\n")
run_step("Step 5", project_path("pipeline", "06_validate_outputs.R"))
cat("\n")

# --- Summary ---
cat("========================================\n")
cat("  Pipeline complete\n")
cat("========================================\n")

elapsed <- round(difftime(Sys.time(), start_time, units = "secs"), 1)
cat("Elapsed time:", elapsed, "seconds\n\n")

# List output files
output_files <- list.files(DATA_DIR, pattern = "\\.csv$", full.names = FALSE)
if (length(output_files) > 0) {
  cat("Output files in data/:\n")
  for (f in output_files) {
    size <- file.size(file.path(DATA_DIR, f))
    cat(sprintf("  %-40s %s\n", f, format(size, big.mark = ",")))
  }
} else {
  cat("WARNING: No output files generated.\n")
}

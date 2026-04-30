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

# Set working directory to project root (one level up from pipeline/)
setwd(dirname(dirname(
  if (interactive()) {
    rstudioapi::getSourceEditorContext()$path
  } else {
    # Get the script path when run via Rscript
    args <- commandArgs(trailingOnly = FALSE)
    script_arg <- args[grep("--file=", args)]
    if (length(script_arg) > 0) {
      normalizePath(sub("--file=", "", script_arg))
    } else {
      normalizePath("pipeline/05_driver.R")
    }
  }
)))

cat("========================================\n")
cat("  Household Affordability Data Pipeline\n")
cat("========================================\n")
cat("Working directory:", getwd(), "\n")
cat("Started:", format(start_time), "\n\n")

run_step <- function(label, path) {
  tryCatch(
    source(path),
    error = function(e) {
      stop(label, " failed: ", conditionMessage(e), call. = FALSE)
    }
  )
}

# --- Step 0: Config ---
cat("=== Step 0: Loading configuration ===\n")
run_step("Step 0", "pipeline/00_config.R")
cat("\n")

# --- Step 1: SIH ---
cat("=== Step 1: Processing SIH workbooks ===\n")
run_step("Step 1", "pipeline/01_process_sih.R")
cat("\n")

# --- Step 2: ABS time series ---
cat("=== Step 2: Fetching ABS time series ===\n")
run_step("Step 2", "pipeline/02_fetch_abs_timeseries.R")
cat("\n")

# --- Step 2b: ABS supply & demand ---
cat("=== Step 2b: Fetching ABS supply & demand ===\n")
run_step("Step 2b", "pipeline/02b_fetch_abs_supply.R")
cat("\n")

# --- Step 3: RBA ---
cat("=== Step 3: Fetching RBA data ===\n")
run_step("Step 3", "pipeline/03_fetch_rba.R")
cat("\n")

# --- Step 4: Derived indicators ---
cat("=== Step 4: Deriving affordability indicators ===\n")
run_step("Step 4", "pipeline/04_derive_indicators.R")
cat("\n")

# --- Step 5: Validation ---
cat("=== Step 5: Validating pipeline outputs ===\n")
run_step("Step 5", "pipeline/06_validate_outputs.R")
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

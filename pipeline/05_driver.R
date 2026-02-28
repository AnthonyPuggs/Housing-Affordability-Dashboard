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

# --- Step 0: Config ---
cat("=== Step 0: Loading configuration ===\n")
source("pipeline/00_config.R")
cat("\n")

# --- Step 1: SIH ---
cat("=== Step 1: Processing SIH workbooks ===\n")
tryCatch(
  source("pipeline/01_process_sih.R"),
  error = function(e) warning("Step 1 failed: ", conditionMessage(e))
)
cat("\n")

# --- Step 2: ABS time series ---
cat("=== Step 2: Fetching ABS time series ===\n")
tryCatch(
  source("pipeline/02_fetch_abs_timeseries.R"),
  error = function(e) warning("Step 2 failed: ", conditionMessage(e))
)
cat("\n")

# --- Step 2b: ABS supply & demand ---
cat("=== Step 2b: Fetching ABS supply & demand ===\n")
tryCatch(
  source("pipeline/02b_fetch_abs_supply.R"),
  error = function(e) warning("Step 2b failed: ", conditionMessage(e))
)
cat("\n")

# --- Step 3: RBA ---
cat("=== Step 3: Fetching RBA data ===\n")
tryCatch(
  source("pipeline/03_fetch_rba.R"),
  error = function(e) warning("Step 3 failed: ", conditionMessage(e))
)
cat("\n")

# --- Step 4: Derived indicators ---
cat("=== Step 4: Deriving affordability indicators ===\n")
tryCatch(
  source("pipeline/04_derive_indicators.R"),
  error = function(e) warning("Step 4 failed: ", conditionMessage(e))
)
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

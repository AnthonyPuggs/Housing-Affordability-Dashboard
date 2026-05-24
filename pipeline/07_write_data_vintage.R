# ==============================================================================
# 07_write_data_vintage.R — Persist data vintage metadata for dashboard display
# ==============================================================================

cat("--- Writing data vintage metadata ---\n")

if (!exists("write_data_vintage", mode = "function")) {
  source(project_path("R", "data_vintage.R"))
}

write_data_vintage(DATA_DIR, refreshed_at = Sys.time())

cat("--- Data vintage metadata complete ---\n")

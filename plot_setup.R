# ==============================================================================
# Shared Data Loading, Helpers & Theme
# ==============================================================================
# Sourced by both app.R (Shiny dashboard) and save_plots.R (static export).
# This file is a compatibility entrypoint: page modules may keep using the
# existing global helper names and app-ready data objects.
# ==============================================================================

library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(scales)
library(rlang)
library(lubridate)

.load_plot_setup_project_paths <- function(envir = parent.frame()) {
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
    if (!is.null(source_file)) {
      dirname(normalizePath(source_file, winslash = "/", mustWork = TRUE))
    },
    getwd()
  ))
  candidates <- unique(c(
    file.path(starts, "R", "project_paths.R"),
    file.path(dirname(starts), "R", "project_paths.R")
  ))
  candidates <- candidates[file.exists(candidates)]
  if (length(candidates) == 0) {
    stop("Could not locate R/project_paths.R for data loading.", call. = FALSE)
  }
  source(candidates[[1]], local = envir)
}

if (!exists("project_path", mode = "function")) {
  .load_plot_setup_project_paths()
}
rm(.load_plot_setup_project_paths)

source(project_path("R", "indicator_registry.R"), local = TRUE)
source(project_path("R", "sih_quality_helpers.R"), local = TRUE)
source(project_path("R", "data_loader.R"), local = TRUE)
source(project_path("R", "dashboard_formatting.R"), local = TRUE)
source(project_path("R", "dashboard_theme.R"), local = TRUE)
source(project_path("R", "precomputed_series.R"), local = TRUE)

data_dir <- project_path("data")

dashboard_data <- load_dashboard_csvs(data_dir)
list2env(dashboard_data, envir = environment())
rm(dashboard_data)

dashboard_series <- precompute_dashboard_series(
  abs_ts = abs_ts,
  rba_rates = rba_rates,
  afford_idx = afford_idx
)
list2env(dashboard_series, envir = environment())
rm(dashboard_series)

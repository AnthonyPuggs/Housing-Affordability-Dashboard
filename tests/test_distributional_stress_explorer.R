repo_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
failures <- character()

check <- function(condition, message) {
  if (!isTRUE(condition)) {
    failures <<- c(failures, message)
  }
}

module_path <- file.path(repo_root, "R", "affordability_module.R")
chart_path <- file.path(repo_root, "R", "chart_builders.R")
quality_path <- file.path(repo_root, "R", "sih_quality_helpers.R")

check(file.exists(module_path), "R/affordability_module.R does not exist")
check(file.exists(chart_path), "R/chart_builders.R does not exist")
check(file.exists(quality_path), "R/sih_quality_helpers.R does not exist")

if (file.exists(module_path)) {
  module_text <- paste(readLines(module_path, warn = FALSE), collapse = "\n")
  required_module_text <- c(
    "Distributional Stress Explorer",
    'selectInput(ns("dist_measure")',
    'selectInput(ns("dist_tenure")',
    'selectInput(ns("dist_group")',
    'plotlyOutput(ns("distributional_stress")',
    "output$distributional_stress <- renderPlotly",
    "distributional_stress_data(",
    "build_distributional_stress_plot(",
    "Official SIH/NHHA burden measure",
    "not modelled market-entry"
  )
  missing_module_text <- required_module_text[
    !vapply(required_module_text, grepl, logical(1), module_text, fixed = TRUE)
  ]
  check(length(missing_module_text) == 0,
        paste("Distributional stress UI/server is missing:",
              paste(missing_module_text, collapse = "; ")))
}

if (all(file.exists(c(module_path, chart_path, quality_path)))) {
  suppressPackageStartupMessages({
    library(shiny)
    library(bslib)
    library(plotly)
    library(ggplot2)
    library(dplyr)
    library(scales)
    library(stringr)
  })
  source(file.path(repo_root, "R", "project_paths.R"))
  source(file.path(repo_root, "R", "indicator_registry.R"))
  source(file.path(repo_root, "R", "visual_semantics.R"))
  source(file.path(repo_root, "R", "dashboard_theme.R"))
  source(file.path(repo_root, "R", "ui_style_system.R"))
  source(file.path(repo_root, "R", "app_ui_helpers.R"))
  source(file.path(repo_root, "R", "dashboard_formatting.R"))
  source(quality_path)
  source(chart_path)

  afford_idx <- read.csv(file.path(repo_root, "data", "affordability_indices.csv"),
                         stringsAsFactors = FALSE)
  afford_idx$date <- as.Date(afford_idx$date)
  sih_stress <- read.csv(file.path(repo_root, "data", "sih_stress_bands_2020.csv"),
                         stringsAsFactors = FALSE)
  sih_cost_ratios <- read.csv(file.path(repo_root, "data", "sih_cost_ratios_2020.csv"),
                              stringsAsFactors = FALSE)
  sih_lower_income_states <- read.csv(
    file.path(repo_root, "data", "sih_lower_income_states.csv"),
    stringsAsFactors = FALSE
  )
  sih_nhha <- read.csv(file.path(repo_root, "data", "sih_nhha_rental_stress.csv"),
                       stringsAsFactors = FALSE)
  sih_quality <- read.csv(file.path(repo_root, "data", "sih_estimate_quality.csv"),
                          stringsAsFactors = FALSE)

  source(module_path)

  check(exists("distributional_stress_data", mode = "function"),
        "distributional_stress_data() must be defined")
  check(exists("build_distributional_stress_plot", mode = "function"),
        "build_distributional_stress_plot() must be defined")

  if (exists("distributional_stress_data", mode = "function")) {
    default_view <- distributional_stress_data(
      measure = "nhha_state",
      tenure = "renter_lower_income",
      group = "state",
      quality = sih_quality
    )
    required_cols <- c(
      "survey_year",
      "value",
      "measure_label",
      "group_label",
      "measure_class",
      "quality_hover",
      "reliability_marker",
      "interval_label"
    )
    missing_cols <- setdiff(required_cols, names(default_view))
    check(length(missing_cols) == 0,
          paste("distributional_stress_data() missing columns:",
                paste(missing_cols, collapse = ", ")))
    check(nrow(default_view) > 0,
          "Default lower-income renter state view must return rows")
    check(all(default_view$measure_class == "official_survey"),
          "Distributional stress explorer must identify SIH/NHHA rows as official_survey")
    check(any(grepl("NHHA", default_view$measure_label, fixed = TRUE)),
          "Default distributional view must expose NHHA wording")
    check(any(grepl("95%|RSE|standard|caution|unreliable",
                    default_view$quality_hover)),
          "Distributional stress rows must include SIH quality hover text")
  }

  if (exists("build_distributional_stress_plot", mode = "function") &&
      exists("distributional_stress_data", mode = "function")) {
    default_view <- distributional_stress_data(
      measure = "nhha_state",
      tenure = "renter_lower_income",
      group = "state",
      quality = sih_quality
    )
    p <- build_distributional_stress_plot(default_view, dark = FALSE)
    check(inherits(p, "ggplot"),
          "build_distributional_stress_plot() must return a ggplot")
  }
}

if (length(failures) > 0) {
  stop(
    paste(c("Distributional stress explorer checks failed:",
            paste0("- ", failures)),
          collapse = "\n"),
    call. = FALSE
  )
}

cat("Distributional stress explorer checks passed.\n")

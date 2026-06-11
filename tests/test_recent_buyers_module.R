# Runs standalone via `Rscript tests/test_recent_buyers_module.R` (repo root) and under
# testthat::test_dir("tests"); see tests/helper-contracts.R.
if (!exists("contracts_harness_loaded", mode = "function")) {
  source(file.path(if (basename(getwd()) == "tests") "." else "tests",
                   "helper-contracts.R"))
}

test_that("recent_buyers_module contracts", {
  repo_root <- repo_root_path()


  helper_path <- file.path(repo_root, "R", "app_ui_helpers.R")
  buyers_helper_path <- file.path(repo_root, "R", "recent_buyers_helpers.R")
  module_path <- file.path(repo_root, "R", "recent_buyers_module.R")
  app_path <- file.path(repo_root, "app.R")
  readme_path <- file.path(repo_root, "README.md")

  check(file.exists(helper_path), "R/app_ui_helpers.R does not exist")
  check(file.exists(buyers_helper_path), "R/recent_buyers_helpers.R does not exist")
  check(file.exists(module_path), "R/recent_buyers_module.R does not exist")
  check(file.exists(app_path), "app.R does not exist")
  check(file.exists(readme_path), "README.md does not exist")

  if (file.exists(module_path)) {
    parsed <- tryCatch({
      parse(module_path)
      TRUE
    }, error = function(e) conditionMessage(e))
    check(identical(parsed, TRUE),
          paste(module_path, "does not parse:", parsed))
  }

  if (all(file.exists(c(helper_path, buyers_helper_path, module_path)))) {
    suppressPackageStartupMessages({
      library(shiny)
      library(bslib)
      library(plotly)
      library(dplyr)
    })
    source(helper_path, local = TRUE)

    sih_sampling_error_note <- "SIH estimates are survey estimates."

    source(module_path, local = TRUE)

    check(exists("recentBuyersPageUI", mode = "function"),
          "recentBuyersPageUI() must be defined")
    check(exists("recentBuyersPageServer", mode = "function"),
          "recentBuyersPageServer() must be defined")

    module_ui <- paste(as.character(recentBuyersPageUI("recent_buyers")),
                       collapse = "\n")
    required_ui_text <- c(
      "Recent Buyers",
      "Financial metric",
      "Dwelling type",
      "Household profile",
      "Financial Characteristics by Buyer Type",
      "Household Profile by Buyer Type",
      "2019-20 SIH recent home buyer households",
      "sampling-error metadata for File 9",
      "recent_buyers-recent_buyers_chart",
      "recent_buyers-recent_buyers_profile_chart",
      "recent_buyers-recent_buyers_metric",
      "recent_buyers-recent_buyers_dwelling",
      "recent_buyers-recent_buyers_profile",
      "recent_buyers-recent_buyers_summary"
    )
    missing_ui_text <- required_ui_text[
      !vapply(required_ui_text, grepl, logical(1), module_ui, fixed = TRUE)
    ]
    check(length(missing_ui_text) == 0,
          paste("recentBuyersPageUI() missing expected UI text/IDs:",
                paste(missing_ui_text, collapse = "; ")))
  }

  if (file.exists(module_path)) {
    module_text <- paste(readLines(module_path, warn = FALSE), collapse = "\n")
    required_module_text <- c(
      "recentBuyersPageUI <- function(id)",
      "recentBuyersPageServer <- function(id, is_dark)",
      "NS(id)",
      "moduleServer",
      "normalise_recent_buyers(",
      "normalise_recent_buyers_profile(",
      "recent_buyers_metric_choices(",
      "recent_buyers_profile_choices(",
      "recent_buyers_summary(",
      'plotlyOutput(ns("recent_buyers_chart")',
      'plotlyOutput(ns("recent_buyers_profile_chart")',
      "output$recent_buyers_chart <- renderPlotly",
      "output$recent_buyers_profile_chart <- renderPlotly",
      "output$recent_buyers_summary <- renderUI",
      "bindCache(input$recent_buyers_metric, input$recent_buyers_dwelling,",
      "bindCache(input$recent_buyers_profile, is_dark())",
      # Quality machinery wired (File 9 has no published metadata yet, so the
      # markers stay dormant; the join degrades to a not-available hover note).
      "join_sih_quality(",
      "quality_hover",
      "build_recent_buyers_plot(",
      "build_recent_buyers_profile_plot(",
      "dashboard_ggplotly",
      'tooltip = "text"'
    )
    missing_module_text <- required_module_text[
      !vapply(required_module_text, grepl, logical(1), module_text, fixed = TRUE)
    ]
    check(length(missing_module_text) == 0,
          paste("R/recent_buyers_module.R missing module constructs:",
                paste(missing_module_text, collapse = "; ")))
    check(!grepl("ggplot(", module_text, fixed = TRUE),
          "R/recent_buyers_module.R should delegate ggplot construction to R/chart_builders.R")
  }

  if (file.exists(app_path)) {
    app_text <- paste(readLines(app_path, warn = FALSE), collapse = "\n")
    required_app_text <- c(
      'source(project_path("R", "recent_buyers_module.R"), local = TRUE)',
      'recentBuyersPageUI("recent_buyers")',
      'recentBuyersPageServer("recent_buyers", is_dark = is_dark)'
    )
    missing_app_text <- required_app_text[
      !vapply(required_app_text, grepl, logical(1), app_text, fixed = TRUE)
    ]
    check(length(missing_app_text) == 0,
          paste("app.R missing recent buyers module wiring:",
                paste(missing_app_text, collapse = "; ")))
  }

  # The Affordability module must no longer carry the old Recent Buyers tab.
  affordability_path <- file.path(repo_root, "R", "affordability_module.R")
  if (file.exists(affordability_path)) {
    affordability_text <- paste(readLines(affordability_path, warn = FALSE),
                                collapse = "\n")
    check(!grepl("output$recent_buyers_chart", affordability_text, fixed = TRUE),
          "R/affordability_module.R must not keep the old Recent Buyers outputs")
  }

  if (file.exists(readme_path)) {
    readme_text <- paste(readLines(readme_path, warn = FALSE), collapse = "\n")
    required_readme <- c(
      "R/recent_buyers_module.R",
      "Rscript tests/test_recent_buyers_module.R"
    )
    missing_readme <- required_readme[
      !vapply(required_readme, grepl, logical(1), readme_text, fixed = TRUE)
    ]
    check(length(missing_readme) == 0,
          paste("README.md must document the Recent Buyers module:",
                paste(missing_readme, collapse = "; ")))
  }
})
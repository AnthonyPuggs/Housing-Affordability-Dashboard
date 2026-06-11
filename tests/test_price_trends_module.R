# Runs standalone via `Rscript tests/test_price_trends_module.R` (repo root) and under
# testthat::test_dir("tests"); see tests/helper-contracts.R.
if (!exists("contracts_harness_loaded", mode = "function")) {
  source(file.path(if (basename(getwd()) == "tests") "." else "tests",
                   "helper-contracts.R"))
}

test_that("price_trends_module contracts", {
  repo_root <- repo_root_path()


  helper_path <- file.path(repo_root, "R", "app_ui_helpers.R")
  chart_builder_path <- file.path(repo_root, "R", "chart_builders.R")
  module_path <- file.path(repo_root, "R", "price_trends_module.R")
  app_path <- file.path(repo_root, "app.R")
  readme_path <- file.path(repo_root, "README.md")

  check(file.exists(helper_path), "R/app_ui_helpers.R does not exist")
  check(file.exists(chart_builder_path), "R/chart_builders.R does not exist")
  check(file.exists(module_path), "R/price_trends_module.R does not exist")
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

  if (all(file.exists(c(helper_path, chart_builder_path, module_path)))) {
    suppressPackageStartupMessages({
      library(shiny)
      library(bslib)
      library(plotly)
      library(ggplot2)
      library(dplyr)
    })
    source(file.path(repo_root, "R", "dashboard_formatting.R"), local = TRUE)
    source(file.path(repo_root, "R", "dashboard_theme.R"), local = TRUE)
    source(helper_path, local = TRUE)
    source(chart_builder_path, local = TRUE)

    rppi_cities <- c("Sydney", "Melbourne", "Weighted average of eight capital cities")
    rppi_states <- c("Australia", "New South Wales", "Victoria", "Queensland")
    rent_cpi_national_city <- "Weighted average of eight capital cities"
    rent_cpi_cities <- c(
      rent_cpi_national_city, "Brisbane", "Melbourne", "Perth", "Sydney"
    )
    rent_cpi_city_cities <- setdiff(rent_cpi_cities, rent_cpi_national_city)
    rent_cpi_default_cities <- c("Sydney", "Melbourne", "Brisbane", "Perth")
    rent_cpi_national_range <- as.Date(c("1972-07-01", "2026-01-01"))
    rent_cpi_city_range <- as.Date(c("2022-07-01", "2026-01-01"))
    rent_cpi_combined <- data.frame(
      date = as.Date(c(
        "2012-01-01", "2026-01-01",
        "2022-07-01", "2026-01-01",
        "2022-07-01", "2026-01-01"
      )),
      value = c(73.5, 101.5, 83, 101.4, 85.4, 101.1),
      city = c(
        rent_cpi_national_city, rent_cpi_national_city,
        "Sydney", "Sydney",
        "Melbourne", "Melbourne"
      ),
      stringsAsFactors = FALSE
    )

    source(module_path, local = TRUE)

    check(exists("priceTrendsPageUI", mode = "function"),
          "priceTrendsPageUI() must be defined")
    check(exists("priceTrendsPageServer", mode = "function"),
          "priceTrendsPageServer() must be defined")

    module_ui <- paste(as.character(priceTrendsPageUI("price_trends")),
                       collapse = "\n")
    required_ui_text <- c(
      "Price Trends",
      "Dwelling Price Index",
      "Rent CPI",
      "Capital Cities",
      "States &amp; Territories",
      "Dwelling Type",
      "state means",
      "city medians",
      "whole-of-state mean-price indexes",
      "Transform",
      "Data Type",
      "Rent CPI View",
      "Eight-capital-city average (long run)",
      "Capital-city comparison",
      "Include weighted-average reference",
      "City CPI rent series in the saved data are post-rebase and available only from July 2022",
      "Rent Consumer Price Index (CPI) by Greater Capital City",
      "price_trends-rent_cpi_view",
      "price_trends-price_cities",
      "price_trends-price_dwelling",
      "price_trends-price_dates",
      "price_trends-price_transform",
      "price_trends-rent_cpi_cities",
      "price_trends-rent_cpi_include_national",
      "price_trends-rent_cpi_datatype",
      "price_trends-rent_cpi_dates",
      "price_trends-price_chart",
      "price_trends-rent_cpi_chart"
    )
    missing_ui_text <- required_ui_text[
      !vapply(required_ui_text, grepl, logical(1), module_ui, fixed = TRUE)
    ]
    check(length(missing_ui_text) == 0,
          paste("priceTrendsPageUI() missing expected UI text/IDs:",
                paste(missing_ui_text, collapse = "; ")))
  }

  if (file.exists(module_path)) {
    module_text <- paste(readLines(module_path, warn = FALSE), collapse = "\n")
    required_module_text <- c(
      "priceTrendsPageUI <- function(id)",
      "priceTrendsPageServer <- function(id, is_dark)",
      "NS(id)",
      "moduleServer",
      "price_data <- reactive",
      "rent_cpi_data <- reactive",
      "input$rent_cpi_view",
      "updateSliderInput",
      "updateRadioButtons",
      # Geography choices must swap with the dwelling-type view (state means
      # vs genuine capital-city medians - review STAT-01).
      "price_geography_choices(",
      "price_geography_default(",
      "updateSelectizeInput",
      "price_series_transform(",
      "rent_cpi_series_transform(",
      "build_dwelling_price_plot(",
      "build_rent_cpi_plot(",
      'plotlyOutput(ns("price_chart")',
      'plotlyOutput(ns("rent_cpi_chart")',
      "output$price_chart <- renderPlotly",
      "output$rent_cpi_chart <- renderPlotly",
      "bindCache(input$price_cities, input$price_dwelling, input$price_dates,",
      "input$price_transform, is_dark())",
      "bindCache(input$rent_cpi_cities, input$rent_cpi_dates,",
      "input$rent_cpi_datatype, input$rent_cpi_view,",
      "input$rent_cpi_include_national, is_dark())",
      "dashboard_ggplotly"
    )
    missing_module_text <- required_module_text[
      !vapply(required_module_text, grepl, logical(1), module_text, fixed = TRUE)
    ]
    check(length(missing_module_text) == 0,
          paste("R/price_trends_module.R missing module constructs:",
                paste(missing_module_text, collapse = "; ")))
    check(!grepl("ggplot(", module_text, fixed = TRUE),
          "R/price_trends_module.R should delegate ggplot construction to R/chart_builders.R")
  }

  if (file.exists(app_path)) {
    app_text <- paste(readLines(app_path, warn = FALSE), collapse = "\n")
    required_app_text <- c(
      'source(project_path("R", "chart_builders.R"), local = TRUE)',
      'source(project_path("R", "price_trends_module.R"), local = TRUE)',
      'priceTrendsPageUI("price_trends")',
      'priceTrendsPageServer("price_trends", is_dark = is_dark)'
    )
    missing_app_text <- required_app_text[
      !vapply(required_app_text, grepl, logical(1), app_text, fixed = TRUE)
    ]
    check(length(missing_app_text) == 0,
          paste("app.R missing price trends module wiring:",
                paste(missing_app_text, collapse = "; ")))
    check(!grepl("output$price_chart <- renderPlotly", app_text,
                 fixed = TRUE),
          "app.R must not keep the old inline Price Trends Plotly outputs")
    check(!grepl("price_data <- reactive", app_text, fixed = TRUE),
          "app.R must not keep the old inline price_data reactive")
    check(!grepl('nav_panel(\n    "Price Trends"', app_text, fixed = TRUE),
          "app.R must not keep the old inline Price Trends UI")
  }

  if (file.exists(readme_path)) {
    readme_text <- paste(readLines(readme_path, warn = FALSE), collapse = "\n")
    check(grepl("R/chart_builders.R", readme_text, fixed = TRUE),
          "README.md must document the chart builder helper surface")
    check(grepl("R/price_trends_module.R", readme_text, fixed = TRUE),
          "README.md must document the Price Trends module pilot")
    check(grepl("Rscript tests/test_chart_builders.R", readme_text, fixed = TRUE),
          "README.md must document the chart builder test command")
  }
})
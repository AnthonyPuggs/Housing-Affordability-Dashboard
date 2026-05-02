repo_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
failures <- character()

check <- function(condition, message) {
  if (!isTRUE(condition)) {
    failures <<- c(failures, message)
  }
}

helper_path <- file.path(repo_root, "R", "app_ui_helpers.R")
registry_path <- file.path(repo_root, "R", "indicator_registry.R")
module_path <- file.path(repo_root, "R", "overview_module.R")
app_path <- file.path(repo_root, "app.R")
readme_path <- file.path(repo_root, "README.md")

check(file.exists(helper_path), "R/app_ui_helpers.R does not exist")
check(file.exists(registry_path), "R/indicator_registry.R does not exist")
check(file.exists(module_path), "R/overview_module.R does not exist")
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

if (all(file.exists(c(helper_path, registry_path, module_path)))) {
  suppressPackageStartupMessages({
    library(shiny)
    library(bslib)
    library(plotly)
  })
  source(helper_path)
  source(registry_path)

  median_prices_combined <- data.frame(
    date = as.Date(c("2010-01-01", "2024-01-01")),
    value = c(500, 900),
    city = c("Sydney", "National Avg"),
    stringsAsFactors = FALSE
  )

  source(module_path)

  check(exists("overviewPageUI", mode = "function"),
        "overviewPageUI() must be defined")
  check(exists("overviewPageServer", mode = "function"),
        "overviewPageServer() must be defined")

  module_ui <- paste(as.character(overviewPageUI("overview")),
                     collapse = "\n")
  required_ui_text <- c(
    "Overview",
    "Housing Affordability",
    "Analysing the state of the Australian market",
    "National Median Price",
    "Sydney Median Price",
    "Modelled Serviceability",
    "Rental Affordability",
    "Capital City Median House Prices",
    "Affordability Indices",
    "Cost-pressure indexes; higher = less affordable",
    "overview-vb_nat_price",
    "overview-vb_nat_price_date",
    "overview-vb_nat_price_change",
    "overview-vb_syd_price",
    "overview-vb_syd_price_date",
    "overview-vb_syd_price_change",
    "overview-vb_service",
    "overview-vb_service_change",
    "overview-vb_rental",
    "overview-vb_rental_date",
    "overview-vb_rental_change",
    "overview-overview_price_subtitle",
    "overview-overview_price_dates",
    "overview-overview_price_transform",
    "overview-overview_median_prices",
    "overview-overview_afford_change"
  )
  missing_ui_text <- required_ui_text[
    !vapply(required_ui_text, grepl, logical(1), module_ui, fixed = TRUE)
  ]
  check(length(missing_ui_text) == 0,
        paste("overviewPageUI() missing expected UI text/IDs:",
              paste(missing_ui_text, collapse = "; ")))
}

if (file.exists(module_path)) {
  module_text <- paste(readLines(module_path, warn = FALSE), collapse = "\n")
  required_module_text <- c(
    "overviewPageUI <- function(id)",
    "overviewPageServer <- function(id, is_dark)",
    "NS(id)",
    "moduleServer",
    "overview_cost_pressure_indicators <- c(",
    "overview_cost_pressure_colours <- stats::setNames(",
    'plotlyOutput(ns("overview_median_prices")',
    'plotlyOutput(ns("overview_afford_change")',
    "output$vb_nat_price <- renderText",
    "output$vb_nat_price_date <- renderText",
    "output$vb_nat_price_change <- renderUI",
    "output$vb_syd_price <- renderText",
    "output$vb_syd_price_date <- renderText",
    "output$vb_syd_price_change <- renderUI",
    "output$vb_service <- renderText",
    "output$vb_service_change <- renderUI",
    "output$vb_rental <- renderText",
    "output$vb_rental_date <- renderText",
    "output$vb_rental_change <- renderUI",
    "output$overview_price_subtitle <- renderUI",
    "output$overview_median_prices <- renderPlotly",
    "output$overview_afford_change <- renderPlotly",
    'kpi_change_class(ch$change, favourable = "decrease")',
    'kpi_change_class(diff_val, favourable = "decrease")',
    "cost_pressure_palette(",
    "dashboard_ggplotly",
    "annotations = annotations",
    "margin = list(r = 100)",
    "bindCache(input$overview_price_dates, input$overview_price_transform,",
    "bindCache(is_dark())"
  )
  missing_module_text <- required_module_text[
    !vapply(required_module_text, grepl, logical(1), module_text, fixed = TRUE)
  ]
  check(length(missing_module_text) == 0,
        paste("R/overview_module.R missing module constructs:",
              paste(missing_module_text, collapse = "; ")))
}

if (file.exists(app_path)) {
  app_text <- paste(readLines(app_path, warn = FALSE), collapse = "\n")
  required_app_text <- c(
    'source(project_path("R", "overview_module.R"), local = TRUE)',
    'overviewPageUI("overview")',
    'overviewPageServer("overview", is_dark = is_dark)'
  )
  missing_app_text <- required_app_text[
    !vapply(required_app_text, grepl, logical(1), app_text, fixed = TRUE)
  ]
  check(length(missing_app_text) == 0,
        paste("app.R missing overview module wiring:",
              paste(missing_app_text, collapse = "; ")))
  check(!grepl("output$overview_median_prices <- renderPlotly", app_text,
               fixed = TRUE),
        "app.R must not keep the old inline Overview Plotly outputs")
  check(!grepl("output$vb_nat_price <- renderText", app_text,
               fixed = TRUE),
        "app.R must not keep the old inline Overview value-box outputs")
  check(!grepl("overview_cost_pressure_indicators <- c(", app_text,
               fixed = TRUE),
        "app.R must not keep Overview-only cost-pressure constants")
  check(!grepl('nav_panel(\n    "Overview"', app_text, fixed = TRUE),
        "app.R must not keep the old inline Overview UI")
}

if (file.exists(readme_path)) {
  readme_text <- paste(readLines(readme_path, warn = FALSE), collapse = "\n")
  check(grepl("R/overview_module.R", readme_text, fixed = TRUE),
        "README.md must document the Overview module pilot")
  check(grepl("Rscript tests/test_overview_module.R",
              readme_text, fixed = TRUE),
        "README.md must document the Overview module test command")
}

if (length(failures) > 0) {
  stop(
    paste(c("Overview module checks failed:", paste0("- ", failures)),
          collapse = "\n"),
    call. = FALSE
  )
}

cat("Overview module checks passed.\n")

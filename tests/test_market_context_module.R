repo_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
failures <- character()

check <- function(condition, message) {
  if (!isTRUE(condition)) {
    failures <<- c(failures, message)
  }
}

helper_path <- file.path(repo_root, "R", "app_ui_helpers.R")
chart_builder_path <- file.path(repo_root, "R", "chart_builders.R")
module_path <- file.path(repo_root, "R", "market_context_module.R")
app_path <- file.path(repo_root, "app.R")
readme_path <- file.path(repo_root, "README.md")

check(file.exists(helper_path), "R/app_ui_helpers.R does not exist")
check(file.exists(chart_builder_path), "R/chart_builders.R does not exist")
check(file.exists(module_path), "R/market_context_module.R does not exist")
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
  source(file.path(repo_root, "R", "dashboard_formatting.R"))
  source(file.path(repo_root, "R", "dashboard_theme.R"))
  source(helper_path)
  source(chart_builder_path)
  source(module_path)

  check(exists("marketContextPageUI", mode = "function"),
        "marketContextPageUI() must be defined")
  check(exists("marketContextPageServer", mode = "function"),
        "marketContextPageServer() must be defined")

  module_ui <- paste(as.character(marketContextPageUI("market_context")),
                     collapse = "\n")
  required_ui_text <- c(
    "Market Context",
    "Labour &amp; Demographics",
    "Analysing the state of the Australian market",
    "Unemployment Rate",
    "Net Overseas Migration",
    "Participation Rate",
    "Date Range",
    "Interest Rates on Residential Mortgages",
    "Labour Market Spare Capacity",
    "Population Demand",
    "market_context-vb_unemp",
    "market_context-vb_unemp_change",
    "market_context-vb_nom",
    "market_context-vb_nom_change",
    "market_context-vb_participation",
    "market_context-vb_participation_change",
    "market_context-context_dates",
    "market_context-context_rates",
    "market_context-context_labour",
    "market_context-context_pop"
  )
  missing_ui_text <- required_ui_text[
    !vapply(required_ui_text, grepl, logical(1), module_ui, fixed = TRUE)
  ]
  check(length(missing_ui_text) == 0,
        paste("marketContextPageUI() missing expected UI text/IDs:",
              paste(missing_ui_text, collapse = "; ")))
}

if (file.exists(module_path)) {
  module_text <- paste(readLines(module_path, warn = FALSE), collapse = "\n")
  required_module_text <- c(
    "marketContextPageUI <- function(id)",
    "marketContextPageServer <- function(id, is_dark)",
    "NS(id)",
    "moduleServer",
    'plotlyOutput(ns("context_rates")',
    'plotlyOutput(ns("context_labour")',
    'plotlyOutput(ns("context_pop")',
    "output$vb_unemp <- renderText",
    "output$vb_unemp_change <- renderUI",
    "output$vb_nom <- renderText",
    "output$vb_nom_change <- renderUI",
    "output$vb_participation <- renderText",
    "output$vb_participation_change <- renderUI",
    "output$context_rates <- renderPlotly",
    "output$context_labour <- renderPlotly",
    "output$context_pop <- renderPlotly",
    "build_context_rates_plot(",
    "build_context_labour_plot(",
    "build_context_population_plot(",
    "latest_change(abs_ts, \"series\", \"Unemployment Rate\"",
    "latest_change(abs_ts, \"series\", \"Participation Rate\"",
    'kpi_change_class(diff_val, favourable = "decrease")',
    'kpi_change_class(pct, favourable = "neutral")',
    'kpi_change_class(diff_val, favourable = "increase")',
    "bindCache(input$context_dates, is_dark())",
    "dashboard_ggplotly"
  )
  missing_module_text <- required_module_text[
    !vapply(required_module_text, grepl, logical(1), module_text, fixed = TRUE)
  ]
  check(length(missing_module_text) == 0,
        paste("R/market_context_module.R missing module constructs:",
              paste(missing_module_text, collapse = "; ")))
  check(!grepl("ggplot(", module_text, fixed = TRUE),
        "R/market_context_module.R should delegate ggplot construction to R/chart_builders.R")
}

if (file.exists(app_path)) {
  app_text <- paste(readLines(app_path, warn = FALSE), collapse = "\n")
  required_app_text <- c(
    'source(project_path("R", "chart_builders.R"), local = TRUE)',
    'source(project_path("R", "market_context_module.R"), local = TRUE)',
    'marketContextPageUI("market_context")',
    'marketContextPageServer("market_context", is_dark = is_dark)'
  )
  missing_app_text <- required_app_text[
    !vapply(required_app_text, grepl, logical(1), app_text, fixed = TRUE)
  ]
  check(length(missing_app_text) == 0,
        paste("app.R missing market context module wiring:",
              paste(missing_app_text, collapse = "; ")))
  check(!grepl("output$context_rates <- renderPlotly", app_text,
               fixed = TRUE),
        "app.R must not keep the old inline Market Context Plotly outputs")
  check(!grepl("output$vb_unemp <- renderText", app_text,
               fixed = TRUE),
        "app.R must not keep the old inline Market Context value-box outputs")
  check(!grepl('nav_panel(\n    "Market Context"', app_text, fixed = TRUE),
        "app.R must not keep the old inline Market Context UI")
}

if (file.exists(readme_path)) {
  readme_text <- paste(readLines(readme_path, warn = FALSE), collapse = "\n")
  check(grepl("R/chart_builders.R", readme_text, fixed = TRUE),
        "README.md must document the chart builder helper surface")
  check(grepl("R/market_context_module.R", readme_text, fixed = TRUE),
        "README.md must document the Market Context module pilot")
  check(grepl("Rscript tests/test_chart_builders.R",
              readme_text, fixed = TRUE),
        "README.md must document the chart builder test command")
  check(grepl("Rscript tests/test_market_context_module.R",
              readme_text, fixed = TRUE),
        "README.md must document the Market Context module test command")
}

if (length(failures) > 0) {
  stop(
    paste(c("Market Context module checks failed:", paste0("- ", failures)),
          collapse = "\n"),
    call. = FALSE
  )
}

cat("Market Context module checks passed.\n")

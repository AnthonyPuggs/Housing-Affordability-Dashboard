repo_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
failures <- character()

check <- function(condition, message) {
  if (!isTRUE(condition)) {
    failures <<- c(failures, message)
  }
}

helper_path <- file.path(repo_root, "R", "app_ui_helpers.R")
module_path <- file.path(repo_root, "R", "rental_market_module.R")
app_path <- file.path(repo_root, "app.R")
readme_path <- file.path(repo_root, "README.md")

check(file.exists(helper_path), "R/app_ui_helpers.R does not exist")
check(file.exists(module_path), "R/rental_market_module.R does not exist")
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

if (all(file.exists(c(helper_path, module_path)))) {
  suppressPackageStartupMessages({
    library(shiny)
    library(bslib)
    library(plotly)
  })
  source(helper_path)

  sih_sampling_error_note <- "SIH estimates are survey estimates."
  sih_nhha <- data.frame(
    survey_year = c("2019-20", "2017-18"),
    geography = c("NSW", "Aust."),
    stringsAsFactors = FALSE
  )

  source(module_path)

  check(exists("rentalMarketPageUI", mode = "function"),
        "rentalMarketPageUI() must be defined")
  check(exists("rentalMarketPageServer", mode = "function"),
        "rentalMarketPageServer() must be defined")

  module_ui <- paste(as.character(rentalMarketPageUI("rental_market")),
                     collapse = "\n")
  required_ui_text <- c(
    "Rental Market",
    "Survey Year (NHHA)",
    "States/Territories",
    "Rental Costs By",
    "NHHA Rental Stress by State",
    "NHHA Rental Stress Trends (Over Time)",
    "Rental Affordability Index",
    "Weekly Rental Costs by Demographics (2019-20)",
    "rental_market-rental_stress_state",
    "rental_market-rental_stress_trend",
    "rental_market-rental_afford_index",
    "rental_market-rental_costs_demo"
  )
  missing_ui_text <- required_ui_text[
    !vapply(required_ui_text, grepl, logical(1), module_ui, fixed = TRUE)
  ]
  check(length(missing_ui_text) == 0,
        paste("rentalMarketPageUI() missing expected UI text/IDs:",
              paste(missing_ui_text, collapse = "; ")))
}

if (file.exists(module_path)) {
  module_text <- paste(readLines(module_path, warn = FALSE), collapse = "\n")
  required_module_text <- c(
    "rentalMarketPageUI <- function(id)",
    "rentalMarketPageServer <- function(id, is_dark)",
    "NS(id)",
    "moduleServer",
    'layout_column_wrap(',
    'width = "420px"',
    'plotlyOutput(ns("rental_stress_state")',
    'plotlyOutput(ns("rental_stress_trend")',
    'plotlyOutput(ns("rental_afford_index")',
    'plotlyOutput(ns("rental_costs_demo")',
    "output$rental_stress_state <- renderPlotly",
    "output$rental_stress_trend <- renderPlotly",
    "output$rental_afford_index <- renderPlotly",
    "output$rental_costs_demo <- renderPlotly",
    "bindCache(input$rental_year, input$rental_states, is_dark())",
    "bindCache(input$rental_states, is_dark())",
    "bindCache(input$rental_cost_breakdown, is_dark())",
    "hoverinfo <- \"skip\"",
    "dashboard_ggplotly",
    "join_sih_quality(",
    "sih_reliability_marker(",
    "sih_quality_hover_text(",
    "reliability_marker",
    "quality_hover",
    "interval_label",
    "build_rental_stress_state_plot(",
    "build_rental_stress_trend_plot(",
    "build_rental_affordability_index_plot(",
    "build_rental_costs_demographic_plot(",
    "tooltip = c(\"x\", \"y\", \"text\")",
    "tooltip = c(\"fill\", \"text\")"
  )
  missing_module_text <- required_module_text[
    !vapply(required_module_text, grepl, logical(1), module_text, fixed = TRUE)
  ]
  check(length(missing_module_text) == 0,
        paste("R/rental_market_module.R missing module constructs:",
              paste(missing_module_text, collapse = "; ")))
  check(!grepl("ggplot(", module_text, fixed = TRUE),
        "R/rental_market_module.R should delegate ggplot construction to R/chart_builders.R")
}

if (file.exists(app_path)) {
  app_text <- paste(readLines(app_path, warn = FALSE), collapse = "\n")
  required_app_text <- c(
    'source(project_path("R", "rental_market_module.R"), local = TRUE)',
    'rentalMarketPageUI("rental_market")',
    'rentalMarketPageServer("rental_market", is_dark = is_dark)'
  )
  missing_app_text <- required_app_text[
    !vapply(required_app_text, grepl, logical(1), app_text, fixed = TRUE)
  ]
  check(length(missing_app_text) == 0,
        paste("app.R missing rental market module wiring:",
              paste(missing_app_text, collapse = "; ")))
  check(!grepl("output$rental_stress_state <- renderPlotly", app_text,
               fixed = TRUE),
        "app.R must not keep the old inline rental market Plotly outputs")
  check(!grepl('nav_panel(\n    "Rental Market"', app_text, fixed = TRUE),
        "app.R must not keep the old inline Rental Market UI")
}

if (file.exists(readme_path)) {
  readme_text <- paste(readLines(readme_path, warn = FALSE), collapse = "\n")
  check(grepl("R/rental_market_module.R", readme_text, fixed = TRUE),
        "README.md must document the Rental Market module pilot")
}

if (length(failures) > 0) {
  stop(
    paste(c("Rental Market module checks failed:", paste0("- ", failures)),
          collapse = "\n"),
    call. = FALSE
  )
}

cat("Rental Market module checks passed.\n")

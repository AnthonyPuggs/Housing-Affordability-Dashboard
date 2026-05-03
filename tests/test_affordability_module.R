repo_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
failures <- character()

check <- function(condition, message) {
  if (!isTRUE(condition)) {
    failures <<- c(failures, message)
  }
}

helper_path <- file.path(repo_root, "R", "app_ui_helpers.R")
scenario_path <- file.path(repo_root, "R", "market_entry_scenarios.R")
module_path <- file.path(repo_root, "R", "affordability_module.R")
app_path <- file.path(repo_root, "app.R")
readme_path <- file.path(repo_root, "README.md")

check(file.exists(helper_path), "R/app_ui_helpers.R does not exist")
check(file.exists(scenario_path), "R/market_entry_scenarios.R does not exist")
check(file.exists(module_path), "R/affordability_module.R does not exist")
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

if (all(file.exists(c(helper_path, scenario_path, module_path)))) {
  suppressPackageStartupMessages({
    library(shiny)
    library(bslib)
    library(plotly)
  })
  source(file.path(repo_root, "R", "indicator_registry.R"))
  source(helper_path)
  source(scenario_path)

  affordability_ui_indicators <- c(
    "Price-to-Income Ratio",
    "Mortgage Serviceability Index",
    "Rental Affordability Index",
    "Deposit Gap (Years)"
  )
  affordability_indicator_choices <- c(
    stats::setNames(affordability_ui_indicators,
                    indicator_chart_label(affordability_ui_indicators)),
    "Modelled Serviceability" = "Housing Serviceability"
  )
  afford_idx <- data.frame(date = as.Date(c("2003-01-01", "2020-01-01")))
  sih_sampling_error_note <- "SIH estimates are survey estimates."

  source(module_path)

  check(exists("affordabilityPageUI", mode = "function"),
        "affordabilityPageUI() must be defined")
  check(exists("affordabilityPageServer", mode = "function"),
        "affordabilityPageServer() must be defined")

  module_ui <- paste(as.character(affordabilityPageUI("affordability")),
                     collapse = "\n")
  required_ui_text <- c(
    "Affordability",
    "Affordability Analysis",
    "Indices",
    "Calculator",
    "Housing Stress",
    "Cost Burden",
    "affordability-indices-page",
    "affordability-afford_indices_chart",
    "affordability-afford_serviceability",
    "affordability-stress_chart",
    "affordability-burden_heatmap",
    "affordability-calc_repayment",
    "affordability-calc_assessed_ratio",
    "affordability-calc_total_interest"
  )
  missing_ui_text <- required_ui_text[
    !vapply(required_ui_text, grepl, logical(1), module_ui, fixed = TRUE)
  ]
  check(length(missing_ui_text) == 0,
        paste("affordabilityPageUI() missing expected UI text/IDs:",
              paste(missing_ui_text, collapse = "; ")))
}

if (file.exists(module_path)) {
  module_text <- paste(readLines(module_path, warn = FALSE), collapse = "\n")
  required_module_text <- c(
    "affordabilityPageUI <- function(id)",
    "affordabilityPageServer <- function(id, is_dark)",
    "NS(id)",
    "moduleServer",
    'class = "affordability-indices-page"',
    "market_entry_scenario(",
    "market_entry_serviceability_series(",
    'sliderInput(ns("serviceability_buffer")',
    'sliderInput(ns("serviceability_deposit_pct")',
    'sliderInput(ns("serviceability_term")',
    'sliderInput(ns("calc_assessment_buffer")',
    'numericInput(ns("calc_annual_expenses")',
    'numericInput(ns("calc_monthly_debt")',
    'plotlyOutput(ns("afford_indices_chart")',
    'textOutput(ns("calc_repayment")',
    'textOutput(ns("calc_assessed_ratio")',
    "output$afford_indices_chart <- renderPlotly",
    "output$afford_serviceability <- renderPlotly",
    "output$stress_chart <- renderPlotly",
    "output$burden_heatmap <- renderPlotly",
    "output$calc_repayment",
    "output$calc_assessed_ratio",
    "deposit_pct = input$serviceability_deposit_pct",
    "term_years = input$serviceability_term",
    "bindCache(input$afford_indices, input$afford_dates, is_dark())",
    "bindCache(input$afford_indices, input$afford_dates, input$serviceability_deposit_pct, input$serviceability_term, input$serviceability_buffer, is_dark())",
    "bindCache(input$stress_breakdown, input$stress_population, is_dark())",
    "dashboard_ggplotly",
    "Assessment buffer and expense inputs are sensitivity assumptions, not a lender assessment",
    "uses AWE individual earnings as the income proxy",
    "join_sih_quality(",
    "sih_reliability_marker(",
    "sih_quality_hover_text(",
    "reliability_marker",
    "quality_hover",
    "interval_label",
    "build_affordability_indices_plot(",
    "build_market_entry_serviceability_plot(",
    "build_housing_stress_bands_plot(",
    "build_cost_burden_heatmap_plot(",
    "tooltip = c(\"x\", \"y\", \"fill\", \"text\")"
  )
  missing_module_text <- required_module_text[
    !vapply(required_module_text, grepl, logical(1), module_text, fixed = TRUE)
  ]
  check(length(missing_module_text) == 0,
        paste("R/affordability_module.R missing module constructs:",
              paste(missing_module_text, collapse = "; ")))
  check(!grepl("geom_errorbar(", module_text, fixed = TRUE),
        "Affordability stress stacked bars must not draw error bars")
  check(!grepl("ggplot(", module_text, fixed = TRUE),
        "R/affordability_module.R should delegate ggplot construction to R/chart_builders.R")
}

if (file.exists(app_path)) {
  app_text <- paste(readLines(app_path, warn = FALSE), collapse = "\n")
  required_app_text <- c(
    'source(project_path("R", "market_entry_scenarios.R"), local = TRUE)',
    'source(project_path("R", "affordability_module.R"), local = TRUE)',
    'affordabilityPageUI("affordability")',
    'affordabilityPageServer("affordability", is_dark = is_dark)'
  )
  missing_app_text <- required_app_text[
    !vapply(required_app_text, grepl, logical(1), app_text, fixed = TRUE)
  ]
  check(length(missing_app_text) == 0,
        paste("app.R missing affordability module wiring:",
              paste(missing_app_text, collapse = "; ")))
  check(!grepl("output$afford_indices_chart <- renderPlotly", app_text,
               fixed = TRUE),
        "app.R must not keep the old inline affordability Plotly outputs")
  check(!grepl('navset_card_tab(\n      title = "Affordability Analysis"',
               app_text, fixed = TRUE),
        "app.R must not keep the old inline Affordability UI")
}

if (file.exists(readme_path)) {
  readme_text <- paste(readLines(readme_path, warn = FALSE), collapse = "\n")
  check(grepl("R/affordability_module.R", readme_text, fixed = TRUE),
        "README.md must document the Affordability module pilot")
  check(grepl("R/market_entry_scenarios.R", readme_text, fixed = TRUE),
        "README.md must document the market-entry scenario helper")
  check(grepl("Rscript tests/test_market_entry_scenarios.R",
              readme_text, fixed = TRUE),
        "README.md must document the market-entry scenario test command")
}

if (length(failures) > 0) {
  stop(
    paste(c("Affordability module checks failed:", paste0("- ", failures)),
          collapse = "\n"),
    call. = FALSE
  )
}

cat("Affordability module checks passed.\n")

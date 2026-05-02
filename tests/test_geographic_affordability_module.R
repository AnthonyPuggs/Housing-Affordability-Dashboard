repo_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
failures <- character()

check <- function(condition, message) {
  if (!isTRUE(condition)) {
    failures <<- c(failures, message)
  }
}

helper_path <- file.path(repo_root, "R", "app_ui_helpers.R")
plot_setup_path <- file.path(repo_root, "plot_setup.R")
module_path <- file.path(repo_root, "R", "geographic_affordability_module.R")
app_path <- file.path(repo_root, "app.R")
readme_path <- file.path(repo_root, "README.md")

check(file.exists(helper_path), "R/app_ui_helpers.R does not exist")
check(file.exists(plot_setup_path), "plot_setup.R does not exist")
check(file.exists(module_path), "R/geographic_affordability_module.R does not exist")
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

if (all(file.exists(c(helper_path, plot_setup_path, module_path)))) {
  suppressPackageStartupMessages({
    library(shiny)
    library(bslib)
    library(plotly)
  })
  source(plot_setup_path)
  source(helper_path)
  source(module_path)

  check(exists("geographicAffordabilityPageUI", mode = "function"),
        "geographicAffordabilityPageUI() must be defined")
  check(exists("geographicAffordabilityPageServer", mode = "function"),
        "geographicAffordabilityPageServer() must be defined")

  module_ui <- paste(as.character(
    geographicAffordabilityPageUI("geographic_affordability")
  ), collapse = "\n")
  required_ui_text <- c(
    "Geographic Affordability",
    "Geography-aligned SIH affordability measures",
    "States/Territories",
    "State trend measure",
    "State tenure",
    "Lower-income state measure",
    "Lower-income tenure",
    "Capital/rest-of-state measure",
    "Capital/rest-of-state tenure",
    "Capital/rest-of-state geographies",
    "State SIH Cost-to-Income Trend",
    "Latest State Comparison",
    "Lower-Income State Burden",
    "Capital City / Rest-of-State Comparison",
    "geographic_affordability-geo_states",
    "geographic_affordability-geo_state_metric",
    "geographic_affordability-geo_state_tenure",
    "geographic_affordability-geo_lower_metric",
    "geographic_affordability-geo_lower_tenure",
    "geographic_affordability-geo_gcc_geographies",
    "geographic_affordability-geo_gcc_metric",
    "geographic_affordability-geo_gcc_tenure",
    "geographic_affordability-geo_state_trend",
    "geographic_affordability-geo_state_latest",
    "geographic_affordability-geo_lower_income",
    "geographic_affordability-geo_gcc_comparison"
  )
  missing_ui_text <- required_ui_text[
    !vapply(required_ui_text, grepl, logical(1), module_ui, fixed = TRUE)
  ]
  check(length(missing_ui_text) == 0,
        paste("geographicAffordabilityPageUI() missing expected UI text/IDs:",
              paste(missing_ui_text, collapse = "; ")))
}

if (file.exists(module_path)) {
  module_text <- paste(readLines(module_path, warn = FALSE), collapse = "\n")
  required_module_text <- c(
    "geographicAffordabilityPageUI <- function(id)",
    "geographicAffordabilityPageServer <- function(id, is_dark)",
    "NS(id)",
    "moduleServer",
    'plotlyOutput(ns("geo_state_trend")',
    'plotlyOutput(ns("geo_state_latest")',
    'plotlyOutput(ns("geo_lower_income")',
    'plotlyOutput(ns("geo_gcc_comparison")',
    "output$geo_state_trend <- renderPlotly",
    "output$geo_state_latest <- renderPlotly",
    "output$geo_lower_income <- renderPlotly",
    "output$geo_gcc_comparison <- renderPlotly",
    "dashboard_ggplotly",
    "bindCache(input$geo_states, input$geo_state_metric, input$geo_state_tenure, is_dark())",
    "bindCache(input$geo_states, input$geo_lower_metric, input$geo_lower_tenure, is_dark())",
    "bindCache(input$geo_gcc_metric, input$geo_gcc_tenure",
    "input$geo_gcc_geographies, is_dark())",
    "geography-aligned",
    "not modelled market-entry indexes",
    "join_sih_quality(",
    "sih_reliability_marker(",
    "sih_quality_hover_text(",
    "reliability_marker",
    "quality_hover",
    "tooltip = c(\"x\", \"y\", \"text\")"
  )
  missing_module_text <- required_module_text[
    !vapply(required_module_text, grepl, logical(1), module_text, fixed = TRUE)
  ]
  check(length(missing_module_text) == 0,
        paste("R/geographic_affordability_module.R missing module constructs:",
              paste(missing_module_text, collapse = "; ")))

  forbidden_terms <- c(
    "afford_idx",
    "rppi",
    "awe_ts",
    "mortgage_rate_qtr",
    "market_entry_scenario",
    "WPI",
    "price-to-income"
  )
  leaked_terms <- forbidden_terms[
    vapply(forbidden_terms, grepl, logical(1), module_text, fixed = TRUE)
  ]
  check(length(leaked_terms) == 0,
        paste("Geographic Affordability module must not use proxy/modelled inputs:",
              paste(leaked_terms, collapse = ", ")))
}

if (file.exists(app_path)) {
  app_text <- paste(readLines(app_path, warn = FALSE), collapse = "\n")
  required_app_text <- c(
    'source(project_path("R", "geographic_affordability_module.R"), local = TRUE)',
    'geographicAffordabilityPageUI("geographic_affordability")',
    'geographicAffordabilityPageServer("geographic_affordability", is_dark = is_dark)'
  )
  missing_app_text <- required_app_text[
    !vapply(required_app_text, grepl, logical(1), app_text, fixed = TRUE)
  ]
  check(length(missing_app_text) == 0,
        paste("app.R missing geographic affordability module wiring:",
              paste(missing_app_text, collapse = "; ")))
}

if (file.exists(readme_path)) {
  readme_text <- paste(readLines(readme_path, warn = FALSE), collapse = "\n")
  check(grepl("R/geographic_affordability_module.R", readme_text, fixed = TRUE),
        "README.md must document the Geographic Affordability module")
  check(grepl("Rscript tests/test_geographic_affordability_module.R",
              readme_text, fixed = TRUE),
        "README.md must document the Geographic Affordability module test")
}

if (length(failures) > 0) {
  stop(
    paste(c("Geographic Affordability module checks failed:",
            paste0("- ", failures)),
          collapse = "\n"),
    call. = FALSE
  )
}

cat("Geographic Affordability module checks passed.\n")

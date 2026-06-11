# Runs standalone via `Rscript tests/test_overview_module.R` (repo root) and under
# testthat::test_dir("tests"); see tests/helper-contracts.R.
if (!exists("contracts_harness_loaded", mode = "function")) {
  source(file.path(if (basename(getwd()) == "tests") "." else "tests",
                   "helper-contracts.R"))
}

test_that("overview_module contracts", {
  repo_root <- repo_root_path()


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
    source(helper_path, local = TRUE)
    source(registry_path, local = TRUE)

    median_prices_combined <- data.frame(
      date = as.Date(c("2010-01-01", "2024-01-01")),
      value = c(500, 900),
      city = c("Sydney", "National Avg"),
      stringsAsFactors = FALSE
    )

    source(module_path, local = TRUE)

    check(exists("overviewPageUI", mode = "function"),
          "overviewPageUI() must be defined")
    check(exists("overviewPageServer", mode = "function"),
          "overviewPageServer() must be defined")
    check(exists("overview_score_date_should_update", mode = "function"),
          "overview_score_date_should_update() must be defined")

    if (exists("overview_score_date_should_update", mode = "function")) {
      check(overview_score_date_should_update(as.Date("2024-01-01"),
                                              as.Date("2023-01-01")),
            "Score-date click guard must update when the clicked date changes")
      check(!overview_score_date_should_update(as.Date("2024-01-01"),
                                               as.Date("2024-01-01")),
            "Score-date click guard must ignore clicks on the already-selected date")
      check(!overview_score_date_should_update(NULL, as.Date("2024-01-01")),
            "Score-date click guard must ignore missing click dates")
    }

    module_ui <- paste(as.character(overviewPageUI("overview")),
                       collapse = "\n")
    required_ui_text <- c(
      "Overview",
      "Housing Affordability",
      "Analysing the state of the Australian market",
      "National Market-Entry Affordability Score",
      "Relative index, not household stress",
      "Higher = easier market entry relative to 2012-2025 history",
      "not the share of households who can afford housing",
      "Modelled national score for entering ownership or renting",
      "mortgage serviceability, rental cost pressure and deposit barriers",
      "Not an official ABS/NHHA statistic or lender assessment",
      "affordability-score-basis",
      "Component scores and weighted contribution",
      "Mortgage = monthly repayment burden",
      "Rental = rent pressure relative to wages",
      "Deposit = upfront saving barrier",
      "Reset to latest",
      "Official SIH/NHHA burden snapshot",
      "Observed household burden measures",
      "overview-official_burden_summary",
      "National Mean Dwelling Price",
      "Highest Capital Median Price",
      "Modelled Serviceability",
      "Rental Affordability",
      "Capital City Median House Prices",
      "Affordability Indices",
      "overview-affordability-indices-title-wrap",
      "overview-affordability-indices-title",
      "policy-info-icon",
      "policy-info-icon-left-aligned",
      "Affordability indices note",
      "Cost-pressure indexes are burden measures where higher = less affordable",
      "overview-vb_nat_price",
      "overview-vb_nat_price_date",
      "overview-vb_nat_price_change",
      "overview-vb_high_capital_price",
      "overview-vb_high_capital_price_city",
      "overview-vb_high_capital_price_change",
      "overview-vb_service",
      "overview-vb_service_change",
      "overview-vb_rental",
      "overview-vb_rental_date",
      "overview-vb_rental_change",
      "overview-vb_afford_score",
      "overview-vb_afford_score_date",
      "overview-vb_afford_score_change",
      "overview-vb_afford_score_basis",
      "overview-reset_afford_score_date",
      "overview-overview_afford_score_trend",
      "overview-overview_afford_score_components",
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
      'plotlyOutput(ns("overview_afford_score_trend")',
      'plotlyOutput(ns("overview_median_prices")',
      'plotlyOutput(ns("overview_afford_change")',
      "output$vb_nat_price <- renderText",
      "output$vb_nat_price_date <- renderText",
      "output$vb_nat_price_change <- renderUI",
      "output$vb_high_capital_price <- renderText",
      "output$vb_high_capital_price_city <- renderText",
      "output$vb_high_capital_price_change <- renderUI",
      "output$vb_service <- renderText",
      "output$vb_service_change <- renderUI",
      "output$vb_rental <- renderText",
      "output$vb_rental_date <- renderText",
      "output$vb_rental_change <- renderUI",
      "output$vb_afford_score <- renderText",
      "output$vb_afford_score_date <- renderText",
      "output$vb_afford_score_change <- renderUI",
      "output$vb_afford_score_basis <- renderText",
      "Relative to ",
      " history",
      "score_component_explanations <- c(",
      "score_component_short_labels <- c(",
      "overview_affordability_indices_note <-",
      "policy_info_icon(",
      'class = "policy-info-icon-left-aligned"',
      "score_click <- reactive({",
      "tryCatch(",
      "overview_score_date_should_update <- function",
      "overview_score_date_should_update(clicked, selected_score_date())",
      "output$overview_afford_score_components <- renderUI",
      "output$official_burden_summary <- renderUI",
      "output$overview_afford_score_trend <- renderPlotly",
      "selected_score_date <- reactiveVal",
      'event_data("plotly_click", source = "overview_afford_score"',
      "observeEvent(input$reset_afford_score_date",
      "output$overview_price_subtitle <- renderUI",
      "output$overview_median_prices <- renderPlotly",
      "output$overview_afford_change <- renderPlotly",
      'kpi_change_class(ch$change, favourable = "decrease")',
      'kpi_change_class(diff_val, favourable = "decrease")',
      "cost_pressure_palette(",
      "overview_price_series_transform(",
      "build_overview_median_prices_plot(",
      "build_national_affordability_score_plot(",
      "latest_capital_price_extreme(",
      "official_burden_summary(",
      'source = "overview_afford_score"',
      "plotly::event_register",
      "dragmode = FALSE",
      "scrollZoom = FALSE",
      "doubleClick = FALSE",
      "modeBarButtonsToRemove = c(",
      '"zoom2d"',
      '"resetScale2d"',
      "selected_score_date()",
      "build_overview_affordability_plot(",
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
    check(!grepl("ggplot(", module_text, fixed = TRUE),
          "R/overview_module.R should delegate ggplot construction to R/chart_builders.R")
    check(!grepl("note = overview_affordability_indices_note", module_text,
                 fixed = TRUE),
          "Affordability Indices explanatory text must live in title tooltip, not card body note")
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
})
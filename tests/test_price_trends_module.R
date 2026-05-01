repo_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
failures <- character()

check <- function(condition, message) {
  if (!isTRUE(condition)) {
    failures <<- c(failures, message)
  }
}

helper_path <- file.path(repo_root, "R", "app_ui_helpers.R")
module_path <- file.path(repo_root, "R", "price_trends_module.R")
app_path <- file.path(repo_root, "app.R")
readme_path <- file.path(repo_root, "README.md")

check(file.exists(helper_path), "R/app_ui_helpers.R does not exist")
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

if (all(file.exists(c(helper_path, module_path)))) {
  suppressPackageStartupMessages({
    library(shiny)
    library(bslib)
    library(plotly)
  })
  source(helper_path)

  rppi_cities <- c("Sydney", "Melbourne", "Weighted average of eight capital cities")
  rent_cpi_cities <- c("Sydney", "Melbourne")
  rent_cpi_combined <- data.frame(
    date = as.Date(c("2012-01-01", "2024-01-01")),
    value = c(100, 130),
    city = c("Sydney", "Melbourne"),
    stringsAsFactors = FALSE
  )

  source(module_path)

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
    "Dwelling Type",
    "Transform",
    "Data Type",
    "Rent Consumer Price Index (CPI) by Greater Capital City",
    "price_trends-price_cities",
    "price_trends-price_dwelling",
    "price_trends-price_dates",
    "price_trends-price_transform",
    "price_trends-rent_cpi_cities",
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
    'plotlyOutput(ns("price_chart")',
    'plotlyOutput(ns("rent_cpi_chart")',
    "output$price_chart <- renderPlotly",
    "output$rent_cpi_chart <- renderPlotly",
    "bindCache(input$price_cities, input$price_dwelling, input$price_dates,",
    "input$price_transform, is_dark())",
    "bindCache(input$rent_cpi_cities, input$rent_cpi_dates,",
    "input$rent_cpi_datatype, is_dark())",
    "dashboard_ggplotly"
  )
  missing_module_text <- required_module_text[
    !vapply(required_module_text, grepl, logical(1), module_text, fixed = TRUE)
  ]
  check(length(missing_module_text) == 0,
        paste("R/price_trends_module.R missing module constructs:",
              paste(missing_module_text, collapse = "; ")))
}

if (file.exists(app_path)) {
  app_text <- paste(readLines(app_path, warn = FALSE), collapse = "\n")
  required_app_text <- c(
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
  check(grepl("R/price_trends_module.R", readme_text, fixed = TRUE),
        "README.md must document the Price Trends module pilot")
}

if (length(failures) > 0) {
  stop(
    paste(c("Price Trends module checks failed:", paste0("- ", failures)),
          collapse = "\n"),
    call. = FALSE
  )
}

cat("Price Trends module checks passed.\n")

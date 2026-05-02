repo_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
failures <- character()

check <- function(condition, message) {
  if (!isTRUE(condition)) {
    failures <<- c(failures, message)
  }
}

helper_path <- file.path(repo_root, "R", "app_ui_helpers.R")
module_path <- file.path(repo_root, "R", "housing_supply_module.R")
app_path <- file.path(repo_root, "app.R")
readme_path <- file.path(repo_root, "README.md")

check(file.exists(helper_path), "R/app_ui_helpers.R does not exist")
check(file.exists(module_path), "R/housing_supply_module.R does not exist")
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
  source(module_path)

  check(exists("housingSupplyPageUI", mode = "function"),
        "housingSupplyPageUI() must be defined")
  check(exists("housingSupplyPageServer", mode = "function"),
        "housingSupplyPageServer() must be defined")

  module_ui <- paste(as.character(housingSupplyPageUI("housing_supply")),
                     collapse = "\n")
  required_ui_text <- c(
    "Housing Supply",
    "Building activity and construction costs",
    "NSW Approvals",
    "VIC Approvals",
    "Construction Costs",
    "Houses Share",
    "Date Range",
    "States/Territories",
    "Building type",
    "Sector",
    "Building Approvals",
    "CPI New Dwelling Purchase (Construction Cost)",
    "housing_supply-supply_states",
    "housing_supply-supply_building_type",
    "housing_supply-supply_sector",
    "housing_supply-vb_approvals_nsw",
    "housing_supply-vb_approvals_vic",
    "housing_supply-vb_construction",
    "housing_supply-vb_houses_share",
    "housing_supply-supply_approvals",
    "housing_supply-supply_cpi_construction"
  )
  missing_ui_text <- required_ui_text[
    !vapply(required_ui_text, grepl, logical(1), module_ui, fixed = TRUE)
  ]
  check(length(missing_ui_text) == 0,
        paste("housingSupplyPageUI() missing expected UI text/IDs:",
              paste(missing_ui_text, collapse = "; ")))
}

if (file.exists(module_path)) {
  module_text <- paste(readLines(module_path, warn = FALSE), collapse = "\n")
  required_module_text <- c(
    "housingSupplyPageUI <- function(id)",
    "housingSupplyPageServer <- function(id, is_dark)",
    "NS(id)",
    "moduleServer",
    'plotlyOutput(ns("supply_approvals")',
    'plotlyOutput(ns("supply_cpi_construction")',
    "output$vb_approvals_nsw <- renderText",
    "output$vb_approvals_vic <- renderText",
    "output$vb_construction <- renderText",
    "output$vb_houses_share <- renderText",
    "output$supply_approvals <- renderPlotly",
    "output$supply_cpi_construction <- renderPlotly",
    "supply_approval_series_components",
    "approval_state %in% input$supply_states",
    "approval_building_type == input$supply_building_type",
    "approval_sector == input$supply_sector",
    "color = approval_label",
    "date >= input$supply_dates[1]",
    "date <= input$supply_dates[2]",
    "bindCache(input$supply_dates, input$supply_states, input$supply_building_type, input$supply_sector, is_dark())",
    "dashboard_ggplotly"
  )
  missing_module_text <- required_module_text[
    !vapply(required_module_text, grepl, logical(1), module_text, fixed = TRUE)
  ]
  check(length(missing_module_text) == 0,
        paste("R/housing_supply_module.R missing module constructs:",
              paste(missing_module_text, collapse = "; ")))
  check(!grepl("input$price_dates", module_text, fixed = TRUE),
        "Housing Supply module must not reference input$price_dates")
}

if (file.exists(app_path)) {
  app_text <- paste(readLines(app_path, warn = FALSE), collapse = "\n")
  required_app_text <- c(
    'source(project_path("R", "housing_supply_module.R"), local = TRUE)',
    'housingSupplyPageUI("housing_supply")',
    'housingSupplyPageServer("housing_supply", is_dark = is_dark)'
  )
  missing_app_text <- required_app_text[
    !vapply(required_app_text, grepl, logical(1), app_text, fixed = TRUE)
  ]
  check(length(missing_app_text) == 0,
        paste("app.R missing housing supply module wiring:",
              paste(missing_app_text, collapse = "; ")))
  check(!grepl("output$supply_approvals <- renderPlotly", app_text,
               fixed = TRUE),
        "app.R must not keep the old inline Housing Supply Plotly outputs")
  check(!grepl('nav_panel(\n    "Housing Supply"', app_text, fixed = TRUE),
        "app.R must not keep the old inline Housing Supply UI")
}

if (file.exists(readme_path)) {
  readme_text <- paste(readLines(readme_path, warn = FALSE), collapse = "\n")
  check(grepl("R/housing_supply_module.R", readme_text, fixed = TRUE),
        "README.md must document the Housing Supply module pilot")
}

if (length(failures) > 0) {
  stop(
    paste(c("Housing Supply module checks failed:", paste0("- ", failures)),
          collapse = "\n"),
    call. = FALSE
  )
}

cat("Housing Supply module checks passed.\n")

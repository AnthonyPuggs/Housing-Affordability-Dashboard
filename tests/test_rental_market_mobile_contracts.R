repo_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
failures <- character()

check <- function(condition, message) {
  if (!isTRUE(condition)) {
    failures <<- c(failures, message)
  }
}

app_path <- file.path(repo_root, "app.R")
helper_path <- file.path(repo_root, "R", "app_ui_helpers.R")
module_path <- file.path(repo_root, "R", "rental_market_module.R")
readme_path <- file.path(repo_root, "README.md")
description_path <- file.path(repo_root, "DESCRIPTION")

check(file.exists(app_path), "app.R does not exist")
check(file.exists(helper_path), "R/app_ui_helpers.R does not exist")
check(file.exists(module_path), "R/rental_market_module.R does not exist")
check(file.exists(readme_path), "README.md does not exist")
check(file.exists(description_path), "DESCRIPTION does not exist")

if (file.exists(module_path)) {
  parsed <- tryCatch({
    parse(module_path)
    TRUE
  }, error = function(e) conditionMessage(e))
  check(identical(parsed, TRUE),
        paste("R/rental_market_module.R does not parse:", parsed))
}

if (all(file.exists(c(helper_path, module_path)))) {
  suppressPackageStartupMessages({
    library(shiny)
    library(bslib)
    library(plotly)
  })
  source(helper_path)
  source(module_path)

  sih_nhha <- data.frame(
    survey_year = "2019-20",
    geography = "Aust.",
    stringsAsFactors = FALSE
  )

  module_ui <- paste(as.character(rentalMarketPageUI("rental_market")),
                     collapse = "\n")
  required_ui <- c(
    "rental-market-page",
    "rental-market-grid",
    "rental-market-chart",
    "rental-market-chart-square",
    "rental-market-chart-wide",
    "rental_market-rental_stress_state",
    "rental_market-rental_stress_trend",
    "rental_market-rental_afford_index",
    "rental_market-rental_costs_demo"
  )
  missing_ui <- required_ui[
    !vapply(required_ui, grepl, logical(1), module_ui, fixed = TRUE)
  ]
  check(length(missing_ui) == 0,
        paste("Rental Market UI missing mobile classes/outputs:",
              paste(missing_ui, collapse = "; ")))
}

if (file.exists(module_path)) {
  module_text <- paste(readLines(module_path, warn = FALSE), collapse = "\n")
  required_module_text <- c(
    "rental_plot_margins <- list(",
    "state = list(l = 82, r = 20, b = 76, t = 38)",
    "trend = list(l = 86, r = 20, b = 92, t = 32)",
    "index = list(l = 74, r = 20, b = 72, t = 30)",
    "costs = list(l = 150, r = 20, b = 78, t = 30)",
    'div(class = "rental-market-page"',
    'div(class = "rental-market-grid"',
    'div(class = "chart-square rental-market-chart rental-market-chart-square"',
    'div(class = "chart-wide rental-market-chart rental-market-chart-wide"',
    "margin = rental_plot_margins$state",
    "margin = rental_plot_margins$trend",
    "margin = rental_plot_margins$index",
    "margin = rental_plot_margins$costs",
    "dashboard_ggplotly",
    "bindCache(input$rental_year, input$rental_states, is_dark())",
    "bindCache(input$rental_states, is_dark())",
    "bindCache(is_dark())",
    "bindCache(input$rental_cost_breakdown, is_dark())"
  )
  missing_module_text <- required_module_text[
    !vapply(required_module_text, grepl, logical(1), module_text, fixed = TRUE)
  ]
  check(length(missing_module_text) == 0,
        paste("R/rental_market_module.R missing mobile contracts:",
              paste(missing_module_text, collapse = "; ")))
}

if (file.exists(app_path)) {
  app_text <- paste(readLines(app_path, warn = FALSE), collapse = "\n")
  required_css <- c(
    "max-height: 52vh",
    "padding-top: 0.48rem",
    ".rental-market-page",
    ".rental-market-grid",
    ".rental-market-chart",
    "grid-template-columns: minmax(0, 1fr) !important",
    "grid-auto-rows: auto !important",
    "height: 360px",
    "height: 390px"
  )
  missing_css <- required_css[
    !vapply(required_css, grepl, logical(1), app_text, fixed = TRUE)
  ]
  check(length(missing_css) == 0,
        paste("app.R missing mobile Rental Market CSS:",
              paste(missing_css, collapse = "; ")))

  required_nav_contract <- c(
    "bootstrap.Collapse",
    ".navbar-collapse.show",
    "main_nav",
    "window.innerWidth >= 992"
  )
  missing_nav <- required_nav_contract[
    !vapply(required_nav_contract, grepl, logical(1), app_text, fixed = TRUE)
  ]
  check(length(missing_nav) == 0,
        paste("app.R missing mobile nav collapse contract:",
              paste(missing_nav, collapse = "; ")))
}

if (file.exists(description_path)) {
  desc_text <- paste(readLines(description_path, warn = FALSE), collapse = "\n")
  forbidden_direct_dependencies <- c(
    "shinyjs",
    "htmltools",
    "waiter",
    "shinycssloaders",
    "DT",
    "bsicons"
  )
  unexpected <- forbidden_direct_dependencies[
    vapply(forbidden_direct_dependencies, function(pkg) {
      grepl(paste0("(^|\\n)\\s*", pkg, "\\s*,?\\s*($|\\n)"),
            desc_text, perl = TRUE)
    }, logical(1))
  ]
  check(length(unexpected) == 0,
        paste("DESCRIPTION must not add new direct dependencies:",
              paste(unexpected, collapse = ", ")))
}

if (file.exists(readme_path)) {
  readme_text <- paste(readLines(readme_path, warn = FALSE), collapse = "\n")
  check(grepl("Rscript tests/test_rental_market_mobile_contracts.R",
              readme_text, fixed = TRUE),
        "README.md must document the Rental Market mobile contract test")
}

if (length(failures) > 0) {
  stop(
    paste(c("Rental Market mobile contract checks failed:",
            paste0("- ", failures)),
          collapse = "\n"),
    call. = FALSE
  )
}

cat("Rental Market mobile contract checks passed.\n")

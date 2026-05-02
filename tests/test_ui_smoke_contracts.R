repo_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
failures <- character()

check <- function(condition, message) {
  if (!isTRUE(condition)) {
    failures <<- c(failures, message)
  }
}

read_text <- function(path) {
  paste(readLines(path, warn = FALSE), collapse = "\n")
}

app_path <- file.path(repo_root, "app.R")
readme_path <- file.path(repo_root, "README.md")
docs_path <- file.path(repo_root, "docs", "ui_smoke_checklist.md")
module_paths <- list.files(file.path(repo_root, "R"),
                           pattern = "_module[.]R$",
                           full.names = TRUE)

check(file.exists(app_path), "app.R does not exist")
check(file.exists(readme_path), "README.md does not exist")

app_text <- if (file.exists(app_path)) read_text(app_path) else ""
module_text <- vapply(module_paths, read_text, character(1))
all_ui_text <- paste(c(app_text, module_text), collapse = "\n")

expected_modules <- data.frame(
  label = c(
    "Overview",
    "Price Trends",
    "Affordability",
    "Geographic Affordability",
    "Market Context",
    "Housing Supply",
    "Rental Market",
    "Methodology"
  ),
  file = c(
    "overview_module.R",
    "price_trends_module.R",
    "affordability_module.R",
    "geographic_affordability_module.R",
    "market_context_module.R",
    "housing_supply_module.R",
    "rental_market_module.R",
    "methodology_module.R"
  ),
  ui = c(
    "overviewPageUI",
    "priceTrendsPageUI",
    "affordabilityPageUI",
    "geographicAffordabilityPageUI",
    "marketContextPageUI",
    "housingSupplyPageUI",
    "rentalMarketPageUI",
    "methodologyPageUI"
  ),
  server = c(
    "overviewPageServer",
    "priceTrendsPageServer",
    "affordabilityPageServer",
    "geographicAffordabilityPageServer",
    "marketContextPageServer",
    "housingSupplyPageServer",
    "rentalMarketPageServer",
    "methodologyPageServer"
  ),
  id = c(
    "overview",
    "price_trends",
    "affordability",
    "geographic_affordability",
    "market_context",
    "housing_supply",
    "rental_market",
    "methodology"
  ),
  stringsAsFactors = FALSE
)

for (i in seq_len(nrow(expected_modules))) {
  module_file <- expected_modules$file[i]
  ui_fn <- expected_modules$ui[i]
  server_fn <- expected_modules$server[i]
  id <- expected_modules$id[i]
  label <- expected_modules$label[i]

  check(
    grepl(sprintf('source\\(project_path\\("R", "%s"\\), local = TRUE\\)',
                  module_file),
          app_text, perl = TRUE),
    sprintf("app.R must source R/%s", module_file)
  )
  check(
    grepl(sprintf('%s\\s*\\(\\s*"%s"\\s*\\)', ui_fn, id),
          app_text, perl = TRUE),
    sprintf("app.R must mount %s(\"%s\")", ui_fn, id)
  )
  if (identical(server_fn, "methodologyPageServer")) {
    check(
      grepl(sprintf('%s\\s*\\(\\s*"%s"\\s*\\)', server_fn, id),
            app_text, perl = TRUE),
      sprintf("app.R must call %s(\"%s\")", server_fn, id)
    )
  } else {
    check(
      grepl(sprintf('%s\\s*\\(\\s*"%s"\\s*,\\s*is_dark\\s*=\\s*is_dark\\s*\\)',
                    server_fn, id),
            app_text, perl = TRUE),
      sprintf("app.R must call %s(\"%s\", is_dark = is_dark)",
              server_fn, id)
    )
  }
  check(
    grepl(sprintf('nav_panel\\(\\s*"%s"', label), all_ui_text, perl = TRUE),
    sprintf("UI must include top-level nav_panel(\"%s\")", label)
  )
}

theme_contract <- c(
  'page_navbar(',
  'id = "main_nav"',
  'input_dark_mode(id = "theme_mode")',
  'is_dark <- reactive',
  'input$theme_mode',
  "document.addEventListener('click'",
  'bootstrap.Collapse',
  "$(collapseEl).collapse('hide')",
  'forceHideNavbar',
  "getComputedStyle(toggle)",
  '.navbar-collapse.show',
  'window.innerWidth >= 992'
)
missing_theme <- theme_contract[
  !vapply(theme_contract, grepl, logical(1), app_text, fixed = TRUE)
]
check(length(missing_theme) == 0,
      paste("Theme/mobile nav contract missing:",
            paste(missing_theme, collapse = ", ")))

input_ids <- c(
  "overview_price_dates",
  "overview_price_transform",
  "price_cities",
  "price_dwelling",
  "price_dates",
  "price_transform",
  "rent_cpi_cities",
  "rent_cpi_dates",
  "rent_cpi_datatype",
  "afford_indices",
  "afford_dates",
  "serviceability_buffer",
  "serviceability_deposit_pct",
  "serviceability_term",
  "calc_price",
  "calc_income",
  "calc_rate",
  "calc_assessment_buffer",
  "calc_deposit_pct",
  "calc_term",
  "calc_savings_rate",
  "calc_annual_expenses",
  "calc_monthly_debt",
  "stress_breakdown",
  "stress_population",
  "burden_breakdown",
  "burden_stat",
  "geo_states",
  "geo_state_metric",
  "geo_state_tenure",
  "geo_lower_metric",
  "geo_lower_tenure",
  "geo_gcc_geographies",
  "geo_gcc_metric",
  "geo_gcc_tenure",
  "context_dates",
  "supply_dates",
  "supply_states",
  "supply_building_type",
  "supply_sector",
  "rental_year",
  "rental_states",
  "rental_cost_breakdown"
)

missing_inputs <- input_ids[
  !vapply(input_ids, function(id) {
    grepl(sprintf('ns\\(\\s*"%s"\\s*\\)', id), all_ui_text, perl = TRUE)
  }, logical(1))
]
check(length(missing_inputs) == 0,
      paste("Expected UI input IDs missing:",
            paste(missing_inputs, collapse = ", ")))

plotly_output_ids <- c(
  "overview_median_prices",
  "overview_afford_change",
  "price_chart",
  "rent_cpi_chart",
  "afford_indices_chart",
  "afford_serviceability",
  "stress_chart",
  "burden_heatmap",
  "geo_state_trend",
  "geo_state_latest",
  "geo_lower_income",
  "geo_gcc_comparison",
  "context_rates",
  "context_labour",
  "context_pop",
  "supply_approvals",
  "supply_cpi_construction",
  "rental_stress_state",
  "rental_stress_trend",
  "rental_afford_index",
  "rental_costs_demo"
)

missing_plotly_ui <- plotly_output_ids[
  !vapply(plotly_output_ids, function(id) {
    grepl(sprintf('plotlyOutput\\(ns\\("%s"\\)', id),
          all_ui_text, perl = TRUE)
  }, logical(1))
]
missing_plotly_server <- plotly_output_ids[
  !vapply(plotly_output_ids, function(id) {
    grepl(sprintf('output\\$%s\\s*<-\\s*renderPlotly\\s*\\(', id),
          all_ui_text, perl = TRUE)
  }, logical(1))
]
check(length(missing_plotly_ui) == 0,
      paste("Expected plotlyOutput IDs missing:",
            paste(missing_plotly_ui, collapse = ", ")))
check(length(missing_plotly_server) == 0,
      paste("Expected renderPlotly outputs missing:",
            paste(missing_plotly_server, collapse = ", ")))

other_output_ids <- c(
  "indicator_table",
  "provenance_download",
  "calc_repayment",
  "calc_ratio",
  "calc_assessed_ratio",
  "calc_years",
  "calc_lvr",
  "calc_total_interest",
  "calc_deposit_amt"
)
missing_other_outputs <- other_output_ids[
  !vapply(other_output_ids, function(id) {
    grepl(sprintf('ns\\(\\s*"%s"\\s*\\)', id), all_ui_text, perl = TRUE) &&
      grepl(sprintf('output\\$%s\\s*<-', id), all_ui_text, perl = TRUE)
  }, logical(1))
]
check(length(missing_other_outputs) == 0,
      paste("Expected non-Plotly outputs missing:",
            paste(missing_other_outputs, collapse = ", ")))

required_nested_tabs <- c(
  "Dwelling Price Index",
  "Rent CPI",
  "Indices",
  "Calculator",
  "Housing Stress",
  "Cost Burden"
)
missing_tabs <- required_nested_tabs[
  !vapply(required_nested_tabs, function(tab) {
    grepl(sprintf('nav_panel\\(\\s*"%s"', tab), all_ui_text, perl = TRUE)
  }, logical(1))
]
check(length(missing_tabs) == 0,
      paste("Expected nested tabs missing:",
            paste(missing_tabs, collapse = ", ")))

required_cache_contracts <- c(
  "bindCache(input$rental_year, input$rental_states, is_dark())",
  "bindCache(input$rental_states, is_dark())",
  "bindCache(input$rental_cost_breakdown, is_dark())",
  "bindCache(input$supply_dates, input$supply_states, input$supply_building_type, input$supply_sector, is_dark())",
  "bindCache(input$afford_indices, input$afford_dates, input$serviceability_deposit_pct, input$serviceability_term, input$serviceability_buffer, is_dark())",
  "bindCache(input$geo_states, input$geo_lower_metric, input$geo_lower_tenure, is_dark())"
)
missing_cache_contracts <- required_cache_contracts[
  !vapply(required_cache_contracts, grepl, logical(1),
          all_ui_text, fixed = TRUE)
]
check(length(missing_cache_contracts) == 0,
      paste("Critical control cache contracts missing:",
            paste(missing_cache_contracts, collapse = "; ")))

required_runtime_contracts <- c(
  "normalise_rental_states(input$rental_states",
  "tile_label = paste0(sprintf(\"%.0f\", value), reliability_marker)",
  "rental_plot_margins$trend",
  "dashboard_ggplotly(",
  "downloadHandler(",
  "methodology_provenance_report("
)
missing_runtime_contracts <- required_runtime_contracts[
  !vapply(required_runtime_contracts, grepl, logical(1),
          all_ui_text, fixed = TRUE)
]
check(length(missing_runtime_contracts) == 0,
      paste("Runtime smoke contracts missing:",
            paste(missing_runtime_contracts, collapse = ", ")))

check(file.exists(docs_path), "docs/ui_smoke_checklist.md does not exist")
if (file.exists(docs_path)) {
  docs_text <- read_text(docs_path)
  required_docs_text <- c(
    "Codex in-app browser",
    "http://127.0.0.1:3971/",
    "NHHA heatmap tiles show values",
    "Rental Market year/state/breakdown controls",
    "mobile navbar collapses",
    "no Shiny binding errors"
  )
  missing_docs <- required_docs_text[
    !vapply(required_docs_text, grepl, logical(1),
            docs_text, fixed = TRUE)
  ]
  check(length(missing_docs) == 0,
        paste("UI smoke checklist missing required text:",
              paste(missing_docs, collapse = ", ")))
}

if (file.exists(readme_path)) {
  readme_text <- read_text(readme_path)
  readme_contract <- c(
    "Rscript tests/test_ui_smoke_contracts.R",
    "docs/ui_smoke_checklist.md",
    "no new browser-testing dependencies"
  )
  missing_readme <- readme_contract[
    !vapply(readme_contract, grepl, logical(1),
            readme_text, fixed = TRUE)
  ]
  check(length(missing_readme) == 0,
        paste("README.md missing UI smoke documentation:",
              paste(missing_readme, collapse = ", ")))
}

if (length(failures) > 0) {
  stop(
    paste(c("UI smoke contract checks failed:", paste0("- ", failures)),
          collapse = "\n"),
    call. = FALSE
  )
}

cat("UI smoke contract checks passed.\n")

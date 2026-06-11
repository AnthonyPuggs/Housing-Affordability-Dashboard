# Runs standalone via `Rscript tests/test_scenario_sensitivity.R` (repo root) and under
# testthat::test_dir("tests"); see tests/helper-contracts.R.
if (!exists("contracts_harness_loaded", mode = "function")) {
  source(file.path(if (basename(getwd()) == "tests") "." else "tests",
                   "helper-contracts.R"))
}

test_that("scenario_sensitivity contracts", {
  repo_root <- repo_root_path()


  scenario_path <- file.path(repo_root, "R", "market_entry_scenarios.R")
  module_path <- file.path(repo_root, "R", "affordability_module.R")
  chart_path <- file.path(repo_root, "R", "chart_builders.R")

  check(file.exists(scenario_path), "R/market_entry_scenarios.R does not exist")
  check(file.exists(module_path), "R/affordability_module.R does not exist")
  check(file.exists(chart_path), "R/chart_builders.R does not exist")

  if (file.exists(scenario_path)) {
    source(scenario_path, local = TRUE)
    required_functions <- c(
      "market_entry_scenario_presets",
      "market_entry_sensitivity_grid"
    )
    missing_functions <- required_functions[
      !exists_in(required_functions, environment())
    ]
    check(length(missing_functions) == 0,
          paste("Missing scenario sensitivity helpers:",
                paste(missing_functions, collapse = ", ")))

    if (exists("market_entry_scenario_presets", mode = "function")) {
      presets <- market_entry_scenario_presets()
      required_preset_cols <- c(
        "preset_id",
        "label",
        "dwelling_price",
        "gross_annual_income",
        "annual_rate_pct",
        "deposit_pct",
        "term_years",
        "savings_rate_pct",
        "assessment_buffer_pp",
        "annual_non_housing_expenses",
        "monthly_other_debt"
      )
      missing_preset_cols <- setdiff(required_preset_cols, names(presets))
      check(length(missing_preset_cols) == 0,
            paste("market_entry_scenario_presets() missing columns:",
                  paste(missing_preset_cols, collapse = ", ")))
      check(all(c("first_home_buyer", "mortgage_stress", "high_lvr_buyer") %in%
                  presets$preset_id),
            "Ownership scenario presets must include first-home buyer, mortgage-stress and high-LVR cases")
      check(!("renter_entry" %in% presets$preset_id),
            "Ownership scenario presets must not include renter_entry")
    }

    if (exists("market_entry_sensitivity_grid", mode = "function")) {
      grid <- market_entry_sensitivity_grid(
        dwelling_price = 800000,
        gross_annual_income = 120000,
        annual_rate_pct = 6,
        deposit_pct = 20,
        term_years = 30,
        savings_rate_pct = 15,
        assessment_buffer_pp = 3,
        annual_non_housing_expenses = 30000,
        monthly_other_debt = 0
      )
      required_grid_cols <- c(
        "sensitivity",
        "input_value",
        "assessed_repayment_to_gross_income_pct",
        "expense_adjusted_repayment_ratio_pct",
        "years_to_save_deposit"
      )
      missing_grid_cols <- setdiff(required_grid_cols, names(grid))
      check(length(missing_grid_cols) == 0,
            paste("market_entry_sensitivity_grid() missing columns:",
                  paste(missing_grid_cols, collapse = ", ")))

      by_rate <- grid[grid$sensitivity == "interest_rate", ]
      by_price <- grid[grid$sensitivity == "dwelling_price", ]
      by_deposit <- grid[grid$sensitivity == "deposit_share", ]
      by_expenses <- grid[grid$sensitivity == "non_housing_expenses", ]
      check(nrow(by_rate) >= 3 && all(diff(by_rate$assessed_repayment_to_gross_income_pct) > 0),
            "Higher interest rates must worsen assessed serviceability")
      check(nrow(by_price) >= 3 && all(diff(by_price$assessed_repayment_to_gross_income_pct) > 0),
            "Higher dwelling prices must worsen assessed serviceability")
      check(nrow(by_deposit) >= 3 && all(diff(by_deposit$assessed_repayment_to_gross_income_pct) < 0),
            "Higher deposit shares must improve assessed serviceability")
      check(nrow(by_expenses) >= 3 && all(diff(by_expenses$expense_adjusted_repayment_ratio_pct) > 0),
            "Higher non-housing expenses must worsen expense-adjusted serviceability")
    }
  }

  if (file.exists(chart_path)) {
    chart_text <- paste(readLines(chart_path, warn = FALSE), collapse = "\n")
    check(grepl("build_market_entry_sensitivity_plot", chart_text, fixed = TRUE),
          "R/chart_builders.R must define build_market_entry_sensitivity_plot()")
  }

  if (file.exists(module_path)) {
    module_text <- paste(readLines(module_path, warn = FALSE), collapse = "\n")
    required_module_text <- c(
      'selectInput(ns("calc_preset")',
      "First-home buyer",
      "Mortgage-stress",
      "High-LVR buyer",
      "Rental entry",
      'selectInput(ns("calc_pathway")',
      'plotlyOutput(ns("calc_sensitivity")',
      "output$calc_sensitivity <- renderPlotly",
      "market_entry_sensitivity_grid(",
      "build_market_entry_sensitivity_plot("
    )
    missing_module_text <- required_module_text[
      !vapply(required_module_text, grepl, logical(1), module_text, fixed = TRUE)
    ]
    check(length(missing_module_text) == 0,
          paste("Affordability calculator missing sensitivity UI/server text:",
                paste(missing_module_text, collapse = "; ")))
  }
})
repo_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
scenario_path <- file.path(repo_root, "R", "market_entry_scenarios.R")
failures <- character()

check <- function(condition, message) {
  if (!isTRUE(condition)) {
    failures <<- c(failures, message)
  }
}

near <- function(actual, expected, tolerance = 1e-6) {
  isTRUE(abs(actual - expected) <= tolerance)
}

check(file.exists(scenario_path), "R/market_entry_scenarios.R does not exist")

if (file.exists(scenario_path)) {
  source(scenario_path)

  check(exists("renter_entry_scenario", mode = "function"),
        "renter_entry_scenario() must be defined")
  check(exists("renter_entry_scenario_presets", mode = "function"),
        "renter_entry_scenario_presets() must be defined")

  ownership_presets <- market_entry_scenario_presets()
  check(!("renter_entry" %in% ownership_presets$preset_id),
        "Ownership presets must not include renter_entry")
  check("high_lvr_buyer" %in% ownership_presets$preset_id,
        "Ownership presets must include high_lvr_buyer as the former renter-entry replacement")

  if (exists("renter_entry_scenario", mode = "function")) {
    scenario <- renter_entry_scenario(
      weekly_rent = 620,
      gross_annual_income = 95000,
      bond_weeks = 4,
      upfront_moving_costs = 3000,
      savings_rate_pct = 10,
      annual_non_housing_expenses = 28000
    )

    required_cols <- c(
      "weekly_rent",
      "annual_rent",
      "gross_annual_income",
      "rent_to_gross_income_pct",
      "expense_adjusted_rent_ratio_pct",
      "bond_amount",
      "upfront_cash_required",
      "years_to_save_upfront",
      "weeks_to_save_upfront"
    )
    missing_cols <- setdiff(required_cols, names(scenario))
    check(length(missing_cols) == 0,
          paste("renter_entry_scenario() missing columns:",
                paste(missing_cols, collapse = ", ")))
    check(near(scenario$annual_rent, 620 * 52),
          "Annual rent must equal weekly rent multiplied by 52")
    check(near(scenario$bond_amount, 620 * 4),
          "Bond amount must equal weekly rent multiplied by bond weeks")
    check(near(scenario$upfront_cash_required, 620 * 4 + 3000),
          "Upfront cash required must equal bond plus moving/setup costs")
    check(near(scenario$rent_to_gross_income_pct,
               620 * 52 / 95000 * 100),
          "Rent-to-gross-income ratio formula is incorrect")
    check(scenario$expense_adjusted_rent_ratio_pct >
            scenario$rent_to_gross_income_pct,
          "Expense-adjusted rent ratio must exceed gross-income rent ratio")
  }

  if (exists("renter_entry_scenario_presets", mode = "function")) {
    renter_presets <- renter_entry_scenario_presets()
    required_preset_cols <- c(
      "preset_id",
      "label",
      "weekly_rent",
      "gross_annual_income",
      "bond_weeks",
      "upfront_moving_costs",
      "savings_rate_pct",
      "annual_non_housing_expenses"
    )
    missing_preset_cols <- setdiff(required_preset_cols, names(renter_presets))
    check(length(missing_preset_cols) == 0,
          paste("renter_entry_scenario_presets() missing columns:",
                paste(missing_preset_cols, collapse = ", ")))
    check("median_renter_entry" %in% renter_presets$preset_id,
          "Renter presets must include median_renter_entry")
  }
}

if (length(failures) > 0) {
  stop(
    paste(c("Renter-entry scenario checks failed:",
            paste0("- ", failures)), collapse = "\n"),
    call. = FALSE
  )
}

cat("Renter-entry scenario checks passed.\n")

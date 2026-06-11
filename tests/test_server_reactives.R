# Runs standalone via `Rscript tests/test_server_reactives.R` (repo root) and under
# testthat::test_dir("tests"); see tests/helper-contracts.R.
#
# shiny::testServer() coverage for the two reactive surfaces the static text
# contracts cannot exercise: the market-entry calculator (both pathways) and
# the overview affordability score card (review TEST-05).
if (!exists("contracts_harness_loaded", mode = "function")) {
  source(file.path(if (basename(getwd()) == "tests") "." else "tests",
                   "helper-contracts.R"))
}

test_that("market-entry calculator reactives compute hand-checked values", {
  repo_root <- repo_root_path()

  suppressPackageStartupMessages({
    library(shiny)
    library(bslib)
    library(plotly)
    library(dplyr)
    library(stringr)
    library(scales)
  })
  source(file.path(repo_root, "R", "indicator_registry.R"), local = TRUE)
  source(file.path(repo_root, "R", "visual_semantics.R"), local = TRUE)
  source(file.path(repo_root, "R", "dashboard_formatting.R"), local = TRUE)
  source(file.path(repo_root, "R", "app_ui_helpers.R"), local = TRUE)
  source(file.path(repo_root, "R", "market_entry_scenarios.R"), local = TRUE)

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

  source(file.path(repo_root, "R", "affordability_module.R"), local = TRUE)

  testServer(affordabilityPageServer, args = list(is_dark = reactive(FALSE)), {
    session$setInputs(
      calc_price = 800000,
      calc_income = 120000,
      calc_rate = 6,
      calc_deposit_pct = 20,
      calc_term = 30,
      calc_savings_rate = 15,
      calc_assessment_buffer = 3,
      calc_annual_expenses = 30000,
      calc_monthly_debt = 0,
      rent_weekly_input = 620,
      rent_income = 95000,
      rent_bond_weeks = 4,
      rent_upfront_costs = 3000,
      rent_savings_rate = 15,
      rent_annual_expenses = 28000
    )

    # Hand-computed 30-year P&I on an $640k loan at 6%: $3,837.12/month.
    expect_equal(output$calc_repayment, fmt_dollar(3837.12), info =
                   "Monthly repayment must match the hand-computed annuity")

    vals <- calc_vals()
    expect_true(abs(vals$monthly_nominal_repayment - 3837.12) < 0.05,
                info = "calc_vals() must compute the hand-checked annuity payment")
    expect_true(abs(vals$deposit - 160000) < 1e-6,
                info = "Deposit must be 20% of the dwelling price")
    # Repayment share of gross income: 3837.12 * 12 / 120000 = 38.37%.
    expect_true(abs(vals$nominal_repayment_to_gross_income_pct - 38.3712) < 0.01,
                info = "Repayment-to-income share must match the hand-checked ratio")

    # Renter pathway: 620 * 52 / 95000 = 33.94% of gross income.
    rent <- rent_vals()
    expect_true(abs(rent$rent_to_gross_income_pct - 620 * 52 / 95000 * 100) < 1e-6,
                info = "Rent-to-income share must match the hand-checked ratio")
    expect_equal(output$rent_weekly, fmt_dollar(620), info =
                   "Weekly rent output must echo the input")
  })
})

test_that("overview score card reactives select and format the score", {
  repo_root <- repo_root_path()

  suppressPackageStartupMessages({
    library(shiny)
    library(bslib)
    library(plotly)
    library(dplyr)
    library(stringr)
    library(scales)
  })
  source(file.path(repo_root, "R", "indicator_registry.R"), local = TRUE)
  source(file.path(repo_root, "R", "visual_semantics.R"), local = TRUE)
  source(file.path(repo_root, "R", "dashboard_formatting.R"), local = TRUE)
  source(file.path(repo_root, "R", "app_ui_helpers.R"), local = TRUE)
  source(file.path(repo_root, "R", "national_affordability_score.R"),
         local = TRUE)

  median_prices_combined <- data.frame(
    date = as.Date(c("2010-01-01", "2024-01-01")),
    value = c(500, 900),
    city = c("Sydney", "National Avg"),
    stringsAsFactors = FALSE
  )
  national_affordability_score_ts <- data.frame(
    date = as.Date(c("2024-10-01", "2025-04-01", "2025-10-01")),
    score = c(30.5, 25.0, 22.6),
    stringsAsFactors = FALSE
  )

  source(file.path(repo_root, "R", "overview_module.R"), local = TRUE)

  testServer(overviewPageServer, args = list(is_dark = reactive(FALSE)), {
    row <- selected_score_row()
    expect_true(nrow(row) == 1 && row$date == as.Date("2025-10-01"),
                info = "Score card must select the latest score date by default")
    expect_equal(output$vb_afford_score, paste0(fmt_index(22.6), " / 100"),
                 info = "Headline score must format the latest fixture score")
    expect_true(grepl("Latest:", output$vb_afford_score_date, fixed = TRUE),
                info = "Score as-at line must mark the latest date")
    expect_true(grepl("reference window", output$vb_afford_score_basis,
                      fixed = TRUE),
                info = "Score basis must state the frozen reference window")
  })
})

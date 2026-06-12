# Runs standalone via `Rscript tests/test_market_entry_scenarios.R` (repo root) and under
# testthat::test_dir("tests"); see tests/helper-contracts.R.
if (!exists("contracts_harness_loaded", mode = "function")) {
  source(file.path(if (basename(getwd()) == "tests") "." else "tests",
                   "helper-contracts.R"))
}

test_that("market_entry_scenarios contracts", {
  repo_root <- repo_root_path()
  scenario_path <- file.path(repo_root, "R", "market_entry_scenarios.R")


  near <- function(actual, expected, tolerance = 1e-6) {
    isTRUE(abs(actual - expected) <= tolerance)
  }

  check(file.exists(scenario_path), "R/market_entry_scenarios.R does not exist")

  if (file.exists(scenario_path)) {
    parsed <- tryCatch({
      parse(scenario_path)
      TRUE
    }, error = function(e) conditionMessage(e))
    check(identical(parsed, TRUE),
          paste("R/market_entry_scenarios.R does not parse:", parsed))

    source(scenario_path, local = TRUE)

    required_functions <- c(
      "mortgage_monthly_payment",
      "mortgage_max_loan",
      "market_entry_scenario",
      "borrowing_capacity_scenario",
      "renter_entry_scenario",
      "market_entry_serviceability_series"
    )
    missing_functions <- required_functions[
      !exists_in(required_functions, environment())
    ]
    check(length(missing_functions) == 0,
          paste("Missing market-entry scenario functions:",
                paste(missing_functions, collapse = ", ")))

    if (exists("mortgage_monthly_payment", mode = "function")) {
      zero_rate <- mortgage_monthly_payment(
        loan_amount = 120000,
        annual_rate_pct = 0,
        term_years = 30
      )
      check(near(zero_rate, 120000 / (30 * 12)),
            "Zero-rate repayment must equal principal divided by payments")

      positive_rate <- mortgage_monthly_payment(
        loan_amount = 100000,
        annual_rate_pct = 6,
        term_years = 30
      )
      check(near(round(positive_rate, 2), 599.55, tolerance = 0.01),
            "Positive-rate repayment formula is incorrect")
    }

    if (exists("market_entry_scenario", mode = "function")) {
      base <- market_entry_scenario(
        dwelling_price = 800000,
        gross_annual_income = 120000,
        annual_rate_pct = 6,
        deposit_pct = 20,
        term_years = 30,
        savings_rate_pct = 15,
        assessment_buffer_pp = 3,
        annual_non_housing_expenses = 30000,
        monthly_other_debt = 500
      )

      required_columns <- c(
        "deposit",
        "loan_amount",
        "lvr_pct",
        "monthly_nominal_repayment",
        "monthly_assessed_repayment",
        "nominal_repayment_to_gross_income_pct",
        "assessed_repayment_to_gross_income_pct",
        "expense_adjusted_repayment_ratio_pct",
        "years_to_save_deposit",
        "total_nominal_interest"
      )
      missing_columns <- setdiff(required_columns, names(base))
      check(length(missing_columns) == 0,
            paste("market_entry_scenario() missing columns:",
                  paste(missing_columns, collapse = ", ")))

      check(nrow(base) == 1, "market_entry_scenario() must return one row")
      check(near(base$deposit, 160000),
            "Deposit must equal price multiplied by deposit percentage")
      check(near(base$loan_amount, 640000),
            "Loan amount must equal dwelling price less deposit")
      check(near(base$lvr_pct, 80),
            "LVR must equal loan divided by dwelling price")
      check(base$monthly_assessed_repayment > base$monthly_nominal_repayment,
            "Assessment buffer must increase assessed repayment")
      check(base$assessed_repayment_to_gross_income_pct >
              base$nominal_repayment_to_gross_income_pct,
            "Assessment buffer must increase assessed repayment ratio")
      check(base$expense_adjusted_repayment_ratio_pct >
              base$assessed_repayment_to_gross_income_pct,
            "Expenses and other debt must raise the expense-adjusted ratio")
      check(near(base$years_to_save_deposit,
                 160000 / (120000 * 0.15)),
            "Years to save deposit formula is incorrect")

      invalid_cases <- list(
        function() market_entry_scenario(
          dwelling_price = 0,
          gross_annual_income = 120000,
          annual_rate_pct = 6
        ),
        function() market_entry_scenario(
          dwelling_price = 800000,
          gross_annual_income = -1,
          annual_rate_pct = 6
        ),
        function() market_entry_scenario(
          dwelling_price = 800000,
          gross_annual_income = 120000,
          annual_rate_pct = 6,
          term_years = 0
        ),
        function() market_entry_scenario(
          dwelling_price = 800000,
          gross_annual_income = 120000,
          annual_rate_pct = 6,
          annual_non_housing_expenses = 120000
        )
      )
      invalid_messages <- vapply(invalid_cases, function(case) {
        tryCatch({
          case()
          ""
        }, error = function(e) conditionMessage(e))
      }, character(1))
      check(all(nzchar(invalid_messages)),
            "Invalid scenario inputs must fail clearly")
    }

    if (exists("mortgage_max_loan", mode = "function") &&
        exists("mortgage_monthly_payment", mode = "function")) {
      # Round-trip: mortgage_max_loan is the exact inverse of the payment annuity.
      round_trip <- mortgage_max_loan(
        mortgage_monthly_payment(640000, 6, 30), 6, 30
      )
      check(near(round_trip, 640000, tolerance = 1e-4),
            "mortgage_max_loan() must invert mortgage_monthly_payment() exactly")
      check(near(mortgage_max_loan(3000, 0, 30), 3000 * 360),
            "Zero-rate max loan must equal payment times number of payments")
    }

    if (exists("borrowing_capacity_scenario", mode = "function")) {
      cap <- borrowing_capacity_scenario(
        gross_annual_income = 120000,
        annual_rate_pct = 6,
        assessment_buffer_pp = 3,
        deposit_pct = 20,
        term_years = 30,
        target_repayment_ratio_pct = 30,
        monthly_other_debt = 0
      )
      check(nrow(cap) == 1, "borrowing_capacity_scenario() must return one row")
      check(near(cap$assessment_rate_pct, 9),
            "Assessed rate must be the nominal rate plus the buffer")
      check(near(cap$max_monthly_repayment, 0.30 * 120000 / 12),
            "Max repayment must be the threshold share of monthly gross income")
      check(near(cap$max_loan, mortgage_max_loan(3000, 9, 30), tolerance = 1e-4),
            "Max loan must be the assessed-rate annuity inverse of the cap")
      check(near(cap$implied_max_price, cap$max_loan / 0.8, tolerance = 1e-4),
            "Implied max price must gross up the loan by the deposit share")
      check(near(cap$required_deposit, cap$implied_max_price - cap$max_loan,
                 tolerance = 1e-4),
            "Required deposit must be price minus loan")

      # Monotonicity.
      higher_rate <- borrowing_capacity_scenario(120000, 8)
      check(higher_rate$max_loan < cap$max_loan,
            "A higher interest rate must lower borrowing capacity")
      higher_threshold <- borrowing_capacity_scenario(
        120000, 6, target_repayment_ratio_pct = 40
      )
      check(higher_threshold$max_loan > cap$max_loan,
            "A higher repayment-share threshold must raise borrowing capacity")
      with_debt <- borrowing_capacity_scenario(120000, 6, monthly_other_debt = 1000)
      check(with_debt$max_loan < cap$max_loan,
            "Existing debt repayments must lower borrowing capacity")

      # Edge: other debt exhausting the cap yields zero capacity, not an error.
      exhausted <- borrowing_capacity_scenario(120000, 6, monthly_other_debt = 1e6)
      check(near(exhausted$max_loan, 0) &&
              near(exhausted$implied_max_price, 0) &&
              near(exhausted$required_deposit, 0),
            "Capacity must clamp to zero when other debt exhausts the cap")

      invalid_capacity_cases <- list(
        function() borrowing_capacity_scenario(gross_annual_income = 0,
                                               annual_rate_pct = 6),
        function() borrowing_capacity_scenario(120000, 6, deposit_pct = 100),
        function() borrowing_capacity_scenario(120000, 6, term_years = 0),
        function() borrowing_capacity_scenario(120000, 6,
                                               target_repayment_ratio_pct = 0)
      )
      invalid_capacity_messages <- vapply(invalid_capacity_cases, function(case) {
        tryCatch({
          case()
          ""
        }, error = function(e) conditionMessage(e))
      }, character(1))
      check(all(nzchar(invalid_capacity_messages)),
            "Invalid borrowing-capacity inputs must fail clearly")
    }

    if (exists("market_entry_serviceability_series", mode = "function")) {
      price_ts <- data.frame(
        date = as.Date(c("2024-01-01", "2024-04-01")),
        price_k = c(800, 820)
      )
      income_ts <- data.frame(
        date = as.Date(c("2024-01-01", "2024-04-01")),
        awe = c(2300, 2320)
      )
      rate_ts <- data.frame(
        date = as.Date(c("2024-01-01", "2024-04-01")),
        rate = c(6, 6.25)
      )
      serviceability <- market_entry_serviceability_series(
        price_ts = price_ts,
        income_ts = income_ts,
        rate_ts = rate_ts,
        assessment_buffer_pp = 3
      )
      required_series_columns <- c(
        "date",
        "scenario",
        "serviceability_pct",
        "assessment_buffer_pp"
      )
      missing_series_columns <- setdiff(required_series_columns,
                                        names(serviceability))
      check(length(missing_series_columns) == 0,
            paste("market_entry_serviceability_series() missing columns:",
                  paste(missing_series_columns, collapse = ", ")))
      check(all(c("Nominal rate", "Assessed rate") %in% serviceability$scenario),
            "Serviceability series must include nominal and assessed scenarios")
      assessed <- serviceability[
        serviceability$scenario == "Assessed rate",
        "serviceability_pct"
      ]
      nominal <- serviceability[
        serviceability$scenario == "Nominal rate",
        "serviceability_pct"
      ]
      check(all(assessed > nominal),
            "Assessed serviceability series must exceed nominal series")
    }
  }
})
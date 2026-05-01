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
  parsed <- tryCatch({
    parse(scenario_path)
    TRUE
  }, error = function(e) conditionMessage(e))
  check(identical(parsed, TRUE),
        paste("R/market_entry_scenarios.R does not parse:", parsed))

  source(scenario_path)

  required_functions <- c(
    "mortgage_monthly_payment",
    "market_entry_scenario",
    "market_entry_serviceability_series"
  )
  missing_functions <- required_functions[
    !vapply(required_functions, exists, logical(1), mode = "function")
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

if (length(failures) > 0) {
  stop(
    paste(c("Market-entry scenario checks failed:", paste0("- ", failures)),
          collapse = "\n"),
    call. = FALSE
  )
}

cat("Market-entry scenario checks passed.\n")

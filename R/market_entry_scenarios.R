# ==============================================================================
# Market-entry scenario helpers
# ==============================================================================
# Stylised mortgage and deposit calculations used by app-only scenario surfaces.
# These helpers are not official ABS measures and are not lender assessments.
# ==============================================================================

scenario_scalar <- function(value, name, positive = FALSE,
                            non_negative = FALSE) {
  if (length(value) != 1 || !is.numeric(value) || !is.finite(value)) {
    stop(name, " must be one finite numeric value.", call. = FALSE)
  }
  if (positive && value <= 0) {
    stop(name, " must be greater than zero.", call. = FALSE)
  }
  if (non_negative && value < 0) {
    stop(name, " must be zero or greater.", call. = FALSE)
  }
  value
}

mortgage_monthly_payment <- function(loan_amount, annual_rate_pct,
                                     term_years) {
  loan_amount <- scenario_scalar(loan_amount, "loan_amount", positive = TRUE)
  annual_rate_pct <- scenario_scalar(
    annual_rate_pct,
    "annual_rate_pct",
    non_negative = TRUE
  )
  term_years <- scenario_scalar(term_years, "term_years", positive = TRUE)

  n_payments <- term_years * 12
  monthly_rate <- annual_rate_pct / 100 / 12

  if (monthly_rate == 0) {
    return(loan_amount / n_payments)
  }

  loan_amount * monthly_rate / (1 - (1 + monthly_rate)^(-n_payments))
}

# Inverse of mortgage_monthly_payment(): the largest loan an annuity repayment
# of monthly_payment supports at annual_rate_pct over term_years.
mortgage_max_loan <- function(monthly_payment, annual_rate_pct, term_years) {
  monthly_payment <- scenario_scalar(
    monthly_payment,
    "monthly_payment",
    non_negative = TRUE
  )
  annual_rate_pct <- scenario_scalar(
    annual_rate_pct,
    "annual_rate_pct",
    non_negative = TRUE
  )
  term_years <- scenario_scalar(term_years, "term_years", positive = TRUE)

  n_payments <- term_years * 12
  monthly_rate <- annual_rate_pct / 100 / 12

  if (monthly_rate == 0) {
    return(monthly_payment * n_payments)
  }

  monthly_payment * (1 - (1 + monthly_rate)^(-n_payments)) / monthly_rate
}

market_entry_scenario <- function(dwelling_price, gross_annual_income,
                                  annual_rate_pct, deposit_pct = 20,
                                  term_years = 30, savings_rate_pct = 15,
                                  assessment_buffer_pp = 0,
                                  annual_non_housing_expenses = 0,
                                  monthly_other_debt = 0) {
  dwelling_price <- scenario_scalar(
    dwelling_price,
    "dwelling_price",
    positive = TRUE
  )
  gross_annual_income <- scenario_scalar(
    gross_annual_income,
    "gross_annual_income",
    positive = TRUE
  )
  annual_rate_pct <- scenario_scalar(
    annual_rate_pct,
    "annual_rate_pct",
    non_negative = TRUE
  )
  deposit_pct <- scenario_scalar(
    deposit_pct,
    "deposit_pct",
    positive = TRUE
  )
  term_years <- scenario_scalar(term_years, "term_years", positive = TRUE)
  savings_rate_pct <- scenario_scalar(
    savings_rate_pct,
    "savings_rate_pct",
    positive = TRUE
  )
  assessment_buffer_pp <- scenario_scalar(
    assessment_buffer_pp,
    "assessment_buffer_pp",
    non_negative = TRUE
  )
  annual_non_housing_expenses <- scenario_scalar(
    annual_non_housing_expenses,
    "annual_non_housing_expenses",
    non_negative = TRUE
  )
  monthly_other_debt <- scenario_scalar(
    monthly_other_debt,
    "monthly_other_debt",
    non_negative = TRUE
  )

  if (deposit_pct >= 100) {
    stop("deposit_pct must be less than 100.", call. = FALSE)
  }
  if (savings_rate_pct > 100) {
    stop("savings_rate_pct must be 100 or less.", call. = FALSE)
  }

  expense_adjusted_income <- gross_annual_income - annual_non_housing_expenses
  if (expense_adjusted_income <= 0) {
    stop(
      "gross_annual_income must exceed annual_non_housing_expenses.",
      call. = FALSE
    )
  }

  deposit <- dwelling_price * deposit_pct / 100
  loan_amount <- dwelling_price - deposit
  assessment_rate_pct <- annual_rate_pct + assessment_buffer_pp
  monthly_nominal_repayment <- mortgage_monthly_payment(
    loan_amount,
    annual_rate_pct,
    term_years
  )
  monthly_assessed_repayment <- mortgage_monthly_payment(
    loan_amount,
    assessment_rate_pct,
    term_years
  )
  n_payments <- term_years * 12

  data.frame(
    dwelling_price = dwelling_price,
    gross_annual_income = gross_annual_income,
    annual_rate_pct = annual_rate_pct,
    assessment_buffer_pp = assessment_buffer_pp,
    assessment_rate_pct = assessment_rate_pct,
    deposit_pct = deposit_pct,
    deposit = deposit,
    loan_amount = loan_amount,
    lvr_pct = loan_amount / dwelling_price * 100,
    term_years = term_years,
    savings_rate_pct = savings_rate_pct,
    annual_non_housing_expenses = annual_non_housing_expenses,
    monthly_other_debt = monthly_other_debt,
    monthly_nominal_repayment = monthly_nominal_repayment,
    monthly_assessed_repayment = monthly_assessed_repayment,
    nominal_repayment_to_gross_income_pct =
      monthly_nominal_repayment * 12 / gross_annual_income * 100,
    assessed_repayment_to_gross_income_pct =
      monthly_assessed_repayment * 12 / gross_annual_income * 100,
    expense_adjusted_repayment_ratio_pct =
      (monthly_assessed_repayment + monthly_other_debt) * 12 /
        expense_adjusted_income * 100,
    years_to_save_deposit =
      deposit / (gross_annual_income * savings_rate_pct / 100),
    total_nominal_interest = monthly_nominal_repayment * n_payments -
      loan_amount,
    stringsAsFactors = FALSE
  )
}

# Stylised serviceability-constrained borrowing capacity: the inverse of the
# market-entry scenario. Given income, it returns the largest loan whose
# repayment at the ASSESSED rate (rate + buffer) stays within a flat share of
# gross income (less any other-debt repayments), and the dwelling price that
# loan reaches at the chosen deposit. This is a flat repayment-to-income rule,
# NOT a lender HEM/DTI/net-surplus model, credit decision or approval, and not
# an official ABS measure.
borrowing_capacity_scenario <- function(gross_annual_income, annual_rate_pct,
                                        assessment_buffer_pp = 3,
                                        deposit_pct = 20, term_years = 30,
                                        target_repayment_ratio_pct = 30,
                                        monthly_other_debt = 0) {
  gross_annual_income <- scenario_scalar(
    gross_annual_income,
    "gross_annual_income",
    positive = TRUE
  )
  annual_rate_pct <- scenario_scalar(
    annual_rate_pct,
    "annual_rate_pct",
    non_negative = TRUE
  )
  assessment_buffer_pp <- scenario_scalar(
    assessment_buffer_pp,
    "assessment_buffer_pp",
    non_negative = TRUE
  )
  deposit_pct <- scenario_scalar(deposit_pct, "deposit_pct", positive = TRUE)
  term_years <- scenario_scalar(term_years, "term_years", positive = TRUE)
  target_repayment_ratio_pct <- scenario_scalar(
    target_repayment_ratio_pct,
    "target_repayment_ratio_pct",
    positive = TRUE
  )
  monthly_other_debt <- scenario_scalar(
    monthly_other_debt,
    "monthly_other_debt",
    non_negative = TRUE
  )

  if (deposit_pct >= 100) {
    stop("deposit_pct must be less than 100.", call. = FALSE)
  }

  assessment_rate_pct <- annual_rate_pct + assessment_buffer_pp
  # Flat repayment-to-income cap, net of existing debt commitments. Other debt
  # can exhaust the cap, leaving no capacity; clamp at zero rather than error.
  max_monthly_repayment <- max(
    0,
    target_repayment_ratio_pct / 100 * gross_annual_income / 12 -
      monthly_other_debt
  )
  max_loan <- mortgage_max_loan(
    max_monthly_repayment,
    assessment_rate_pct,
    term_years
  )
  implied_max_price <- max_loan / (1 - deposit_pct / 100)
  required_deposit <- implied_max_price - max_loan

  data.frame(
    gross_annual_income = gross_annual_income,
    annual_rate_pct = annual_rate_pct,
    assessment_buffer_pp = assessment_buffer_pp,
    assessment_rate_pct = assessment_rate_pct,
    deposit_pct = deposit_pct,
    term_years = term_years,
    target_repayment_ratio_pct = target_repayment_ratio_pct,
    monthly_other_debt = monthly_other_debt,
    max_monthly_repayment = max_monthly_repayment,
    max_loan = max_loan,
    implied_max_price = implied_max_price,
    required_deposit = required_deposit,
    stringsAsFactors = FALSE
  )
}

market_entry_scenario_presets <- function() {
  data.frame(
    preset_id = c("first_home_buyer", "mortgage_stress", "high_lvr_buyer"),
    label = c("First-home buyer", "Mortgage-stress", "High-LVR buyer"),
    dwelling_price = c(800000, 950000, 650000),
    gross_annual_income = c(120000, 140000, 95000),
    annual_rate_pct = c(6.0, 7.0, 6.0),
    deposit_pct = c(20, 10, 10),
    term_years = c(30, 30, 30),
    savings_rate_pct = c(15, 10, 12),
    assessment_buffer_pp = c(3, 3, 2),
    annual_non_housing_expenses = c(30000, 45000, 28000),
    monthly_other_debt = c(0, 750, 0),
    stringsAsFactors = FALSE
  )
}

renter_entry_scenario <- function(weekly_rent, gross_annual_income,
                                  bond_weeks = 4,
                                  upfront_moving_costs = 0,
                                  savings_rate_pct = 10,
                                  annual_non_housing_expenses = 0) {
  weekly_rent <- scenario_scalar(weekly_rent, "weekly_rent", positive = TRUE)
  gross_annual_income <- scenario_scalar(
    gross_annual_income,
    "gross_annual_income",
    positive = TRUE
  )
  bond_weeks <- scenario_scalar(bond_weeks, "bond_weeks", non_negative = TRUE)
  upfront_moving_costs <- scenario_scalar(
    upfront_moving_costs,
    "upfront_moving_costs",
    non_negative = TRUE
  )
  savings_rate_pct <- scenario_scalar(
    savings_rate_pct,
    "savings_rate_pct",
    positive = TRUE
  )
  annual_non_housing_expenses <- scenario_scalar(
    annual_non_housing_expenses,
    "annual_non_housing_expenses",
    non_negative = TRUE
  )

  if (savings_rate_pct > 100) {
    stop("savings_rate_pct must be 100 or less.", call. = FALSE)
  }

  expense_adjusted_income <- gross_annual_income - annual_non_housing_expenses
  if (expense_adjusted_income <= 0) {
    stop(
      "gross_annual_income must exceed annual_non_housing_expenses.",
      call. = FALSE
    )
  }

  annual_rent <- weekly_rent * 52
  bond_amount <- weekly_rent * bond_weeks
  upfront_cash_required <- bond_amount + upfront_moving_costs
  annual_savings <- gross_annual_income * savings_rate_pct / 100
  years_to_save_upfront <- upfront_cash_required / annual_savings

  data.frame(
    weekly_rent = weekly_rent,
    annual_rent = annual_rent,
    gross_annual_income = gross_annual_income,
    annual_non_housing_expenses = annual_non_housing_expenses,
    rent_to_gross_income_pct = annual_rent / gross_annual_income * 100,
    expense_adjusted_rent_ratio_pct =
      annual_rent / expense_adjusted_income * 100,
    bond_weeks = bond_weeks,
    bond_amount = bond_amount,
    upfront_moving_costs = upfront_moving_costs,
    upfront_cash_required = upfront_cash_required,
    savings_rate_pct = savings_rate_pct,
    years_to_save_upfront = years_to_save_upfront,
    weeks_to_save_upfront = years_to_save_upfront * 52,
    stringsAsFactors = FALSE
  )
}

renter_entry_scenario_presets <- function() {
  data.frame(
    preset_id = c("median_renter_entry", "tight_rental_entry",
                  "lower_income_renter"),
    label = c("Median renter entry", "Tight rental entry",
              "Lower-income renter"),
    weekly_rent = c(620, 760, 430),
    gross_annual_income = c(95000, 120000, 62000),
    bond_weeks = c(4, 4, 4),
    upfront_moving_costs = c(3000, 4500, 2200),
    savings_rate_pct = c(10, 12, 8),
    annual_non_housing_expenses = c(28000, 36000, 22000),
    stringsAsFactors = FALSE
  )
}

market_entry_sensitivity_grid <- function(dwelling_price, gross_annual_income,
                                          annual_rate_pct, deposit_pct = 20,
                                          term_years = 30,
                                          savings_rate_pct = 15,
                                          assessment_buffer_pp = 0,
                                          annual_non_housing_expenses = 0,
                                          monthly_other_debt = 0) {
  dwelling_price <- scenario_scalar(dwelling_price, "dwelling_price", positive = TRUE)
  gross_annual_income <- scenario_scalar(
    gross_annual_income,
    "gross_annual_income",
    positive = TRUE
  )
  annual_rate_pct <- scenario_scalar(
    annual_rate_pct,
    "annual_rate_pct",
    non_negative = TRUE
  )
  deposit_pct <- scenario_scalar(deposit_pct, "deposit_pct", positive = TRUE)
  term_years <- scenario_scalar(term_years, "term_years", positive = TRUE)
  savings_rate_pct <- scenario_scalar(
    savings_rate_pct,
    "savings_rate_pct",
    positive = TRUE
  )
  assessment_buffer_pp <- scenario_scalar(
    assessment_buffer_pp,
    "assessment_buffer_pp",
    non_negative = TRUE
  )
  annual_non_housing_expenses <- scenario_scalar(
    annual_non_housing_expenses,
    "annual_non_housing_expenses",
    non_negative = TRUE
  )
  monthly_other_debt <- scenario_scalar(
    monthly_other_debt,
    "monthly_other_debt",
    non_negative = TRUE
  )

  sensitivity_inputs <- rbind(
    data.frame(
      sensitivity = "interest_rate",
      input_value = sort(unique(pmax(0, annual_rate_pct + c(-1, 0, 1, 2)))),
      stringsAsFactors = FALSE
    ),
    data.frame(
      sensitivity = "dwelling_price",
      input_value = sort(unique(round(dwelling_price * c(0.85, 1, 1.15, 1.30)))),
      stringsAsFactors = FALSE
    ),
    data.frame(
      sensitivity = "deposit_share",
      input_value = sort(unique(pmin(40, pmax(5, deposit_pct + c(-10, 0, 10, 20))))),
      stringsAsFactors = FALSE
    ),
    data.frame(
      sensitivity = "non_housing_expenses",
      input_value = sort(unique(pmax(0, annual_non_housing_expenses +
        c(0, 15000, 30000, 45000)))),
      stringsAsFactors = FALSE
    )
  )

  rows <- lapply(seq_len(nrow(sensitivity_inputs)), function(i) {
    row <- sensitivity_inputs[i, ]
    scenario <- market_entry_scenario(
      dwelling_price = if (row$sensitivity == "dwelling_price") {
        row$input_value
      } else {
        dwelling_price
      },
      gross_annual_income = gross_annual_income,
      annual_rate_pct = if (row$sensitivity == "interest_rate") {
        row$input_value
      } else {
        annual_rate_pct
      },
      deposit_pct = if (row$sensitivity == "deposit_share") {
        row$input_value
      } else {
        deposit_pct
      },
      term_years = term_years,
      savings_rate_pct = savings_rate_pct,
      assessment_buffer_pp = assessment_buffer_pp,
      annual_non_housing_expenses = if (row$sensitivity == "non_housing_expenses") {
        row$input_value
      } else {
        annual_non_housing_expenses
      },
      monthly_other_debt = monthly_other_debt
    )
    cbind(
      data.frame(
        sensitivity = row$sensitivity,
        input_value = row$input_value,
        stringsAsFactors = FALSE
      ),
      scenario
    )
  })

  out <- do.call(rbind, rows)
  out[order(out$sensitivity, out$input_value), , drop = FALSE]
}

market_entry_serviceability_series <- function(price_ts, income_ts, rate_ts,
                                               deposit_pct = 20,
                                               lvr_pct = NULL,
                                               term_years = 30,
                                               assessment_buffer_pp = 3) {
  if (!is.null(lvr_pct)) {
    lvr_pct <- scenario_scalar(lvr_pct, "lvr_pct", positive = TRUE)
    if (lvr_pct >= 100) {
      stop("lvr_pct must be less than 100.", call. = FALSE)
    }
    deposit_pct <- 100 - lvr_pct
  }
  deposit_pct <- scenario_scalar(deposit_pct, "deposit_pct", positive = TRUE)
  if (deposit_pct >= 100) {
    stop("deposit_pct must be less than 100.", call. = FALSE)
  }
  term_years <- scenario_scalar(term_years, "term_years", positive = TRUE)
  assessment_buffer_pp <- scenario_scalar(
    assessment_buffer_pp,
    "assessment_buffer_pp",
    non_negative = TRUE
  )

  required_price_cols <- c("date", "price_k")
  required_income_cols <- c("date", "awe")
  required_rate_cols <- c("date", "rate")
  if (!all(required_price_cols %in% names(price_ts))) {
    stop("price_ts must include date and price_k columns.", call. = FALSE)
  }
  if (!all(required_income_cols %in% names(income_ts))) {
    stop("income_ts must include date and awe columns.", call. = FALSE)
  }
  if (!all(required_rate_cols %in% names(rate_ts))) {
    stop("rate_ts must include date and rate columns.", call. = FALSE)
  }

  aligned <- merge(price_ts[, required_price_cols],
                   income_ts[, required_income_cols],
                   by = "date")
  aligned <- merge(aligned, rate_ts[, required_rate_cols], by = "date")
  aligned$date <- as.Date(aligned$date)
  aligned$price_k <- suppressWarnings(as.numeric(aligned$price_k))
  aligned$awe <- suppressWarnings(as.numeric(aligned$awe))
  aligned$rate <- suppressWarnings(as.numeric(aligned$rate))
  aligned <- aligned[
    !is.na(aligned$date) &
      is.finite(aligned$price_k) & aligned$price_k > 0 &
      is.finite(aligned$awe) & aligned$awe > 0 &
      is.finite(aligned$rate) & aligned$rate >= 0,
    ,
    drop = FALSE
  ]
  if (nrow(aligned) == 0) {
    return(data.frame(
      date = as.Date(character()),
      scenario = character(),
      serviceability_pct = numeric(),
      assessment_buffer_pp = numeric(),
      stringsAsFactors = FALSE
    ))
  }

  aligned <- aligned[order(aligned$date), , drop = FALSE]

  rows <- lapply(seq_len(nrow(aligned)), function(i) {
    dwelling_price <- aligned$price_k[[i]] * 1000
    gross_annual_income <- aligned$awe[[i]] * 52
    nominal <- market_entry_scenario(
      dwelling_price = dwelling_price,
      gross_annual_income = gross_annual_income,
      annual_rate_pct = aligned$rate[[i]],
      deposit_pct = deposit_pct,
      term_years = term_years,
      savings_rate_pct = 15,
      assessment_buffer_pp = assessment_buffer_pp
    )

    data.frame(
      date = rep(as.Date(aligned$date[[i]]), 2),
      scenario = c("Nominal rate", "Assessed rate"),
      serviceability_pct = c(
        nominal$nominal_repayment_to_gross_income_pct,
        nominal$assessed_repayment_to_gross_income_pct
      ),
      assessment_buffer_pp = assessment_buffer_pp,
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, rows)
}

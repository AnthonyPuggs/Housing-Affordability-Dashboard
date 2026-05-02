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

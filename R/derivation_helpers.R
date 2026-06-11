# Pure derivation helpers for pipeline/04_derive_indicators.R.
#
# Extracted so the formulas behind every derived indicator are unit-testable
# against hand-computed values without running the pipeline (review TEST-10).
# The functions return the same frames the stage-04 blocks built inline;
# stage 04 wraps them with indicator_output() registry metadata.

# --- Alignment and indexing -----------------------------------------------------

# Complete-quarter rule (review STAT-07): a quarter of a higher-frequency
# series only enters a quarterly mean when it has as many observations as
# the series' typical (modal) per-quarter count. Without this, the partial
# latest quarter of a monthly input is averaged from one or two months,
# moves the derived indicator, and then silently revises once the missing
# months arrive. Quarterly inputs (modal count 1) pass through unchanged;
# ties resolve to the larger count so ambiguous partials are dropped.
complete_quarter_mean <- function(df, value_name = "value") {
  with_qtr <- df %>%
    mutate(qtr = floor_date(date, "quarter"))
  per_quarter <- with_qtr %>% count(qtr, name = "n_obs")
  count_freq <- table(per_quarter$n_obs)
  modal_n <- max(as.integer(names(count_freq)[count_freq == max(count_freq)]))

  with_qtr %>%
    group_by(qtr) %>%
    filter(dplyr::n() >= modal_n) %>%
    summarise(!!value_name := mean(value, na.rm = TRUE), .groups = "drop") %>%
    rename(date = qtr)
}

# Align two date/value series to common quarterly dates (mean within each
# complete quarter; see complete_quarter_mean).
align_quarterly <- function(df1, df2, name1 = "v1", name2 = "v2") {
  inner_join(
    complete_quarter_mean(df1, name1),
    complete_quarter_mean(df2, name2),
    by = "date"
  ) %>%
    arrange(date)
}

# Index a series to base_value at a reference position.
index_to_base <- function(values, base_idx = 1, base_value = 100) {
  base <- values[base_idx]
  if (is.na(base) || base == 0) return(rep(NA_real_, length(values)))
  values / base * base_value
}

# Quarterly mean of a (typically monthly) date/value series, complete
# quarters only (see complete_quarter_mean).
quarterly_mean <- function(df, value_name = "value") {
  complete_quarter_mean(df, value_name)
}

# --- Loud series selection -------------------------------------------------------

get_series_exact <- function(df, series_name, min_rows = 1,
                             col = "series", dataset = "input data") {
  if (nrow(df) == 0) {
    stop(dataset, " is empty; cannot select required series '", series_name, "'.")
  }
  if (!col %in% names(df)) {
    stop(dataset, " has no '", col, "' column; cannot select '", series_name, "'.")
  }

  matched <- df %>%
    filter(.data[[col]] == series_name) %>%
    select(date, value, all_of(col)) %>%
    arrange(date)

  if (nrow(matched) == 0) {
    stop(dataset, " is missing required series '", series_name, "'.")
  }
  if (length(unique(matched[[col]])) != 1) {
    stop(dataset, " selected multiple source series for '", series_name, "'.")
  }
  if (anyDuplicated(matched$date) > 0) {
    stop(dataset, " has duplicate dates for required series '", series_name, "'.")
  }
  if (nrow(matched) < min_rows) {
    stop(
      dataset, " series '", series_name, "' has ", nrow(matched),
      " observations; expected at least ", min_rows, "."
    )
  }

  matched %>%
    select(date, value)
}

# --- Indicator derivations --------------------------------------------------------

# Price-to-Income Ratio: indexed price level / indexed WPI x 100.
compute_price_to_income <- function(rppi, wpi) {
  align_quarterly(rppi, wpi, "rppi", "wpi") %>%
    mutate(
      rppi_idx = index_to_base(rppi),
      wpi_idx  = index_to_base(wpi),
      value    = rppi_idx / wpi_idx * 100
    )
}

# Mortgage Serviceability Index v2: indexed 30-year annuity P&I repayment
# burden at lvr x mean dwelling price (in $'000s), deflated by WPI.
compute_mortgage_serviceability <- function(price_k, wpi, new_loan_rate,
                                            lvr = 0.80, term_years = 30) {
  price_wpi <- align_quarterly(price_k, wpi, "price_k", "wpi")
  rate_qtr <- quarterly_mean(new_loan_rate, "rate")

  price_wpi %>%
    inner_join(rate_qtr, by = "date") %>%
    mutate(
      loan = lvr * price_k * 1000,
      monthly_rate = rate / 100 / 12,
      n_payments = term_years * 12,
      monthly_pmt = ifelse(
        monthly_rate == 0,
        loan / n_payments,
        loan * monthly_rate / (1 - (1 + monthly_rate)^(-n_payments))
      ),
      burden = monthly_pmt / wpi,
      value = index_to_base(burden)
    )
}

# Rental Affordability Index: indexed CPI rents / indexed WPI x 100.
compute_rental_affordability <- function(cpi_rents, wpi) {
  align_quarterly(cpi_rents, wpi, "rents", "wpi") %>%
    mutate(
      rents_idx = index_to_base(rents),
      wpi_idx   = index_to_base(wpi),
      value     = rents_idx / wpi_idx * 100
    )
}

# Deposit Gap: years to save a deposit_share deposit on the mean dwelling
# price (in $'000s) at savings_rate of gross AWE-proxy income.
compute_deposit_gap <- function(price_k, awe, savings_rate = 0.15,
                                deposit_share = 0.20) {
  align_quarterly(price_k, awe, "price_k", "awe") %>%
    mutate(
      dwelling_price = price_k * 1000,
      deposit_needed = dwelling_price * deposit_share,
      annual_income  = awe * 52,
      annual_savings = annual_income * savings_rate,
      value = deposit_needed / annual_savings
    ) %>%
    filter(!is.na(value) & is.finite(value))
}

# Real YoY growth: series deflated by CPI, four-quarter percentage change.
compute_real_growth_yoy <- function(series, cpi_all) {
  align_quarterly(series, cpi_all, "numerator", "cpi") %>%
    mutate(
      real_level = numerator / cpi * 100,
      value = 100 * (real_level / lag(real_level, 4) - 1)
    ) %>%
    filter(!is.na(value))
}

# Real Mortgage Rate: quarterly nominal rate minus quarterly CPI inflation.
compute_real_mortgage_rate <- function(mortgage_rate, cpi_inflation) {
  mr_qtr <- quarterly_mean(mortgage_rate, "nominal_rate")
  infl_qtr <- quarterly_mean(cpi_inflation, "inflation")

  inner_join(mr_qtr, infl_qtr, by = "date") %>%
    mutate(value = nominal_rate - inflation) %>%
    filter(!is.na(value))
}

# FHB Average Loan Size: commitment value ($ millions) over commitment count,
# joined on exactly matching dates (both series come from the same ABS 5601.0
# table, so a date mismatch means a source problem, not an alignment choice).
compute_fhb_average_loan_size <- function(value_millions, number) {
  inner_join(
    value_millions %>% rename(value_m = value),
    number %>% rename(n_loans = value),
    by = "date"
  ) %>%
    filter(is.finite(n_loans) & n_loans > 0) %>%
    mutate(value = value_m * 1e6 / n_loans) %>%
    arrange(date)
}

# Year-ended percentage change of a monthly series. Requires the observation
# exactly 12 months earlier (no nearest-neighbour fallback), so a gap in the
# source produces a missing growth value rather than a mislabelled one.
compute_monthly_yoy_growth <- function(df) {
  d <- df %>%
    arrange(date) %>%
    mutate(month_key = format(date, "%Y-%m"))
  prior <- d %>%
    mutate(month_key = format(date %m+% months(12), "%Y-%m")) %>%
    select(month_key, prior_value = value)
  d %>%
    inner_join(prior, by = "month_key") %>%
    filter(is.finite(prior_value) & prior_value != 0) %>%
    transmute(date, value = 100 * (value / prior_value - 1))
}

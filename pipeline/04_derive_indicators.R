# ==============================================================================
# 04_derive_indicators.R — Compute derived affordability indicators
# ==============================================================================
# Input:  data/abs_timeseries.csv, data/rba_rates.csv
# Output: data/affordability_indices.csv
#
# Indicators:
#   1. Price-to-Income Ratio (RPPI / WPI, indexed)
#   2. Mortgage Serviceability Index (annuity P&I repayments at the spliced
#      F6 new-loan rate / WPI, indexed) — v2
#   3. Rental Affordability Index (CPI Rents / WPI, indexed)
#   4. Deposit Gap in years
#   5. Real House Price Growth (RPPI deflated by CPI, YoY %)
#   6. Real Wage Growth (WPI deflated by CPI, YoY %)
#   7. Real Mortgage Rate (nominal rate − CPI inflation)
#
# Schema: date | value | indicator | geography | unit | frequency
# ==============================================================================

cat("--- Deriving affordability indicators ---\n")

if (!exists("indicator_registry", mode = "function")) {
  source(project_path("R", "indicator_registry.R"))
}
if (!exists("calculate_national_affordability_score", mode = "function")) {
  source(project_path("R", "national_affordability_score.R"))
}
if (!exists("compute_price_to_income", mode = "function")) {
  # Pure derivation formulas, unit-tested in tests/test_derivation_helpers.R.
  source(project_path("R", "derivation_helpers.R"))
}

# --- Load pipeline CSVs ------------------------------------------------------
abs_file <- file.path(DATA_DIR, "abs_timeseries.csv")
rba_file <- file.path(DATA_DIR, "rba_rates.csv")

if (!file.exists(abs_file)) {
  stop("abs_timeseries.csv not found. Run 02_fetch_abs_timeseries.R first.")
}

abs_ts <- read_csv(abs_file, show_col_types = FALSE) %>%
  mutate(date = as.Date(date))

rba_ts <- if (file.exists(rba_file)) {
  read_csv(rba_file, show_col_types = FALSE) %>%
    mutate(date = as.Date(date))
} else {
  warning("rba_rates.csv not found — mortgage-related indicators will be skipped")
  tibble()
}

indicator_output <- function(df, indicator_name) {
  metadata <- indicator_registry_output_metadata(indicator_name)
  df %>%
    transmute(
      date, value,
      indicator = metadata$indicator,
      geography = metadata$geography,
      unit = metadata$unit,
      frequency = metadata$frequency
    )
}

# --- Extract key series -------------------------------------------------------
rppi <- get_series_exact(abs_ts, INDICATOR_SOURCE_RPPI, min_rows = 40,
                         dataset = "abs_timeseries.csv")
wpi <- get_series_exact(abs_ts, INDICATOR_SOURCE_WPI, min_rows = 80,
                        dataset = "abs_timeseries.csv")
cpi_all <- get_series_exact(abs_ts, INDICATOR_SOURCE_CPI_ALL_GROUPS, min_rows = 100,
                            dataset = "abs_timeseries.csv")
cpi_rents <- get_series_exact(
  abs_ts,
  INDICATOR_SOURCE_CPI_RENTS_NATIONAL,
  min_rows = 100,
  dataset = "abs_timeseries.csv"
)
cpi_infl <- get_series_exact(abs_ts, INDICATOR_SOURCE_CPI_INFLATION_YOY, min_rows = 90,
                             dataset = "abs_timeseries.csv")
awe <- get_series_exact(abs_ts, INDICATOR_SOURCE_AWE, min_rows = 80,
                        dataset = "abs_timeseries.csv")

mortgage_rate <- tibble()
if (nrow(rba_ts) > 0) {
  mortgage_rate <- get_series_exact(
    rba_ts,
    INDICATOR_SOURCE_RBA_MORTGAGE_RATE,
    min_rows = 50,
    dataset = "rba_rates.csv"
  )
}

# Effective new-loan rate: F6 actual new-loan owner-occupier rates spliced
# onto level-adjusted F5 history (see rba_new_loan_rate_spliced). Loud on
# missing source series or insufficient splice overlap.
new_loan_rate <- tibble()
if (nrow(rba_ts) > 0) {
  new_loan_rate <- rba_new_loan_rate_spliced(rba_ts)
  cat("    Spliced new-loan rate:", nrow(new_loan_rate), "obs, wedge",
      round(attr(new_loan_rate, "splice_wedge"), 3), "pp\n")
}

cash_rate <- tibble()
if (nrow(rba_ts) > 0) {
  cash_rate <- rba_ts %>%
    filter(series == "Cash Rate Target") %>%
    select(date, value) %>%
    arrange(date)
}

all_indicators <- list()

# ==============================================================================
# 1. Price-to-Income Ratio
# ==============================================================================
cat("  Computing Price-to-Income Ratio...\n")

if (nrow(rppi) > 0 && nrow(wpi) > 0) {
  pti <- compute_price_to_income(rppi, wpi)

  all_indicators$price_to_income <- indicator_output(pti, "Price-to-Income Ratio")
  cat("    ", nrow(pti), "observations\n")
}

# ==============================================================================
# 2. Mortgage Serviceability Index
# ==============================================================================
cat("  Computing Mortgage Serviceability Index...\n")

if (nrow(rppi) > 0 && nrow(wpi) > 0 && nrow(new_loan_rate) > 0) {
  # v2 (affordability_indices_v2): indexed annuity principal-and-interest
  # repayment burden - a 30-year monthly annuity payment on an 80% LVR loan
  # against the national mean dwelling price, at the spliced F6 new-loan
  # owner-occupier rate, deflated by WPI as the income-growth proxy.
  # (v1 was interest-only price×rate/WPI, which overstated rate swings and
  # contradicted the methodology doc's own P&I definition - review ECON-02.)
  msi <- compute_mortgage_serviceability(rppi, wpi, new_loan_rate)

  all_indicators$mortgage_serviceability <- indicator_output(msi, "Mortgage Serviceability Index")
  cat("    ", nrow(msi), "observations\n")
}

# ==============================================================================
# 3. Rental Affordability Index
# ==============================================================================
cat("  Computing Rental Affordability Index...\n")

if (nrow(cpi_rents) > 0 && nrow(wpi) > 0) {
  rai <- compute_rental_affordability(cpi_rents, wpi)

  all_indicators$rental_affordability <- indicator_output(rai, "Rental Affordability Index")
  cat("    ", nrow(rai), "observations\n")
}

# ==============================================================================
# 4. Deposit Gap (years to save 20% deposit)
# ==============================================================================
cat("  Computing Deposit Gap...\n")

SAVINGS_RATE <- 0.15  # assumed share of gross income saved (AWE proxy)

if (nrow(rppi) > 0 && nrow(awe) > 0) {
  # The "RPPI" source series is the ABS 6432.0 national mean price of
  # residential dwellings in $'000s, so the dollar level is used directly.
  # (A previous version spliced a hard-coded $575,000 anchor mis-cited to
  # SIH File 10 onto this series' growth; File 10 covers property other than
  # the own home and cannot source an owner-occupied dwelling value.)
  deposit_data <- compute_deposit_gap(rppi, awe, savings_rate = SAVINGS_RATE)

  all_indicators$deposit_gap <- indicator_output(deposit_data, "Deposit Gap (Years)")
  cat("    ", nrow(deposit_data), "observations\n")
}

# ==============================================================================
# 5. Real House Price Growth (RPPI / CPI, YoY %)
# ==============================================================================
cat("  Computing Real House Price Growth...\n")

if (nrow(rppi) > 0 && nrow(cpi_all) > 0) {
  real_hp <- compute_real_growth_yoy(rppi, cpi_all)

  all_indicators$real_house_price_growth <- indicator_output(real_hp, "Real House Price Growth YoY")
  cat("    ", nrow(real_hp), "observations\n")
}

# ==============================================================================
# 6. Real Wage Growth (WPI / CPI, YoY %)
# ==============================================================================
cat("  Computing Real Wage Growth...\n")

if (nrow(wpi) > 0 && nrow(cpi_all) > 0) {
  real_wg <- compute_real_growth_yoy(wpi, cpi_all)

  all_indicators$real_wage_growth <- indicator_output(real_wg, "Real Wage Growth YoY")
  cat("    ", nrow(real_wg), "observations\n")
}

# ==============================================================================
# 7. Real Mortgage Rate
# ==============================================================================
cat("  Computing Real Mortgage Rate...\n")

if (nrow(mortgage_rate) > 0 && nrow(cpi_infl) > 0) {
  real_mr <- compute_real_mortgage_rate(mortgage_rate, cpi_infl)

  all_indicators$real_mortgage_rate <- indicator_output(real_mr, "Real Mortgage Rate")
  cat("    ", nrow(real_mr), "observations\n")
}

# ==============================================================================
# Combine and write
# ==============================================================================
base_affordability_indices <- bind_rows(all_indicators) %>%
  arrange(indicator, date)

national_score_indices <- calculate_national_affordability_score(
  base_affordability_indices
)

affordability_indices <- bind_rows(
  base_affordability_indices,
  national_score_indices
) %>%
  arrange(indicator, date)

if (nrow(affordability_indices) > 0) {
  write_pipeline_csv(affordability_indices, "affordability_indices.csv")
  cat("--- Indicator derivation complete ---\n")
  cat("  Indicators computed:", paste(unique(affordability_indices$indicator),
                                       collapse = ", "), "\n")
} else {
  cat("--- No indicators computed (missing upstream data) ---\n")
}

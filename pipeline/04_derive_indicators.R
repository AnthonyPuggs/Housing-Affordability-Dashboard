# ==============================================================================
# 04_derive_indicators.R — Compute derived affordability indicators
# ==============================================================================
# Input:  data/abs_timeseries.csv, data/rba_rates.csv
# Output: data/affordability_indices.csv
#
# Indicators:
#   1. Price-to-Income Ratio (RPPI / WPI, indexed)
#   2. Mortgage Serviceability Index (RPPI × mortgage_rate / WPI, indexed)
#   3. Rental Affordability Index (CPI Rents / WPI, indexed)
#   4. Deposit Gap in years
#   5. Real House Price Growth (RPPI deflated by CPI, YoY %)
#   6. Real Wage Growth (WPI deflated by CPI, YoY %)
#   7. Real Mortgage Rate (nominal rate − CPI inflation)
#
# Schema: date | value | indicator | geography | unit | frequency
# ==============================================================================

cat("--- Deriving affordability indicators ---\n")

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

# --- Helper: extract a required source series exactly -------------------------
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

# --- Helper: align two series to common quarterly dates -----------------------
align_quarterly <- function(df1, df2, name1 = "v1", name2 = "v2") {
  # Ensure both are quarterly by rounding to quarter-end
  df1 <- df1 %>%
    mutate(qtr = floor_date(date, "quarter")) %>%
    group_by(qtr) %>%
    summarise(!!name1 := mean(value, na.rm = TRUE), .groups = "drop") %>%
    rename(date = qtr)

  df2 <- df2 %>%
    mutate(qtr = floor_date(date, "quarter")) %>%
    group_by(qtr) %>%
    summarise(!!name2 := mean(value, na.rm = TRUE), .groups = "drop") %>%
    rename(date = qtr)

  inner_join(df1, df2, by = "date") %>%
    arrange(date)
}

# --- Helper: index a series to base=100 at a reference date -------------------
index_to_base <- function(values, base_idx = 1, base_value = 100) {
  base <- values[base_idx]
  if (is.na(base) || base == 0) return(rep(NA_real_, length(values)))
  values / base * base_value
}

# --- Extract key series -------------------------------------------------------
rppi <- get_series_exact(abs_ts, "RPPI", min_rows = 40,
                         dataset = "abs_timeseries.csv")
wpi <- get_series_exact(abs_ts, "WPI", min_rows = 80,
                        dataset = "abs_timeseries.csv")
cpi_all <- get_series_exact(abs_ts, "CPI All Groups", min_rows = 100,
                            dataset = "abs_timeseries.csv")
cpi_rents <- get_series_exact(
  abs_ts,
  "CPI Rents ; Weighted average of eight capital cities ;",
  min_rows = 100,
  dataset = "abs_timeseries.csv"
)
cpi_infl <- get_series_exact(abs_ts, "CPI Inflation YoY", min_rows = 90,
                             dataset = "abs_timeseries.csv")
awe <- get_series_exact(abs_ts, "AWE (AWOTE, Persons)", min_rows = 80,
                        dataset = "abs_timeseries.csv")

mortgage_rate <- tibble()
if (nrow(rba_ts) > 0) {
  mortgage_rate <- get_series_exact(
    rba_ts,
    "Lending rates; Housing loans; Banks; Variable; Discounted; Owner-occupier",
    min_rows = 50,
    dataset = "rba_rates.csv"
  )
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
  pti <- align_quarterly(rppi, wpi, "rppi", "wpi") %>%
    mutate(
      rppi_idx = index_to_base(rppi),
      wpi_idx  = index_to_base(wpi),
      value    = rppi_idx / wpi_idx * 100
    )

  all_indicators$price_to_income <- pti %>%
    transmute(
      date, value,
      indicator = "Price-to-Income Ratio",
      geography = "National",
      unit = "Index (base=100)",
      frequency = "Quarter"
    )
  cat("    ", nrow(pti), "observations\n")
}

# ==============================================================================
# 2. Mortgage Serviceability Index
# ==============================================================================
cat("  Computing Mortgage Serviceability Index...\n")

if (nrow(rppi) > 0 && nrow(wpi) > 0 && nrow(mortgage_rate) > 0) {
  # Align RPPI and WPI quarterly
  rppi_wpi <- align_quarterly(rppi, wpi, "rppi", "wpi")

  # Align mortgage rate to quarterly
  mr_qtr <- mortgage_rate %>%
    mutate(qtr = floor_date(date, "quarter")) %>%
    group_by(qtr) %>%
    summarise(mortgage_rate = mean(value, na.rm = TRUE), .groups = "drop") %>%
    rename(date = qtr)

  msi <- rppi_wpi %>%
    inner_join(mr_qtr, by = "date") %>%
    mutate(
      rppi_idx = index_to_base(rppi),
      wpi_idx  = index_to_base(wpi),
      # Multiplicative: (price × rate) / wages
      value = (rppi_idx * mortgage_rate / 100) / wpi_idx * 100
    )

  # Re-index MSI to base=100 at start
  msi$value <- index_to_base(msi$value)

  all_indicators$mortgage_serviceability <- msi %>%
    transmute(
      date, value,
      indicator = "Mortgage Serviceability Index",
      geography = "National",
      unit = "Index (base=100)",
      frequency = "Quarter"
    )
  cat("    ", nrow(msi), "observations\n")
}

# ==============================================================================
# 3. Rental Affordability Index
# ==============================================================================
cat("  Computing Rental Affordability Index...\n")

if (nrow(cpi_rents) > 0 && nrow(wpi) > 0) {
  rai <- align_quarterly(cpi_rents, wpi, "rents", "wpi") %>%
    mutate(
      rents_idx = index_to_base(rents),
      wpi_idx   = index_to_base(wpi),
      value     = rents_idx / wpi_idx * 100
    )

  all_indicators$rental_affordability <- rai %>%
    transmute(
      date, value,
      indicator = "Rental Affordability Index",
      geography = "National",
      unit = "Index (base=100)",
      frequency = "Quarter"
    )
  cat("    ", nrow(rai), "observations\n")
}

# ==============================================================================
# 4. Deposit Gap (years to save 20% deposit)
# ==============================================================================
cat("  Computing Deposit Gap...\n")

SAVINGS_RATE <- 0.15  # assumed household savings rate

if (nrow(rppi) > 0 && nrow(awe) > 0) {
  # Use SIH 2019-20 median dwelling price as base, scale by RPPI
  # SIH File 10 median dwelling value was ~$575,000 in 2019-20 (approx)
  SIH_BASE_PRICE <- 575000
  SIH_BASE_DATE  <- as.Date("2020-06-01")

  # Find RPPI value closest to base date
  rppi_base <- rppi %>%
    mutate(dist = abs(as.numeric(date - SIH_BASE_DATE))) %>%
    arrange(dist) %>%
    slice(1) %>%
    pull(value)

  deposit_data <- align_quarterly(rppi, awe, "rppi", "awe") %>%
    mutate(
      # Scale dwelling price by RPPI movement from base
      dwelling_price = SIH_BASE_PRICE * (rppi / rppi_base),
      deposit_needed = dwelling_price * 0.20,
      annual_income  = awe * 52,
      annual_savings = annual_income * SAVINGS_RATE,
      value = deposit_needed / annual_savings
    ) %>%
    filter(!is.na(value) & is.finite(value))

  all_indicators$deposit_gap <- deposit_data %>%
    transmute(
      date, value,
      indicator = "Deposit Gap (Years)",
      geography = "National",
      unit = "Years",
      frequency = "Quarter"
    )
  cat("    ", nrow(deposit_data), "observations\n")
}

# ==============================================================================
# 5. Real House Price Growth (RPPI / CPI, YoY %)
# ==============================================================================
cat("  Computing Real House Price Growth...\n")

if (nrow(rppi) > 0 && nrow(cpi_all) > 0) {
  real_hp <- align_quarterly(rppi, cpi_all, "rppi", "cpi") %>%
    mutate(
      real_rppi = rppi / cpi * 100,
      value = 100 * (real_rppi / lag(real_rppi, 4) - 1)
    ) %>%
    filter(!is.na(value))

  all_indicators$real_house_price_growth <- real_hp %>%
    transmute(
      date, value,
      indicator = "Real House Price Growth YoY",
      geography = "National",
      unit = "Per cent",
      frequency = "Quarter"
    )
  cat("    ", nrow(real_hp), "observations\n")
}

# ==============================================================================
# 6. Real Wage Growth (WPI / CPI, YoY %)
# ==============================================================================
cat("  Computing Real Wage Growth...\n")

if (nrow(wpi) > 0 && nrow(cpi_all) > 0) {
  real_wg <- align_quarterly(wpi, cpi_all, "wpi", "cpi") %>%
    mutate(
      real_wpi = wpi / cpi * 100,
      value = 100 * (real_wpi / lag(real_wpi, 4) - 1)
    ) %>%
    filter(!is.na(value))

  all_indicators$real_wage_growth <- real_wg %>%
    transmute(
      date, value,
      indicator = "Real Wage Growth YoY",
      geography = "National",
      unit = "Per cent",
      frequency = "Quarter"
    )
  cat("    ", nrow(real_wg), "observations\n")
}

# ==============================================================================
# 7. Real Mortgage Rate
# ==============================================================================
cat("  Computing Real Mortgage Rate...\n")

if (nrow(mortgage_rate) > 0 && nrow(cpi_infl) > 0) {
  mr_qtr <- mortgage_rate %>%
    mutate(qtr = floor_date(date, "quarter")) %>%
    group_by(qtr) %>%
    summarise(nominal_rate = mean(value, na.rm = TRUE), .groups = "drop") %>%
    rename(date = qtr)

  infl_qtr <- cpi_infl %>%
    mutate(qtr = floor_date(date, "quarter")) %>%
    group_by(qtr) %>%
    summarise(inflation = mean(value, na.rm = TRUE), .groups = "drop") %>%
    rename(date = qtr)

  real_mr <- inner_join(mr_qtr, infl_qtr, by = "date") %>%
    mutate(value = nominal_rate - inflation) %>%
    filter(!is.na(value))

  all_indicators$real_mortgage_rate <- real_mr %>%
    transmute(
      date, value,
      indicator = "Real Mortgage Rate",
      geography = "National",
      unit = "Per cent",
      frequency = "Quarter"
    )
  cat("    ", nrow(real_mr), "observations\n")
}

# ==============================================================================
# Combine and write
# ==============================================================================
affordability_indices <- bind_rows(all_indicators) %>%
  arrange(indicator, date)

if (nrow(affordability_indices) > 0) {
  write_pipeline_csv(affordability_indices, "affordability_indices.csv")
  cat("--- Indicator derivation complete ---\n")
  cat("  Indicators computed:", paste(unique(affordability_indices$indicator),
                                       collapse = ", "), "\n")
} else {
  cat("--- No indicators computed (missing upstream data) ---\n")
}

# ==============================================================================
# 02_fetch_abs_timeseries.R — Fetch live ABS time series for housing indicators
# ==============================================================================
# Source:  ABS via readabs package
# Output:  data/abs_timeseries.csv
#
# Schema: date | value | series | series_id | category | unit | frequency
# (matches normalize_abs() pattern from app_old.R)
# ==============================================================================

cat("--- Fetching ABS time series ---\n")

all_series <- list()

# ==============================================================================
# 1. Residential Property Price Index (RPPI) — ABS 6432.0 Mean Dwelling Price
# ==============================================================================
# NOTE: ABS 6416.0 (RPPI) was discontinued after Dec 2021. We now construct a
# proxy RPPI from "Mean price of residential dwellings" in ABS 6432.0 Table 1.
# The mean price series is indexed to base=100 and relabelled to match the old
# "Residential Property Price Index ; <city> ;" format for app compatibility.

# State-to-capital-city mapping for relabelling
state_to_city <- c(
  "Australia"                        = "Weighted average of eight capital cities",
  "New South Wales"                  = "Sydney",
  "Victoria"                         = "Melbourne",
  "Queensland"                       = "Brisbane",
  "South Australia"                  = "Adelaide",
  "Western Australia"                = "Perth",
  "Tasmania"                         = "Hobart",
  "Northern Territory"               = "Darwin",
  "Australian Capital Territory"     = "Canberra"
)

cat("  Fetching mean dwelling prices (6432.0 Table 1)...\n")

rppi_raw <- safe_read(
  read_abs(cat_no = "6432.0", tables = "1"),
  "Mean Dwelling Price 6432.0 Table 1"
)

if (nrow(rppi_raw) > 0) {
  # Select "Mean price of residential dwellings" series
  rppi <- rppi_raw %>%
    filter(str_detect(series, regex("^Mean price of residential dwellings",
                                    ignore_case = TRUE))) %>%
    mutate(
      # Extract state/territory name from series string
      state = str_trim(str_extract(series, ";\\s*([^;]+)\\s*;?$") %>%
                str_remove_all(";") %>% str_trim()),
      # Map state to capital city name
      city = state_to_city[state]
    ) %>%
    filter(!is.na(city)) %>%
    # Convert mean price to base=100 index (per city)
    group_by(city) %>%
    arrange(date) %>%
    mutate(value = value / first(value) * 100) %>%
    ungroup() %>%
    # Label as "Dwelling Price Index ; <city> ;" (distinct from "RPPI" used by derive script)
    mutate(series = paste0("Dwelling Price Index ;  ", city, " ;")) %>%
    # Drop raw unit column so normalize_abs uses our "Index" specification
    select(-any_of("unit")) %>%
    normalize_abs(category = "House Prices", units = "Index",
                  freq_hint = "Quarter")

  all_series$rppi <- rppi
  cat("    ", nrow(rppi), "RPPI (mean dwelling price proxy) observations\n")
}

# Also store a single national "RPPI" series (non-indexed) for the derive script
if (nrow(rppi_raw) > 0) {
  rppi_national <- rppi_raw %>%
    filter(str_detect(series, regex("^Mean price of residential dwellings.*Australia ;",
                                    ignore_case = TRUE))) %>%
    normalize_abs(label = "RPPI", category = "House Prices",
                  units = "AUD", freq_hint = "Quarter")
  all_series$rppi_national <- rppi_national
  cat("    ", nrow(rppi_national), "RPPI national (mean price AUD) observations\n")
}

# ==============================================================================
# 2. CPI Components — ABS 6401.0
# ==============================================================================
cat("  Fetching CPI components (6401.0)...\n")

# CPI groups table (Table 7 has expenditure class detail)
cpi_groups <- safe_read(
  read_abs(cat_no = "6401.0", tables = "7"),
  "CPI Groups 6401.0 Table 7"
)

if (nrow(cpi_groups) > 0) {
  # CPI Rents sub-index
  cpi_rents <- select_series(
    cpi_groups,
    "Rents",
    "CPI Rents", "Rental Prices", units = "Index"
  )
  all_series$cpi_rents <- cpi_rents

  # CPI New dwelling purchase
  cpi_new_dwelling <- select_series(
    cpi_groups,
    "New dwelling purchase",
    "CPI New Dwelling Purchase", "House Prices", units = "Index"
  )
  all_series$cpi_new_dwelling <- cpi_new_dwelling

  cat("    CPI Rents:", nrow(cpi_rents), "obs |",
      "New Dwelling:", nrow(cpi_new_dwelling), "obs\n")
}

# CPI All Groups via readabs helper
cat("  Fetching CPI All Groups...\n")
cpi_all <- safe_read(read_cpi(), "CPI All Groups") %>%
  transmute(date, value = as.numeric(cpi)) %>%
  normalize_abs(label = "CPI All Groups", category = "Prices",
                units = "Index", freq_hint = "Quarter")
all_series$cpi_all <- cpi_all
cat("    CPI All Groups:", nrow(cpi_all), "obs\n")

# Derive CPI Inflation YoY
if (nrow(cpi_all) > 0) {
  cpi_lag <- infer_lag_from_dates(cpi_all$date, fallback = 4)
  cpi_inflation <- cpi_all %>%
    arrange(date) %>%
    mutate(
      value = 100 * (value / lag(value, cpi_lag) - 1),
      series = "CPI Inflation YoY",
      category = "Prices",
      unit = "Per cent"
    ) %>%
    filter(!is.na(value))
  all_series$cpi_inflation <- cpi_inflation
  cat("    CPI Inflation YoY:", nrow(cpi_inflation), "obs\n")
}

# ==============================================================================
# 3. Wage Price Index (WPI) — ABS 6345.0
# ==============================================================================
cat("  Fetching WPI (6345.0)...\n")

wpi_raw <- safe_read(
  read_abs(cat_no = "6345.0", tables = "1"),
  "WPI 6345.0 Table 1"
)

if (nrow(wpi_raw) > 0) {
  # Total hourly rates of pay, all sectors
  wpi <- select_series(
    wpi_raw,
    "Total hourly rates of pay.*All sectors|Percentage Change.*Original.*All sectors",
    "WPI Total Hourly Rates", "Income", units = "Index"
  )

  if (nrow(wpi) == 0) {
    # Fallback: take index series
    wpi <- wpi_raw %>%
      filter(str_detect(series, regex("index|total", ignore_case = TRUE))) %>%
      normalize_abs(label = "WPI", category = "Income",
                    units = "Index", freq_hint = "Quarter")
  }

  all_series$wpi <- wpi
  cat("    WPI:", nrow(wpi), "obs\n")
}

# ==============================================================================
# 4. Average Weekly Earnings (AWE) — readabs helper
# ==============================================================================
cat("  Fetching AWE...\n")

awe <- safe_read(
  read_awe(wage_measure = "awote", sex = "persons", sector = "total"),
  "AWE (AWOTE)"
) %>%
  transmute(date, value = as.numeric(value)) %>%
  normalize_abs(label = "AWE (AWOTE, Persons)", category = "Income",
                units = "AUD", freq_hint = "Quarter")
all_series$awe <- awe
cat("    AWE:", nrow(awe), "obs\n")

# ==============================================================================
# 5. Household Disposable Income — ABS 5206.0 Table 2
# ==============================================================================
cat("  Fetching Household disposable income (5206.0)...\n")

na_table2 <- safe_read(
  read_abs(cat_no = "5206.0", tables = "2"),
  "National Accounts 5206.0 Table 2"
)

if (nrow(na_table2) > 0) {
  # 5206.0 Table 2 is expenditure-side GDP. Household disposable income is in
  # the ABS Household Income Account (cat 5220.0), not in 5206.0.
  # Use Compensation of Employees from Table 7 as a household income proxy.
  hdi_raw <- safe_read(
    read_abs(cat_no = "5206.0", tables = "7"),
    "5206.0 Table 7 (Income from GDP)",
    warn = FALSE
  )

  hdi <- tibble()
  if (nrow(hdi_raw) > 0) {
    # "Compensation of employees" is the broadest measure of labour income
    hdi <- select_series(
      hdi_raw,
      "^Compensation of employees ;$|^Compensation of employees$",
      "Compensation of Employees", "Income", units = "AUD millions"
    )
    # If exact match fails, try broader
    if (nrow(hdi) == 0) {
      hdi <- select_series(
        hdi_raw,
        "Compensation of employees",
        "Compensation of Employees", "Income", units = "AUD millions"
      )
      # Take only the level series (not percentage changes)
      if (nrow(hdi) > 0) {
        hdi <- hdi %>%
          filter(!str_detect(series, regex("percentage|change", ignore_case = TRUE)))
      }
    }
  }

  all_series$hdi <- hdi
  cat("    Compensation of Employees:", nrow(hdi), "obs\n")
}

# ==============================================================================
# 6. Labour Force — ABS 6202.0
# ==============================================================================
cat("  Fetching Labour Force (6202.0)...\n")

labour <- safe_read(
  read_abs(cat_no = "6202.0", tables = "1"),
  "Labour Force 6202.0 Table 1"
)

if (nrow(labour) > 0) {
  unemployment <- select_series(
    labour,
    "Unemployment rate.*Persons",
    "Unemployment Rate", "Labour Market", units = "Per cent"
  )
  all_series$unemployment <- unemployment

  participation <- select_series(
    labour,
    "Participation rate.*Persons",
    "Participation Rate", "Labour Market", units = "Per cent"
  )
  all_series$participation <- participation

  underemployment <- select_series(
    labour,
    "Underemployment rate.*Persons",
    "Underemployment Rate", "Labour Market", units = "Per cent"
  )
  all_series$underemployment <- underemployment

  underutilisation <- select_series(
    labour,
    "Underutilisation rate.*Persons",
    "Labour Underutilisation Rate", "Labour Market", units = "Per cent"
  )
  all_series$underutilisation <- underutilisation

  cat("    Unemployment:", nrow(unemployment), "obs |",
      "Participation:", nrow(participation), "obs |",
      "Underemployment:", nrow(underemployment), "obs |",
      "Underutilisation:", nrow(underutilisation), "obs\n")
}

# ==============================================================================
# 7. RPPI by dwelling type — ABS 6432.0 Table 2 median transfer prices
# ==============================================================================
# 6432.0 Table 2 has "Median Price of Established House Transfers" and
# "Median Price of Attached Dwelling Transfers" by capital city.
cat("  Fetching dwelling-type median prices (6432.0 Table 2)...\n")

rppi_type_raw <- safe_read(
  read_abs(cat_no = "6432.0", tables = "2"),
  "Dwelling Type Medians 6432.0 Table 2"
)

if (nrow(rppi_type_raw) > 0) {
  # Established Houses — median transfer price by capital city
  rppi_houses <- rppi_type_raw %>%
    filter(str_detect(series, regex("^Median Price of Established House Transfers",
                                    ignore_case = TRUE))) %>%
    mutate(series = str_replace(series,
      regex("Median Price of Established House Transfers \\(Unstratified\\)",
            ignore_case = TRUE),
      "RPPI Established Houses")) %>%
    normalize_abs(category = "House Prices", units = "AUD",
                  freq_hint = "Quarter")
  all_series$rppi_houses <- rppi_houses
  cat("    RPPI Houses (median price):", nrow(rppi_houses), "obs\n")

  # Attached Dwellings — median transfer price by capital city
  rppi_units <- rppi_type_raw %>%
    filter(str_detect(series, regex("^Median Price of Attached Dwelling Transfers",
                                    ignore_case = TRUE))) %>%
    mutate(series = str_replace(series,
      regex("Median Price of Attached Dwelling Transfers \\(Unstratified\\)",
            ignore_case = TRUE),
      "RPPI Attached Dwellings")) %>%
    normalize_abs(category = "House Prices", units = "AUD",
                  freq_hint = "Quarter")
  all_series$rppi_units <- rppi_units
  cat("    RPPI Attached Dwellings (median price):", nrow(rppi_units), "obs\n")
}

# ==============================================================================
# Combine and write
# ==============================================================================
abs_timeseries <- bind_rows(all_series) %>%
  distinct(date, series, .keep_all = TRUE) %>%
  arrange(category, series, date)

write_pipeline_csv(abs_timeseries, "abs_timeseries.csv")

cat("--- ABS time series fetch complete ---\n")
cat("  Total series:", length(unique(abs_timeseries$series)), "\n")
cat("  Date range:", as.character(min(abs_timeseries$date, na.rm = TRUE)),
    "to", as.character(max(abs_timeseries$date, na.rm = TRUE)), "\n")

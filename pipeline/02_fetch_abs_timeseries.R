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

# CPI Rents by capital city — Two sources:
#   1. ABS SDMX API (CPI dataflow, INDEX=115522): quarterly national (1972–present)
#   2. ABS 6401.0 Table 10 (readabs): monthly by city (Sep 2017–present)
# Monthly data is converted to quarterly by averaging all 3 months per quarter
# (ABS-recommended approach) and both are combined into a unified series set.

# Region code → city name mapping for the SDMX API
api_region_to_city <- c(
  "50" = "Weighted average of eight capital cities",
  "1"  = "Sydney",  "2" = "Melbourne", "3" = "Brisbane",
  "4"  = "Adelaide", "5" = "Perth",     "6" = "Hobart",
  "7"  = "Darwin",   "8" = "Canberra"
)

# --- Source 1: Quarterly national via ABS SDMX API ---
cat("  Fetching quarterly CPI Rents via ABS data API...\n")
cpi_rents_qtr <- safe_read({
  resp <- httr::GET(
    "https://data.api.abs.gov.au/rest/data/CPI/1.115522.10.50.Q",
    httr::add_headers(Accept = "text/csv"))
  if (httr::status_code(resp) != 200) stop("ABS API returned ", httr::status_code(resp))
  d <- readr::read_csv(I(httr::content(resp, as = "text", encoding = "UTF-8")),
                        show_col_types = FALSE)
  # Convert TIME_PERIOD "2024-Q3" → Date (first day of quarter)
  d %>%
    transmute(
      date = as.Date(paste0(
        substr(TIME_PERIOD, 1, 4), "-",
        sprintf("%02d", as.integer(substr(TIME_PERIOD, 7, 7)) * 3 - 2), "-01")),
      value = OBS_VALUE,
      series = paste0("CPI Rents ; ",
                      api_region_to_city[as.character(REGION)], " ;")
    )
}, "CPI Rents quarterly (API)")

if (nrow(cpi_rents_qtr) > 0) {
  cpi_rents_qtr <- cpi_rents_qtr %>%
    normalize_abs(category = "Rental Prices", units = "Index",
                  freq_hint = "Quarter")
  all_series$cpi_rents_qtr <- cpi_rents_qtr
  cat("    Quarterly national CPI Rents:", nrow(cpi_rents_qtr), "obs (",
      as.character(min(cpi_rents_qtr$date)), "to",
      as.character(max(cpi_rents_qtr$date)), ")\n")
}

# --- Source 2: Monthly city-level via readabs ---
cat("  Fetching monthly CPI Rents by city (6401.0 Table 10)...\n")
cpi_city_raw <- safe_read(
  read_abs(cat_no = "6401.0", tables = "10"),
  "CPI by City 6401.0 Table 10"
)

if (nrow(cpi_city_raw) > 0) {
  # Filter for "Index Numbers ;  Rents ;  <city> ;" and convert monthly to quarterly
  # by averaging all 3 months in each quarter (ABS-recommended seasonal adjustment).
  # NOTE: ABS re-based the CPI index; city-level data only has non-NA values from
  # ~Jul 2022 onward in Table 10. Earlier rows exist but are NA.
  cpi_rents_cities <- cpi_city_raw %>%
    filter(str_detect(series, regex("^Index Numbers ;\\s*Rents ;", ignore_case = TRUE))) %>%
    filter(!is.na(value)) %>%               # drop NA rows (pre-rebase periods)
    mutate(
      city = str_trim(str_extract(series, ";\\s*([^;]+)\\s*;?$") %>%
               str_remove_all(";") %>% str_trim()),
      series = paste0("CPI Rents ; ", city, " ;")
    ) %>%
    filter(!is.na(city), city != "") %>%
    # Deduplicate: keep one series_id per city (both IDs have identical values)
    group_by(city, date) %>%
    slice(1) %>%
    ungroup() %>%
    # Convert monthly to quarterly by averaging 3 months per quarter
    mutate(quarter = lubridate::floor_date(date, "quarter")) %>%
    group_by(city, series, quarter) %>%
    filter(n() == 3) %>%                    # only complete quarters
    summarise(value = mean(value), .groups = "drop") %>%
    rename(date = quarter) %>%
    select(-city) %>%
    normalize_abs(category = "Rental Prices", units = "Index",
                  freq_hint = "Quarter")

  # Remove city dates that overlap with quarterly national (avoid duplicates)
  if (nrow(cpi_rents_qtr) > 0) {
    national_series <- "CPI Rents ; Weighted average of eight capital cities ;"
    national_dates <- cpi_rents_qtr %>%
      filter(series == national_series) %>% pull(date)
    cpi_rents_cities <- cpi_rents_cities %>%
      filter(!(series == national_series & date %in% national_dates))
  }

  all_series$cpi_rents_cities <- cpi_rents_cities
  cat("    Monthly→quarterly CPI Rents by city:", nrow(cpi_rents_cities), "obs,",
      length(unique(cpi_rents_cities$series)), "cities\n")
}

# Also add national from Table 7 (monthly, converted to quarterly) for city-format
if (nrow(cpi_groups) > 0) {
  cpi_rents_national_monthly <- cpi_groups %>%
    filter(str_detect(series, regex("^Index Numbers ;\\s*Rents ;\\s*Australia",
                                    ignore_case = TRUE))) %>%
    filter(!is.na(value)) %>%               # drop NA rows (pre-rebase periods)
    mutate(series = "CPI Rents ; Weighted average of eight capital cities ;") %>%
    # Convert monthly to quarterly by averaging 3 months per quarter
    mutate(quarter = lubridate::floor_date(date, "quarter")) %>%
    group_by(series, quarter) %>%
    filter(n() == 3) %>%                    # only complete quarters
    summarise(value = mean(value), .groups = "drop") %>%
    rename(date = quarter) %>%
    select(-any_of("unit")) %>%
    normalize_abs(category = "Rental Prices", units = "Index",
                  freq_hint = "Quarter")

  # Remove overlap with API quarterly data
  if (nrow(cpi_rents_qtr) > 0) {
    existing_dates <- cpi_rents_qtr$date
    cpi_rents_national_monthly <- cpi_rents_national_monthly %>%
      filter(!date %in% existing_dates)
  }

  if (nrow(cpi_rents_national_monthly) > 0) {
    all_series$cpi_rents_national_monthly <- cpi_rents_national_monthly
    cat("    CPI Rents national (monthly→quarterly supplement):",
        nrow(cpi_rents_national_monthly), "obs\n")
  }
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

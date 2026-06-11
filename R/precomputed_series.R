# App-ready series derived from loaded dashboard CSVs.

rent_cpi_national_city <- "Weighted average of eight capital cities"

rent_cpi_coverage_summary <- function(data, national_city = rent_cpi_national_city) {
  if (nrow(data) == 0 || !"city" %in% names(data)) {
    return(tibble(
      city = character(),
      n = integer(),
      min_date = as.Date(character()),
      max_date = as.Date(character()),
      is_national = logical()
    ))
  }

  data %>%
    group_by(city) %>%
    summarise(
      n = n(),
      min_date = min(date, na.rm = TRUE),
      max_date = max(date, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(is_national = city == national_city)
}

rent_cpi_city_common_range <- function(coverage,
                                       national_city = rent_cpi_national_city) {
  city_coverage <- coverage %>%
    filter(city != national_city)

  if (nrow(city_coverage) == 0) {
    return(as.Date(c(NA, NA)))
  }

  as.Date(c(max(city_coverage$min_date), min(city_coverage$max_date)))
}

rent_cpi_default_city_selection <- function(cities,
                                            national_city = rent_cpi_national_city) {
  city_choices <- setdiff(cities, national_city)
  preferred <- c("Sydney", "Melbourne", "Brisbane", "Perth")
  selected <- preferred[preferred %in% city_choices]

  if (length(selected) == 0) {
    selected <- head(city_choices, 4)
  }

  selected
}

precompute_dashboard_series <- function(abs_ts, rba_rates, afford_idx) {
  rppi_all <- abs_ts %>%
    filter(str_detect(series, "^Dwelling Price Index ;")) %>%
    mutate(
      city = extract_city(series),
      dwelling_type = "Total"
    )

  rppi_houses <- abs_ts %>%
    filter(str_detect(series, "Median Price Established Houses")) %>%
    mutate(
      city = extract_city(series),
      dwelling_type = "Houses"
    )

  rppi_units <- abs_ts %>%
    filter(str_detect(series, "Median Price Attached Dwellings")) %>%
    mutate(
      city = extract_city(series),
      dwelling_type = "Units"
    )

  median_house_prices <- rppi_houses %>%
    filter(city %in% c("Sydney", "Melbourne", "Brisbane", "Adelaide",
                       "Perth", "Hobart", "Darwin", "Canberra")) %>%
    select(date, value, city)

  national_mean_price <- abs_ts %>%
    filter(series == "Mean Dwelling Price ; Australia ;") %>%
    select(date, value) %>%
    mutate(city = "National Avg")

  median_prices_combined <- bind_rows(median_house_prices, national_mean_price)

  awe_ts <- abs_ts %>%
    filter(str_detect(series, "AWE")) %>%
    select(date, awe = value) %>%
    mutate(qtr = lubridate::floor_date(date, "quarter")) %>%
    group_by(qtr) %>%
    summarise(awe = mean(awe, na.rm = TRUE), .groups = "drop") %>%
    rename(date = qtr)

  rppi_national_ts <- abs_ts %>%
    filter(series == "Mean Dwelling Price ; Australia ;") %>%
    select(date, price_k = value) %>%
    mutate(qtr = lubridate::floor_date(date, "quarter")) %>%
    group_by(qtr) %>%
    summarise(price_k = mean(price_k, na.rm = TRUE), .groups = "drop") %>%
    rename(date = qtr)

  # Effective new-loan owner-occupier rate: RBA F6 actual new-loan rates with
  # level-adjusted F5 history (helper in R/indicator_registry.R). Advertised
  # discounted rates sat ~1pp above rates actually paid since the mid-2010s.
  mortgage_rate_qtr <- rba_new_loan_rate_spliced(rba_rates) %>%
    select(date, rate = value) %>%
    mutate(qtr = lubridate::floor_date(date, "quarter")) %>%
    group_by(qtr) %>%
    summarise(rate = mean(rate, na.rm = TRUE), .groups = "drop") %>%
    rename(date = qtr)

  serviceability_ts <- rppi_national_ts %>%
    inner_join(awe_ts, by = "date") %>%
    inner_join(mortgage_rate_qtr, by = "date") %>%
    mutate(
      dwelling_price = price_k * 1000,
      loan = dwelling_price * 0.80,
      monthly_rate = rate / 100 / 12,
      n_payments = 30 * 12,
      monthly_pmt = ifelse(
        monthly_rate == 0,
        loan / n_payments,
        loan * monthly_rate / (1 - (1 + monthly_rate)^(-n_payments))
      ),
      annual_repayment = monthly_pmt * 12,
      annual_income = awe * 52,
      serviceability_pct = annual_repayment / annual_income * 100
    ) %>%
    select(date, serviceability_pct)

  afford_change <- afford_idx %>%
    filter(indicator %in% c("Rental Affordability Index",
                            "Mortgage Serviceability Index",
                            "Price-to-Income Ratio")) %>%
    group_by(indicator) %>%
    arrange(date) %>%
    mutate(
      base_val = first(value),
      pct_change = (value / base_val - 1) * 100
    ) %>%
    ungroup() %>%
    mutate(indicator_label = indicator_chart_label(indicator))

  national_affordability_score_ts <- afford_idx %>%
    filter(indicator == "National Housing Affordability Score") %>%
    transmute(date, score = value)

  score_component_metadata <- national_affordability_score_indicators() %>%
    filter(!is.na(component)) %>%
    select(indicator, component, component_label, weight, display_order)

  national_affordability_score_components <- afford_idx %>%
    filter(indicator %in% score_component_metadata$indicator) %>%
    left_join(score_component_metadata, by = "indicator") %>%
    select(date, value, indicator, component, component_label, weight,
           display_order)

  national_affordability_score_diagnostics_data <-
    national_affordability_score_diagnostics(afford_idx)

  if (nrow(national_affordability_score_components) == 0) {
    national_affordability_score_components <- tibble(
      date = as.Date(character()),
      value = numeric(),
      indicator = character(),
      component = character(),
      component_label = character(),
      weight = numeric(),
      display_order = integer()
    )
  }

  rppi_combined <- bind_rows(rppi_all, rppi_houses, rppi_units)

  # The all-dwellings mean-price indexes are whole-of-state/territory series
  # and are labelled with their true geography (review STAT-01); the genuine
  # capital-city series are the houses/units median transfer prices.
  rppi_states <- sort(unique(rppi_all$city))
  rppi_states <- c(
    rppi_states[rppi_states == "Australia"],
    rppi_states[rppi_states != "Australia"]
  )

  rppi_cities <- sort(unique(c(rppi_houses$city, rppi_units$city)))
  rppi_cities <- c(
    rppi_cities[rppi_cities == "Weighted average of eight capital cities"],
    rppi_cities[rppi_cities != "Weighted average of eight capital cities"]
  )

  rent_cpi_combined <- abs_ts %>%
    filter(str_detect(series, "^CPI Rents ;")) %>%
    mutate(city = extract_city(series))

  rent_cpi_cities <- sort(unique(rent_cpi_combined$city))
  rent_cpi_cities <- c(
    rent_cpi_cities[rent_cpi_cities == "Weighted average of eight capital cities"],
    rent_cpi_cities[rent_cpi_cities != "Weighted average of eight capital cities"]
  )
  rent_cpi_city_cities <- setdiff(rent_cpi_cities, rent_cpi_national_city)
  rent_cpi_default_cities <- rent_cpi_default_city_selection(rent_cpi_cities)
  rent_cpi_coverage <- rent_cpi_coverage_summary(rent_cpi_combined)
  rent_cpi_national_coverage <- rent_cpi_coverage %>%
    filter(city == rent_cpi_national_city)
  rent_cpi_national_range <- if (nrow(rent_cpi_national_coverage) == 0) {
    as.Date(c(NA, NA))
  } else {
    as.Date(c(
      min(rent_cpi_national_coverage$min_date),
      max(rent_cpi_national_coverage$max_date)
    ))
  }
  rent_cpi_city_range <- rent_cpi_city_common_range(rent_cpi_coverage)

  rba_cash_rate <- rba_rates %>%
    filter(series == "Cash Rate Target")

  rba_mortgage_var <- rba_rates %>%
    filter(str_detect(series, "Variable; Discounted; Owner-occupier"))

  rba_mortgage_fixed <- rba_rates %>%
    filter(str_detect(series, "3-year fixed; Owner-occupier"))

  rba_mortgage_std <- rba_rates %>%
    filter(str_detect(series, "Variable; Standard; Owner-occupier"))

  rba_investor_var <- rba_rates %>%
    filter(str_detect(series, "Variable; Discounted; Investor"))

  rba_investor_fixed <- rba_rates %>%
    filter(str_detect(series, "3-year fixed; Investor"))

  list(
    rppi_all = rppi_all,
    rppi_houses = rppi_houses,
    rppi_units = rppi_units,
    median_house_prices = median_house_prices,
    national_mean_price = national_mean_price,
    median_prices_combined = median_prices_combined,
    awe_ts = awe_ts,
    rppi_national_ts = rppi_national_ts,
    mortgage_rate_qtr = mortgage_rate_qtr,
    serviceability_ts = serviceability_ts,
    afford_change = afford_change,
    national_affordability_score_ts = national_affordability_score_ts,
    national_affordability_score_components = national_affordability_score_components,
    national_affordability_score_diagnostics_data =
      national_affordability_score_diagnostics_data,
    rppi_combined = rppi_combined,
    rppi_cities = rppi_cities,
    rppi_states = rppi_states,
    rent_cpi_combined = rent_cpi_combined,
    rent_cpi_cities = rent_cpi_cities,
    rent_cpi_city_cities = rent_cpi_city_cities,
    rent_cpi_default_cities = rent_cpi_default_cities,
    rent_cpi_coverage = rent_cpi_coverage,
    rent_cpi_national_city = rent_cpi_national_city,
    rent_cpi_national_range = rent_cpi_national_range,
    rent_cpi_city_range = rent_cpi_city_range,
    rba_cash_rate = rba_cash_rate,
    rba_mortgage_var = rba_mortgage_var,
    rba_mortgage_fixed = rba_mortgage_fixed,
    rba_mortgage_std = rba_mortgage_std,
    rba_investor_var = rba_investor_var,
    rba_investor_fixed = rba_investor_fixed
  )
}

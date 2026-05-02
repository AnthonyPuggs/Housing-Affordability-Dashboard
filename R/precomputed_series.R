# App-ready series derived from loaded dashboard CSVs.

precompute_dashboard_series <- function(abs_ts, rba_rates, afford_idx) {
  rppi_all <- abs_ts %>%
    filter(str_detect(series, "^Dwelling Price Index ;")) %>%
    mutate(
      city = extract_city(series),
      dwelling_type = "Total"
    )

  rppi_houses <- abs_ts %>%
    filter(str_detect(series, "RPPI Established Houses")) %>%
    mutate(
      city = extract_city(series),
      dwelling_type = "Houses"
    )

  rppi_units <- abs_ts %>%
    filter(str_detect(series, "RPPI Attached Dwellings")) %>%
    mutate(
      city = extract_city(series),
      dwelling_type = "Units"
    )

  median_house_prices <- rppi_houses %>%
    filter(city %in% c("Sydney", "Melbourne", "Brisbane", "Adelaide",
                       "Perth", "Hobart", "Darwin", "Canberra")) %>%
    select(date, value, city)

  national_mean_price <- abs_ts %>%
    filter(series == "RPPI") %>%
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
    filter(series == "RPPI") %>%
    select(date, price_k = value) %>%
    mutate(qtr = lubridate::floor_date(date, "quarter")) %>%
    group_by(qtr) %>%
    summarise(price_k = mean(price_k, na.rm = TRUE), .groups = "drop") %>%
    rename(date = qtr)

  mortgage_rate_qtr <- rba_rates %>%
    filter(str_detect(series, "Variable; Discounted; Owner-occupier")) %>%
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

  rppi_combined <- bind_rows(rppi_all, rppi_houses, rppi_units)

  rppi_cities <- sort(unique(rppi_all$city))
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
    rppi_combined = rppi_combined,
    rppi_cities = rppi_cities,
    rent_cpi_combined = rent_cpi_combined,
    rent_cpi_cities = rent_cpi_cities,
    rba_cash_rate = rba_cash_rate,
    rba_mortgage_var = rba_mortgage_var,
    rba_mortgage_fixed = rba_mortgage_fixed,
    rba_mortgage_std = rba_mortgage_std,
    rba_investor_var = rba_investor_var,
    rba_investor_fixed = rba_investor_fixed
  )
}

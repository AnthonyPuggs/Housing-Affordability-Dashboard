# ==============================================================================
# Shared Data Loading, Helpers & Theme
# ==============================================================================
# Sourced by both app.R (Shiny dashboard) and save_plots.R (static export)
# ==============================================================================

library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(scales)
library(rlang)
library(lubridate)

# ==============================================================================
# DATA LOADING — runs once at app startup
# ==============================================================================

data_dir <- file.path(getwd(), "data")

load_csv <- function(filename) {
  path <- file.path(data_dir, filename)
  if (file.exists(path)) read_csv(path, show_col_types = FALSE) else tibble()
}

abs_ts       <- load_csv("abs_timeseries.csv")
rba_rates    <- load_csv("rba_rates.csv")
afford_idx   <- load_csv("affordability_indices.csv")
supply_demand <- load_csv("abs_supply_demand.csv")

sih_national     <- load_csv("sih_timeseries_national.csv")
sih_state_ts     <- load_csv("sih_state_timeseries.csv")
sih_costs        <- load_csv("sih_costs_2020.csv")
sih_cost_ratios  <- load_csv("sih_cost_ratios_2020.csv")
sih_stress       <- load_csv("sih_stress_bands_2020.csv")
sih_nhha         <- load_csv("sih_nhha_rental_stress.csv")
# Loaded but reserved for future use:
# sih_age_tenure_2020.csv, sih_recent_buyers_2020.csv,
# sih_lower_income_states.csv, sih_geographic_2020.csv

# ==============================================================================
# HELPERS
# ==============================================================================

# Extract latest non-NA value from a time series
latest_val <- function(df, series_col, series_name, val_col = "value",
                       date_col = "date") {
  d <- df %>%
    filter(.data[[series_col]] == series_name, !is.na(.data[[val_col]])) %>%
    arrange(desc(.data[[date_col]]))
  if (nrow(d) == 0) return(NA_real_)
  d[[val_col]][1]
}

latest_date <- function(df, series_col, series_name, date_col = "date") {
  d <- df %>%
    filter(.data[[series_col]] == series_name, !is.na(value)) %>%
    arrange(desc(.data[[date_col]]))
  if (nrow(d) == 0) return(NA_character_)
  format(d[[date_col]][1], "%b %Y")
}

# Compute YoY or QoQ change for a time series
latest_change <- function(df, series_col, series_name, val_col = "value",
                          date_col = "date", periods_back = 4) {
  d <- df %>%
    filter(.data[[series_col]] == series_name, !is.na(.data[[val_col]])) %>%
    arrange(desc(.data[[date_col]]))
  if (nrow(d) < periods_back + 1) return(list(change = NA_real_, label = ""))
  current <- d[[val_col]][1]
  previous <- d[[val_col]][periods_back + 1]
  if (is.na(previous) || previous == 0) return(list(change = NA_real_, label = ""))
  pct <- (current / previous - 1) * 100
  direction <- if (pct >= 0) "\u2191" else "\u2193"
  lbl <- if (periods_back == 4) "YoY" else "QoQ"
  list(change = pct, label = paste0(direction, " ", sprintf("%+.1f%%", pct), " ", lbl))
}

# Smart formatting
fmt_dollar  <- function(x) if (is.na(x)) "N/A" else paste0("$", comma(round(x)))
fmt_dollar_k <- function(x) {
  if (is.na(x)) return("N/A")
  if (abs(x) >= 1e6) paste0("$", number(x / 1e6, accuracy = 0.01), "M")
  else if (abs(x) >= 1e3) paste0("$", comma(round(x / 1e3)), "k")
  else paste0("$", comma(round(x)))
}
fmt_pct     <- function(x, acc = 0.01) if (is.na(x)) "N/A" else paste0(number(x, accuracy = acc), "%")
fmt_ratio   <- function(x) if (is.na(x)) "N/A" else number(x, accuracy = 0.1)
fmt_years   <- function(x) if (is.na(x)) "N/A" else paste0(number(x, accuracy = 0.1), " yrs")
fmt_index   <- function(x) if (is.na(x)) "N/A" else number(x, accuracy = 0.1)
fmt_number  <- function(x) if (is.na(x)) "N/A" else comma(round(x))

# Tenure label mapping
tenure_labels <- c(
  "owner_outright" = "Owner (no mortgage)",
  "owner_mortgage" = "Owner (with mortgage)",
  "owner_total"    = "All Owners",
  "renter_private" = "Private Renter",
  "renter_social"  = "Social Renter",
  "renter_total"   = "All Renters",
  "all"            = "All Households"
)

label_tenure <- function(x) {
  ifelse(x %in% names(tenure_labels), tenure_labels[x], x)
}

# Extract city name from RPPI series string
extract_city <- function(s) {
  str_trim(str_extract(s, ";\\s*([^;]+)\\s*;?$") %>%
    str_remove_all(";") %>% str_trim())
}

# Colour palette for cities
city_colours <- c(
  "Sydney" = "#e41a1c", "Melbourne" = "#377eb8", "Brisbane" = "#4daf4a",
  "Adelaide" = "#984ea3", "Perth" = "#ff7f00", "Hobart" = "#a65628",
  "Darwin" = "#f781bf", "Canberra" = "#999999",
  "Weighted average of eight capital cities" = "#000000"
)

# ggplot theme with dark mode support
theme_afford <- function(dark = FALSE) {
  axis_col  <- if (dark) "#D7E0EA" else "#3B3F4A"
  panel_bg  <- if (dark) "#0F172A" else "#FFFFFF"
  grid_col  <- if (dark) "#334155" else "#D9DDE3"
  strip_col <- if (dark) "#E8EEF6" else "#1E2C4A"


  theme_minimal(base_size = 13) +
    theme(
      panel.background  = element_rect(fill = panel_bg, color = NA),
      plot.background   = element_rect(fill = panel_bg, color = NA),
      panel.grid.major  = element_line(color = grid_col),
      panel.grid.minor  = element_blank(),
      legend.position   = "bottom",
      legend.box        = "vertical",
      axis.text.x       = element_text(color = axis_col),
      axis.text.y       = element_text(color = axis_col),
      axis.title        = element_text(color = axis_col),
      legend.text       = element_text(color = axis_col),
      strip.text        = element_text(face = "bold", color = strip_col)
    )
}

# Apply plotly dark/light layout (handles faceted charts with multiple axes)
plotly_layout <- function(p, dark = FALSE) {
  bg    <- if (dark) "#0F172A" else "#FFFFFF"
  fg    <- if (dark) "#D7E0EA" else "#3B3F4A"
  grid  <- if (dark) "#334155" else "#D9DDE3"

  xax <- list(gridcolor = grid, title = "",
              tickformat = "%Y", dtick = "M60", tickangle = 0)
  yax <- list(gridcolor = grid)

  # Build layout args — include xaxis, xaxis2, ..., xaxis9 for faceted plots
  layout_args <- list(
    p, paper_bgcolor = bg, plot_bgcolor = bg,
    font = list(color = fg),
    legend = list(orientation = "h", y = -0.12, xanchor = "center", x = 0.5),
    margin = list(l = 50, r = 20, t = 30, b = 50),
    autosize = TRUE,
    xaxis = xax, yaxis = yax
  )
  for (i in 2:9) {
    layout_args[[paste0("xaxis", i)]] <- xax
    layout_args[[paste0("yaxis", i)]] <- yax
  }

  result <- do.call(plotly::layout, layout_args)
  result %>% plotly::config(responsive = TRUE)
}

# Build a time series line chart (ggplot, ready for ggplotly)
plot_ts <- function(df, x = "date", y = "value", colour = "series",
                    dark = FALSE, y_label = NULL) {
  p <- ggplot(df, aes(x = .data[[x]], y = .data[[y]], color = .data[[colour]])) +
    geom_line(linewidth = 1, alpha = 0.9) +
    scale_x_date(date_labels = "%Y", date_breaks = "5 years") +
    scale_y_continuous(labels = label_number(big.mark = ",", accuracy = 0.1)) +
    labs(x = NULL, y = y_label, color = NULL) +
    theme_afford(dark)
  p
}

# Build a bar chart
plot_bar <- function(df, x, y, fill = NULL, dark = FALSE,
                     position = "dodge", y_label = NULL,
                     coord_flip = FALSE) {
  mapping <- if (!is.null(fill)) {
    aes(x = .data[[x]], y = .data[[y]], fill = .data[[fill]])
  } else {
    aes(x = .data[[x]], y = .data[[y]])
  }
  p <- ggplot(df, mapping) +
    geom_col(position = position, alpha = 0.85, width = 0.7) +
    scale_y_continuous(labels = label_number(big.mark = ",", accuracy = 0.1)) +
    labs(x = NULL, y = y_label, fill = NULL) +
    theme_afford(dark)
  if (coord_flip) p <- p + coord_flip()
  p
}

# --- Pre-compute RPPI data with city labels ---
rppi_all <- abs_ts %>%
  filter(str_detect(series, "^Dwelling Price Index ;")) %>%
  mutate(city = extract_city(series),
         dwelling_type = "Total")

rppi_houses <- abs_ts %>%
  filter(str_detect(series, "RPPI Established Houses")) %>%
  mutate(city = extract_city(series),
         dwelling_type = "Houses")

rppi_units <- abs_ts %>%
  filter(str_detect(series, "RPPI Attached Dwellings")) %>%
  mutate(city = extract_city(series),
         dwelling_type = "Units")

# --- Median house prices in $'000 by capital city (from ABS 6432.0 Table 2) ---
median_house_prices <- rppi_houses %>%
  filter(city %in% c("Sydney", "Melbourne", "Brisbane", "Adelaide",
                     "Perth", "Hobart", "Darwin", "Canberra")) %>%
  select(date, value, city)

# National mean dwelling price in $'000 (from ABS 6432.0 Table 1)
national_mean_price <- abs_ts %>%
  filter(series == "RPPI") %>%
  select(date, value) %>%
  mutate(city = "National Avg")

# Combined for the overview chart
median_prices_combined <- bind_rows(median_house_prices, national_mean_price)

# --- Compute mortgage-to-income ratio time series ---
# Uses: national mean dwelling price, AWE, and mortgage rate
# Assumption: 80% LVR, 30-year term, standard P&I repayment
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
    monthly_pmt = ifelse(monthly_rate == 0, loan / n_payments,
                         loan * monthly_rate / (1 - (1 + monthly_rate)^(-n_payments))),
    annual_repayment = monthly_pmt * 12,
    annual_income = awe * 52,  # AWE is weekly
    serviceability_pct = annual_repayment / annual_income * 100
  ) %>%
  select(date, serviceability_pct)

# --- Compute affordability index changes from base period ---
# Express each index as % change from its earliest value
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
  mutate(indicator_label = case_when(
    indicator == "Rental Affordability Index" ~ "Rent Affordability",
    indicator == "Mortgage Serviceability Index" ~ "Mortgage Affordability",
    indicator == "Price-to-Income Ratio" ~ "Deposit Affordability"
  ))

rppi_combined <- bind_rows(rppi_all, rppi_houses, rppi_units)

# Available cities
rppi_cities <- sort(unique(rppi_all$city))
rppi_cities <- c(
  rppi_cities[rppi_cities == "Weighted average of eight capital cities"],
  rppi_cities[rppi_cities != "Weighted average of eight capital cities"]
)

# --- Pre-compute RBA key series ---
rba_cash_rate <- rba_rates %>%
  filter(series == "Cash Rate Target")

rba_mortgage_var <- rba_rates %>%
  filter(str_detect(series, "Variable; Discounted; Owner-occupier"))

rba_mortgage_fixed <- rba_rates %>%
  filter(str_detect(series, "3-year fixed; Owner-occupier"))

rba_mortgage_std <- rba_rates %>%
  filter(str_detect(series, "Variable; Standard; Owner-occupier"))

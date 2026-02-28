# ==============================================================================
# Static Plot Export & IDE Display
# ==============================================================================
# Run: source("save_plots.R") in Positron/RStudio console
# - Prints each plot to the IDE Plots pane
# - Saves PNGs to plots/ directory
# ==============================================================================

# --- Toggle flags ---
SAVE_TO_DISK <- TRUE   # Save PNGs to plots/
SHOW_IN_IDE  <- TRUE   # Print to IDE plot pane

# --- Load shared data & helpers ---
source("plot_setup.R")

# ==============================================================================
# BUILD ALL PLOTS (light mode, sensible defaults for reactive inputs)
# ==============================================================================

plots <- list()

# --------------------------------------------------------------------------
# 1. Overview: Capital City Median House Prices
# --------------------------------------------------------------------------
show_cities <- c("Sydney", "Melbourne", "Brisbane", "National Avg")
d <- median_prices_combined %>%
  filter(city %in% show_cities, date >= as.Date("2010-01-01")) %>%
  mutate(value_dollars = value * 1000)

price_colours <- c("Sydney" = "#2196F3", "Melbourne" = "#7B1FA2",
                   "Brisbane" = "#FF5722", "National Avg" = "#4CAF50")

if (nrow(d) > 0) {
  plots$overview_median_prices <- ggplot(d, aes(x = date, y = value_dollars, color = city)) +
    geom_line(aes(linetype = city), linewidth = 1.1, alpha = 0.9) +
    scale_color_manual(values = price_colours) +
    scale_linetype_manual(values = c("Sydney" = "solid", "Melbourne" = "solid",
                                     "Brisbane" = "solid", "National Avg" = "dashed")) +
    scale_x_date(date_labels = "%Y", date_breaks = "3 years") +
    scale_y_continuous(labels = label_dollar(prefix = "$", suffix = "k",
                                             scale = 1/1000, big.mark = ",")) +
    labs(title = "Capital City Median House Prices",
         x = NULL, y = NULL, color = NULL, linetype = NULL) +
    theme_afford(FALSE) +
    guides(linetype = "none")
}

# --------------------------------------------------------------------------
# 2. Overview: Housing Serviceability
# --------------------------------------------------------------------------
d <- serviceability_ts %>%
  filter(!is.na(serviceability_pct), date >= as.Date("2010-01-01"))

if (nrow(d) > 0) {
  plots$overview_serviceability <- ggplot(d, aes(x = date, y = serviceability_pct)) +
    geom_ribbon(aes(ymin = 30, ymax = pmax(serviceability_pct, 30)),
                fill = "#ffcdd2", alpha = 0.4) +
    geom_line(linewidth = 1.2, color = "#e53935") +
    geom_hline(yintercept = 30, linetype = "dashed", color = "#FF9800",
               linewidth = 0.8) +
    annotate("text", x = as.Date("2018-04-01"), y = 31,
             label = "Housing Stress Threshold (30%)",
             color = "#FF9800", size = 3.5, hjust = 0, vjust = 0) +
    scale_x_date(date_labels = "%Y", date_breaks = "3 years") +
    scale_y_continuous(labels = label_percent(scale = 1, accuracy = 0.1)) +
    labs(title = "Housing Serviceability", x = NULL, y = NULL) +
    theme_afford(FALSE)
}

# --------------------------------------------------------------------------
# 3. Overview: Affordability Index Changes
# --------------------------------------------------------------------------
d <- afford_change
change_colours <- c("Rent Affordability" = "#009688",
                    "Mortgage Affordability" = "#FF9800",
                    "Deposit Affordability" = "#1565C0")

if (nrow(d) > 0) {
  latest_vals <- d %>%
    group_by(indicator_label) %>%
    filter(date == max(date)) %>%
    ungroup()

  plots$overview_afford_change <- ggplot(d, aes(x = date, y = pct_change, color = indicator_label)) +
    geom_hline(yintercept = 0, color = "grey60", linewidth = 0.5) +
    geom_line(linewidth = 1.1, alpha = 0.9) +
    geom_point(data = latest_vals, size = 3) +
    scale_color_manual(values = change_colours) +
    scale_x_date(date_labels = "%b %Y", date_breaks = "3 years") +
    scale_y_continuous(labels = label_percent(scale = 1, accuracy = 1)) +
    labs(title = "Change in Affordability Indices Since Base Period",
         x = NULL, y = "Change from base period", color = NULL) +
    theme_afford(FALSE)
}

# --------------------------------------------------------------------------
# 4. Price Trends: Dwelling Price Index (default: all cities, Total, levels)
# --------------------------------------------------------------------------
default_cities <- c("Sydney", "Melbourne", "Brisbane",
                    "Weighted average of eight capital cities")

d <- rppi_combined %>%
  filter(city %in% default_cities,
         dwelling_type == "Total",
         date >= as.Date("2003-01-01"))

if (nrow(d) > 0) {
  plots$price_chart <- ggplot(d, aes(x = date, y = value, color = city)) +
    geom_line(linewidth = 1, alpha = 0.9) +
    scale_color_manual(values = city_colours, na.value = "grey50") +
    scale_x_date(date_labels = "%Y", date_breaks = "3 years") +
    scale_y_continuous(labels = label_number(big.mark = ",", accuracy = 0.1)) +
    labs(title = "Dwelling Price Index by Capital City",
         x = NULL, y = "Index", color = NULL) +
    theme_afford(FALSE)
}

# --------------------------------------------------------------------------
# 5. Price Trends: CPI New Dwelling Purchase
# --------------------------------------------------------------------------
d <- abs_ts %>%
  filter(series == "CPI New Dwelling Purchase",
         date >= as.Date("2003-01-01"))

if (nrow(d) > 0) {
  plots$price_cpi_construction <- ggplot(d, aes(x = date, y = value)) +
    geom_line(linewidth = 1, color = "#0E5A8A") +
    scale_x_date(date_labels = "%Y", date_breaks = "5 years") +
    labs(title = "CPI New Dwelling Purchase (Construction Cost)",
         x = NULL, y = "Index") +
    theme_afford(FALSE)
}

# --------------------------------------------------------------------------
# 6. Affordability: Indices (faceted)
# --------------------------------------------------------------------------
default_indicators <- c("Price-to-Income Ratio", "Mortgage Serviceability Index")

d <- afford_idx %>%
  filter(indicator %in% default_indicators,
         date >= as.Date("2003-01-01"))

if (nrow(d) > 0) {
  plots$afford_indices_chart <- ggplot(d, aes(x = date, y = value, color = indicator)) +
    geom_line(linewidth = 1) +
    facet_wrap(~indicator, scales = "free_y") +
    scale_x_date(date_labels = "%Y", date_breaks = "5 years") +
    labs(title = "Affordability Indicators",
         x = NULL, y = NULL, color = NULL) +
    theme_afford(FALSE) +
    theme(legend.position = "none")
}

# --------------------------------------------------------------------------
# 7. Affordability: Stress bands (default: age_group, all_households)
# --------------------------------------------------------------------------
d <- sih_stress %>%
  filter(breakdown_var == "age_group",
         stat_type == "all_households",
         tenure == "all",
         metric %in% c("pct_25_or_less", "pct_25_to_30",
                       "pct_30_to_50", "pct_over_50"),
         breakdown_val != "Total") %>%
  mutate(stress_band = case_when(
    metric == "pct_25_or_less" ~ "<25%",
    metric == "pct_25_to_30"   ~ "25-30%",
    metric == "pct_30_to_50"   ~ "30-50%",
    metric == "pct_over_50"    ~ ">50%"
  )) %>%
  mutate(stress_band = factor(stress_band,
                              levels = c("<25%", "25-30%", "30-50%", ">50%")))

stress_cols <- c("<25%" = "#2ecc71", "25-30%" = "#f39c12",
                 "30-50%" = "#e74c3c", ">50%" = "#8e44ad")

if (nrow(d) > 0) {
  plots$stress_chart <- ggplot(d, aes(x = breakdown_val, y = value, fill = stress_band)) +
    geom_col(position = "stack", alpha = 0.9) +
    scale_fill_manual(values = stress_cols) +
    labs(title = "Housing Cost Stress Bands (2019-20) by Age Group",
         x = NULL, y = "% of Households", fill = "Cost/Income") +
    coord_flip() +
    theme_afford(FALSE)
}

# --------------------------------------------------------------------------
# 8. Affordability: Cost burden heatmap (default: age_group, mean)
# --------------------------------------------------------------------------
d <- sih_cost_ratios %>%
  filter(breakdown_var == "age_group",
         stat_type == "mean",
         breakdown_val != "Total",
         tenure %in% c("owner_mortgage", "renter_private",
                       "renter_total", "all")) %>%
  mutate(tenure_label = label_tenure(tenure))

if (nrow(d) > 0) {
  plots$burden_heatmap <- ggplot(d, aes(x = tenure_label, y = breakdown_val, fill = value)) +
    geom_tile(color = "white", linewidth = 0.5) +
    geom_text(aes(label = round(value, 1)), size = 3.5) +
    scale_fill_gradient2(low = "#2ecc71", mid = "#f39c12", high = "#e74c3c",
                         midpoint = 25, name = "Cost/Income %") +
    labs(title = "Housing Cost-to-Income Ratio by Tenure & Age (2019-20)",
         x = NULL, y = NULL) +
    theme_afford(FALSE) +
    theme(axis.text.x = element_text(angle = 30, hjust = 1))
}

# --------------------------------------------------------------------------
# 9. Market Context: Cash Rate & Mortgage Rates
# --------------------------------------------------------------------------
d <- bind_rows(
  rba_cash_rate %>% mutate(series = "RBA Cash Rate"),
  rba_mortgage_var %>% mutate(series = "Owner-occ Variable (Discounted)"),
  rba_mortgage_fixed %>% mutate(series = "Owner-occ 3yr Fixed"),
  rba_investor_var %>% mutate(series = "Investor Variable (Discounted)"),
  rba_investor_fixed %>% mutate(series = "Investor 3yr Fixed")
) %>%
  distinct(date, series, .keep_all = TRUE) %>%
  filter(date >= as.Date("2000-01-01"))

rate_colours <- c(
  "RBA Cash Rate" = "#1B5E20",
  "Owner-occ Variable (Discounted)" = "#2196F3",
  "Owner-occ 3yr Fixed" = "#1565C0",
  "Investor Variable (Discounted)" = "#FF9800",
  "Investor 3yr Fixed" = "#E65100"
)

if (nrow(d) > 0) {
  plots$context_rates <- ggplot(d, aes(x = date, y = value, color = series)) +
    geom_line(linewidth = 1, alpha = 0.9) +
    scale_color_manual(values = rate_colours) +
    scale_x_date(date_labels = "%Y", date_breaks = "5 years") +
    scale_y_continuous(labels = label_number(big.mark = ",", accuracy = 0.1)) +
    labs(title = "Cash Rate & Mortgage Rates",
         x = NULL, y = "%", color = NULL) +
    theme_afford(FALSE)
}

# --------------------------------------------------------------------------
# 10. Market Context: Labour Market
# --------------------------------------------------------------------------
d <- abs_ts %>%
  filter(series %in% c("Unemployment Rate", "Underemployment Rate",
                        "Labour Underutilisation Rate"),
         date >= as.Date("2000-01-01"))

labour_fills <- c("Unemployment Rate" = "#2196F3",
                  "Underemployment Rate" = "#AB47BC",
                  "Labour Underutilisation Rate" = "#78909C")

d <- d %>%
  mutate(series = factor(series,
    levels = c("Labour Underutilisation Rate", "Underemployment Rate",
               "Unemployment Rate")))

if (nrow(d) > 0) {
  plots$context_labour <- ggplot(d, aes(x = date, y = value, fill = series)) +
    geom_area(alpha = 0.6, linewidth = 0.5, colour = "white") +
    scale_fill_manual(values = labour_fills) +
    scale_x_date(date_labels = "%Y", date_breaks = "5 years") +
    scale_y_continuous(labels = label_percent(scale = 1, accuracy = 0.1)) +
    labs(title = "Labour Market Spare Capacity",
         x = NULL, y = NULL, fill = NULL) +
    theme_afford(FALSE)
}

# --------------------------------------------------------------------------
# 11. Market Context: Population (NOM)
# --------------------------------------------------------------------------
d <- supply_demand %>%
  filter(str_detect(series, "Net Overseas Migration"),
         date >= as.Date("2000-01-01"))

if (nrow(d) > 0) {
  plots$context_pop <- ggplot(d, aes(x = date, y = value)) +
    geom_col(fill = "#29B6F6", alpha = 0.85, width = 60) +
    scale_x_date(date_labels = "%Y", date_breaks = "2 years") +
    scale_y_continuous(labels = label_number(big.mark = ",", accuracy = 1)) +
    labs(title = "Net Overseas Migration (NOM) per Annum",
         x = NULL, y = "Thousands") +
    theme_afford(FALSE)
}

# --------------------------------------------------------------------------
# 12. Market Context: Building Approvals
# --------------------------------------------------------------------------
d <- supply_demand %>%
  filter(category == "Building Approvals",
         date >= as.Date("2000-01-01")) %>%
  mutate(series_short = series %>%
    str_remove("Total number of dwelling units ;\\s*") %>%
    str_remove("\\s*;\\s*$") %>%
    str_replace(";\\s*", " - ") %>%
    str_replace(";\\s*", " - "))

if (nrow(d) > 0) {
  plots$context_approvals <- ggplot(d, aes(x = date, y = value, color = series_short)) +
    geom_line(linewidth = 0.8, alpha = 0.85) +
    scale_x_date(date_labels = "%Y", date_breaks = "5 years") +
    scale_y_continuous(labels = label_number(big.mark = ",")) +
    labs(title = "Building Approvals",
         x = NULL, y = "Number of Dwellings", color = NULL) +
    theme_afford(FALSE) +
    theme(legend.text = element_text(size = 8))
}

# --------------------------------------------------------------------------
# 13. Rental Market: NHHA Rental Stress by State (default: 2019-20)
# --------------------------------------------------------------------------
default_year <- "2019-20"
d <- sih_nhha %>%
  filter(survey_year == default_year,
         metric == "pct_rental_stress_over_30",
         geography != "Aust.")

nat <- sih_nhha %>%
  filter(survey_year == default_year,
         metric == "pct_rental_stress_over_30",
         geography == "Aust.")

if (nrow(d) > 0) {
  plots$rental_stress_state <- ggplot(d, aes(x = reorder(geography, -value), y = value)) +
    geom_col(fill = "#e74c3c", alpha = 0.85, width = 0.7) +
    {if (nrow(nat) > 0) geom_hline(yintercept = nat$value[1],
                                     linetype = "dashed", color = "#333")} +
    labs(title = paste("NHHA Rental Stress by State (", default_year, ")"),
         x = NULL, y = "% in Rental Stress (>30% of income)") +
    theme_afford(FALSE)
}

# --------------------------------------------------------------------------
# 14. Rental Market: Rental Stress Trends
# --------------------------------------------------------------------------
d <- sih_nhha %>%
  filter(metric == "pct_rental_stress_over_30") %>%
  mutate(year_num = as.numeric(str_extract(survey_year, "^\\d{4}")))

if (nrow(d) > 0) {
  plots$rental_stress_trend <- ggplot(d, aes(x = year_num, y = value, color = geography)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    scale_x_continuous(breaks = unique(d$year_num)) +
    labs(title = "NHHA Rental Stress Trends (Over Time)",
         x = NULL, y = "% in Rental Stress", color = NULL) +
    theme_afford(FALSE)
}

# --------------------------------------------------------------------------
# 15. Rental Market: Rental Affordability Index
# --------------------------------------------------------------------------
d <- afford_idx %>%
  filter(indicator == "Rental Affordability Index")

if (nrow(d) > 0) {
  plots$rental_afford_index <- ggplot(d, aes(x = date, y = value)) +
    geom_line(linewidth = 1, color = "#e74c3c") +
    scale_x_date(date_labels = "%Y", date_breaks = "5 years") +
    labs(title = "Rental Affordability Index",
         x = NULL, y = "Index (CPI Rents / WPI)") +
    theme_afford(FALSE)
}

# --------------------------------------------------------------------------
# 16. Rental Market: Weekly Rental Costs by Demographics (default: age_group)
# --------------------------------------------------------------------------
d <- sih_costs %>%
  filter(tenure %in% c("renter_private", "renter_total"),
         breakdown_var == "age_group",
         stat_type == "mean",
         breakdown_val != "Total") %>%
  mutate(tenure_label = label_tenure(tenure))

if (nrow(d) > 0) {
  plots$rental_costs_demo <- ggplot(d, aes(x = breakdown_val, y = value, fill = tenure_label)) +
    geom_col(position = "dodge", alpha = 0.85) +
    labs(title = "Weekly Rental Costs by Age Group (2019-20)",
         x = NULL, y = "Mean Weekly Rent ($)", fill = NULL) +
    coord_flip() +
    theme_afford(FALSE)
}

# ==============================================================================
# EXPORT LOOP
# ==============================================================================

if (SAVE_TO_DISK) {
  dir.create("plots", showWarnings = FALSE)
}

cat(sprintf("\n=== Exporting %d plots ===\n\n", length(plots)))

for (name in names(plots)) {
  cat(sprintf("  [%d/%d] %s", which(names(plots) == name), length(plots), name))

  if (SAVE_TO_DISK) {
    path <- file.path("plots", paste0(name, ".png"))
    ggsave(path, plots[[name]], width = 10, height = 6, dpi = 150)
    cat(sprintf(" -> %s", path))
  }

  if (SHOW_IN_IDE) {
    print(plots[[name]])
  }

  cat("\n")
}

cat(sprintf("\nDone. %d plots exported.\n", length(plots)))
if (SAVE_TO_DISK) cat("PNGs saved to: plots/\n")

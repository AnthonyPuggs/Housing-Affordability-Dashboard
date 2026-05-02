repo_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
failures <- character()

check <- function(condition, message) {
  if (!isTRUE(condition)) {
    failures <<- c(failures, message)
  }
}

helper_path <- file.path(repo_root, "R", "chart_builders.R")
description_path <- file.path(repo_root, "DESCRIPTION")

check(file.exists(helper_path), "R/chart_builders.R does not exist")
check(file.exists(description_path), "DESCRIPTION does not exist")

if (file.exists(helper_path)) {
  parsed <- tryCatch({
    parse(helper_path)
    TRUE
  }, error = function(e) conditionMessage(e))
  check(identical(parsed, TRUE),
        paste("R/chart_builders.R does not parse:", parsed))
}

suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(scales)
  library(lubridate)
})

source(file.path(repo_root, "R", "dashboard_formatting.R"))
source(file.path(repo_root, "R", "visual_semantics.R"))
source(file.path(repo_root, "R", "dashboard_theme.R"))
if (file.exists(helper_path)) {
  source(helper_path)
}

required_functions <- c(
  "price_series_transform",
  "build_dwelling_price_plot",
  "rent_cpi_series_transform",
  "build_rent_cpi_plot",
  "build_context_rates_plot",
  "build_context_labour_plot",
  "build_context_population_plot",
  "build_supply_approvals_plot",
  "build_supply_construction_cpi_plot",
  "build_rental_stress_state_plot",
  "build_rental_stress_trend_plot",
  "build_rental_affordability_index_plot",
  "build_rental_costs_demographic_plot",
  "build_geo_state_trend_plot",
  "build_geo_state_latest_plot",
  "build_geo_lower_income_plot",
  "build_geo_gcc_comparison_plot",
  "overview_price_series_transform",
  "build_overview_median_prices_plot",
  "build_overview_affordability_plot",
  "build_affordability_indices_plot",
  "build_market_entry_serviceability_plot",
  "build_housing_stress_bands_plot",
  "build_cost_burden_heatmap_plot"
)

for (fn in required_functions) {
  check(exists(fn, mode = "function"), paste(fn, "must be defined"))
}

quarterly_dates <- as.Date(c(
  "2020-01-01", "2020-04-01", "2020-07-01", "2020-10-01", "2021-01-01"
))

price_fixture <- data.frame(
  date = rep(quarterly_dates, 2),
  value = c(100, 110, 120, 130, 150, 80, 82, 84, 86, 88),
  city = rep(c("Sydney", "Melbourne"), each = length(quarterly_dates)),
  stringsAsFactors = FALSE
)

if (exists("price_series_transform", mode = "function")) {
  price_levels <- price_series_transform(price_fixture, "levels")
  check(identical(price_levels$value, price_fixture$value),
        "price_series_transform('levels') must preserve values")

  price_yoy <- price_series_transform(price_fixture, "yoy")
  syd_yoy <- price_yoy$value[price_yoy$city == "Sydney"]
  check(nrow(price_yoy) == 2,
        "price_series_transform('yoy') must keep one fifth-quarter row per city")
  check(isTRUE(all.equal(syd_yoy, 50, tolerance = 1e-8)),
        "price_series_transform('yoy') must use four-quarter percentage change")

  price_index <- price_series_transform(price_fixture, "index")
  syd_index <- price_index$value[price_index$city == "Sydney"]
  check(isTRUE(all.equal(syd_index, c(100, 110, 120, 130, 150),
                         tolerance = 1e-8)),
        "price_series_transform('index') must index each city to its first value")
}

rent_fixture <- price_fixture

if (exists("rent_cpi_series_transform", mode = "function")) {
  rent_qoq <- rent_cpi_series_transform(rent_fixture, "qoq")
  syd_qoq <- rent_qoq$value[rent_qoq$city == "Sydney"]
  check(isTRUE(all.equal(syd_qoq,
                         c(10, 100 * (120 / 110 - 1),
                           100 * (130 / 120 - 1),
                           100 * (150 / 130 - 1)),
                         tolerance = 1e-8)),
        "rent_cpi_series_transform('qoq') must use one-quarter percentage change")

  rent_yoy <- rent_cpi_series_transform(rent_fixture, "yoy")
  check(nrow(rent_yoy) == 2,
        "rent_cpi_series_transform('yoy') must keep one fifth-quarter row per city")
}

if (exists("build_dwelling_price_plot", mode = "function")) {
  p <- build_dwelling_price_plot(price_fixture, "levels", dark = FALSE)
  check(inherits(p, "ggplot"), "build_dwelling_price_plot() must return a ggplot")
}

if (exists("build_rent_cpi_plot", mode = "function")) {
  p <- build_rent_cpi_plot(rent_fixture, "index", dark = TRUE)
  check(inherits(p, "ggplot"), "build_rent_cpi_plot() must return a ggplot")
}

rates_fixture <- data.frame(
  date = quarterly_dates,
  value = c(1, 2, 3, 4, 5),
  series = "RBA Cash Rate",
  stringsAsFactors = FALSE
)

labour_fixture <- data.frame(
  date = rep(quarterly_dates, 3),
  value = rep(c(4, 6, 10, 8, 7), 3),
  series = rep(c("Unemployment Rate", "Underemployment Rate",
                 "Labour Underutilisation Rate"),
               each = length(quarterly_dates)),
  stringsAsFactors = FALSE
)

population_fixture <- data.frame(
  date = quarterly_dates,
  value = c(10, 15, 20, 25, 30),
  stringsAsFactors = FALSE
)

if (exists("build_context_rates_plot", mode = "function")) {
  check(inherits(build_context_rates_plot(rates_fixture, dark = FALSE), "ggplot"),
        "build_context_rates_plot() must return a ggplot")
}

if (exists("build_context_labour_plot", mode = "function")) {
  check(inherits(build_context_labour_plot(labour_fixture, dark = TRUE), "ggplot"),
        "build_context_labour_plot() must return a ggplot")
}

if (exists("build_context_population_plot", mode = "function")) {
  check(inherits(build_context_population_plot(population_fixture, dark = FALSE),
                 "ggplot"),
        "build_context_population_plot() must return a ggplot")
}

supply_fixture <- data.frame(
  date = rep(quarterly_dates, 2),
  value = c(1000, 1200, 1100, 1300, 1250, 900, 940, 990, 1010, 1080),
  approval_label = rep(c("NSW", "VIC"), each = length(quarterly_dates)),
  stringsAsFactors = FALSE
)

construction_fixture <- data.frame(
  date = quarterly_dates,
  value = c(100, 102, 104, 106, 108),
  stringsAsFactors = FALSE
)

if (exists("build_supply_approvals_plot", mode = "function")) {
  check(inherits(build_supply_approvals_plot(
    supply_fixture,
    title = "Total approvals - Total sectors",
    dark = FALSE
  ), "ggplot"), "build_supply_approvals_plot() must return a ggplot")
}

if (exists("build_supply_construction_cpi_plot", mode = "function")) {
  check(inherits(build_supply_construction_cpi_plot(construction_fixture, dark = TRUE),
                 "ggplot"),
        "build_supply_construction_cpi_plot() must return a ggplot")
}

survey_fixture <- data.frame(
  survey_year = c("2015-16", "2017-18", "2019-20", "2015-16", "2017-18", "2019-20"),
  geography = rep(c("NSW", "VIC"), each = 3),
  value = c(32, 35, 37, 29, 31, 34),
  reliability_marker = c("", "\u2020", "", "", "", "\u2020"),
  hover_text = paste("row", seq_len(6)),
  estimate_lower_95 = c(30, 33, NA, 27, NA, 31),
  estimate_upper_95 = c(34, 37, NA, 31, NA, 37),
  tile_label = c("32", "35\u2020", "37", "29", "31", "34\u2020"),
  tile_text_colour = "#F8FAFC",
  stringsAsFactors = FALSE
)
survey_fixture$survey_year <- factor(survey_fixture$survey_year,
                                     levels = c("2015-16", "2017-18", "2019-20"))

if (exists("build_rental_stress_state_plot", mode = "function")) {
  p <- build_rental_stress_state_plot(
    survey_fixture[survey_fixture$survey_year == "2019-20", ],
    national_value = 35,
    dark = FALSE
  )
  check(inherits(p, "ggplot"),
        "build_rental_stress_state_plot() must return a ggplot")
  has_errorbar <- any(vapply(p$layers, function(layer) {
    inherits(layer$geom, "GeomErrorbar")
  }, logical(1)))
  check(has_errorbar,
        "build_rental_stress_state_plot() must include error bars when interval columns are supplied")
}

if (exists("build_rental_stress_trend_plot", mode = "function")) {
  p <- build_rental_stress_trend_plot(survey_fixture, dark = TRUE)
  check(inherits(p, "ggplot"),
        "build_rental_stress_trend_plot() must return a ggplot")
  has_tile <- any(vapply(p$layers, function(layer) {
    inherits(layer$geom, "GeomTile")
  }, logical(1)))
  has_text <- any(vapply(p$layers, function(layer) {
    inherits(layer$geom, "GeomText")
  }, logical(1)))
  check(has_tile && has_text,
        "build_rental_stress_trend_plot() must remain a tile/text heatmap")
}

rental_index_fixture <- data.frame(
  date = quarterly_dates,
  value = c(90, 95, 100, 105, 110),
  stringsAsFactors = FALSE
)

if (exists("build_rental_affordability_index_plot", mode = "function")) {
  check(inherits(build_rental_affordability_index_plot(rental_index_fixture, dark = FALSE),
                 "ggplot"),
        "build_rental_affordability_index_plot() must return a ggplot")
}

rental_cost_fixture <- data.frame(
  breakdown_label = c("Under 35", "35 and over", "Under 35", "35 and over"),
  tenure_label = c("Private Renter", "Private Renter", "All Renters", "All Renters"),
  value = c(420, 480, 390, 450),
  stringsAsFactors = FALSE
)

if (exists("build_rental_costs_demographic_plot", mode = "function")) {
  check(inherits(build_rental_costs_demographic_plot(rental_cost_fixture, dark = TRUE),
                 "ggplot"),
        "build_rental_costs_demographic_plot() must return a ggplot")
}

geo_fixture <- data.frame(
  survey_year = survey_fixture$survey_year,
  geography = survey_fixture$geography,
  value = survey_fixture$value,
  reliability_marker = survey_fixture$reliability_marker,
  hover_text = survey_fixture$hover_text,
  estimate_lower_95 = survey_fixture$estimate_lower_95,
  estimate_upper_95 = survey_fixture$estimate_upper_95,
  stringsAsFactors = FALSE
)

percent_labels <- scales::label_percent(scale = 1, accuracy = 1)

if (exists("build_geo_state_trend_plot", mode = "function")) {
  check(inherits(build_geo_state_trend_plot(
    geo_fixture,
    metric_label = "Cost-to-income ratio",
    axis_label = "% of gross income",
    value_labels = percent_labels,
    dark = FALSE
  ), "ggplot"), "build_geo_state_trend_plot() must return a ggplot")
}

if (exists("build_geo_state_latest_plot", mode = "function")) {
  check(inherits(build_geo_state_latest_plot(
    geo_fixture[geo_fixture$survey_year == "2019-20", ],
    latest_year = "2019-20",
    axis_label = "% of gross income",
    value_labels = percent_labels,
    dark = TRUE
  ), "ggplot"), "build_geo_state_latest_plot() must return a ggplot")
}

if (exists("build_geo_lower_income_plot", mode = "function")) {
  p <- build_geo_lower_income_plot(
    geo_fixture[geo_fixture$survey_year == "2019-20", ],
    national_value = 35,
    metric_label = "Stress over 30%",
    axis_label = "% of gross income",
    value_labels = percent_labels,
    dark = FALSE
  )
  check(inherits(p, "ggplot"),
        "build_geo_lower_income_plot() must return a ggplot")
  has_errorbar <- any(vapply(p$layers, function(layer) {
    inherits(layer$geom, "GeomErrorbar")
  }, logical(1)))
  check(has_errorbar,
        "build_geo_lower_income_plot() must include error bars when interval columns are supplied")
}

if (exists("build_geo_gcc_comparison_plot", mode = "function")) {
  check(inherits(build_geo_gcc_comparison_plot(
    geo_fixture[geo_fixture$survey_year == "2019-20", ],
    metric_label = "Median cost-to-income ratio",
    axis_label = "% of gross income",
    value_labels = percent_labels,
    dark = TRUE
  ), "ggplot"), "build_geo_gcc_comparison_plot() must return a ggplot")
}

overview_price_fixture <- data.frame(
  date = rep(quarterly_dates, 2),
  value = c(500, 550, 600, 650, 700, 450, 475, 500, 525, 550),
  city = rep(c("Sydney", "National Avg"), each = length(quarterly_dates)),
  stringsAsFactors = FALSE
)

if (exists("overview_price_series_transform", mode = "function")) {
  overview_nominal <- overview_price_series_transform(overview_price_fixture, "nominal")
  check(isTRUE(all.equal(
    overview_nominal$plot_value[overview_nominal$city == "Sydney"],
    c(500000, 550000, 600000, 650000, 700000),
    tolerance = 1e-8
  )), "overview_price_series_transform('nominal') must convert thousand-dollar values to dollars")

  overview_index <- overview_price_series_transform(overview_price_fixture, "index")
  check(isTRUE(all.equal(
    overview_index$plot_value[overview_index$city == "Sydney"],
    c(100, 110, 120, 130, 140),
    tolerance = 1e-8
  )), "overview_price_series_transform('index') must index each city to its first value")
}

overview_price_colours <- c(
  "Sydney" = "#2196F3",
  "National Avg" = "#4CAF50"
)
overview_show_cities <- c("Sydney", "National Avg")

if (exists("build_overview_median_prices_plot", mode = "function")) {
  overview_plot_data <- overview_price_series_transform(
    overview_price_fixture,
    "nominal"
  )
  check(inherits(build_overview_median_prices_plot(
    overview_plot_data,
    is_index = FALSE,
    price_colours = overview_price_colours,
    show_cities = overview_show_cities,
    dark = FALSE
  ), "ggplot"), "build_overview_median_prices_plot() must return a ggplot")
}

overview_afford_fixture <- data.frame(
  date = rep(quarterly_dates, 2),
  value = c(100, 105, 110, 120, 125, 95, 98, 102, 106, 111),
  indicator_label = rep(c("Price-to-Income Cost Pressure",
                          "Rent Cost Pressure"),
                        each = length(quarterly_dates)),
  stringsAsFactors = FALSE
)

if (exists("build_overview_affordability_plot", mode = "function")) {
  check(inherits(build_overview_affordability_plot(
    overview_afford_fixture,
    colours = cost_pressure_palette(unique(overview_afford_fixture$indicator_label)),
    dark = TRUE
  ), "ggplot"), "build_overview_affordability_plot() must return a ggplot")
}

if (exists("build_affordability_indices_plot", mode = "function")) {
  check(inherits(build_affordability_indices_plot(
    overview_afford_fixture,
    dark = FALSE
  ), "ggplot"), "build_affordability_indices_plot() must return a ggplot")
}

serviceability_fixture <- data.frame(
  date = rep(quarterly_dates, 2),
  serviceability_pct = c(20, 21, 22, 23, 24, 26, 27, 28, 29, 30),
  scenario = rep(c("Nominal rate", "Assessed rate"),
                 each = length(quarterly_dates)),
  stringsAsFactors = FALSE
)

if (exists("build_market_entry_serviceability_plot", mode = "function")) {
  p <- build_market_entry_serviceability_plot(serviceability_fixture, dark = FALSE)
  check(inherits(p, "ggplot"),
        "build_market_entry_serviceability_plot() must return a ggplot")
  has_hline <- any(vapply(p$layers, function(layer) {
    inherits(layer$geom, "GeomHline")
  }, logical(1)))
  check(has_hline,
        "build_market_entry_serviceability_plot() must include the 30% stress reference line")
}

stress_fixture <- data.frame(
  breakdown_val = rep(c("Group A", "Group B"), each = 4),
  value = c(50, 15, 25, 10, 45, 20, 25, 10),
  stress_band = factor(rep(c("<25%", "25-30%", "30-50%", ">50%"), 2),
                       levels = c("<25%", "25-30%", "30-50%", ">50%")),
  reliability_marker = c("", "", "\u2020", "", "", "", "", ""),
  hover_text = paste("stress", seq_len(8)),
  stringsAsFactors = FALSE
)

if (exists("build_housing_stress_bands_plot", mode = "function")) {
  p <- build_housing_stress_bands_plot(stress_fixture, dark = TRUE)
  check(inherits(p, "ggplot"),
        "build_housing_stress_bands_plot() must return a ggplot")
  has_errorbar <- any(vapply(p$layers, function(layer) {
    inherits(layer$geom, "GeomErrorbar")
  }, logical(1)))
  check(!has_errorbar,
        "build_housing_stress_bands_plot() must not draw stacked-bar error bars")
}

burden_fixture <- data.frame(
  tenure_label = rep(c("All Households", "Private Renter"), each = 2),
  breakdown_val = rep(c("Group A", "Group B"), 2),
  value = c(10, 20, 30, 40),
  stringsAsFactors = FALSE
)

if (exists("build_cost_burden_heatmap_plot", mode = "function")) {
  p <- build_cost_burden_heatmap_plot(burden_fixture, dark = FALSE)
  check(inherits(p, "ggplot"),
        "build_cost_burden_heatmap_plot() must return a ggplot")
  has_tile <- any(vapply(p$layers, function(layer) {
    inherits(layer$geom, "GeomTile")
  }, logical(1)))
  has_text <- any(vapply(p$layers, function(layer) {
    inherits(layer$geom, "GeomText")
  }, logical(1)))
  check(has_tile && has_text,
        "build_cost_burden_heatmap_plot() must include tile and text layers")
}

if (file.exists(description_path)) {
  desc_text <- paste(readLines(description_path, warn = FALSE), collapse = "\n")
  forbidden_direct_dependencies <- c("ggthemes", "patchwork", "cowplot", "DT")
  unexpected <- forbidden_direct_dependencies[
    vapply(forbidden_direct_dependencies, function(pkg) {
      grepl(paste0("(^|\\n)\\s*", pkg, "\\s*,?\\s*($|\\n)"),
            desc_text, perl = TRUE)
    }, logical(1))
  ]
  check(length(unexpected) == 0,
        paste("DESCRIPTION must not add new direct dependencies:",
              paste(unexpected, collapse = ", ")))
}

if (length(failures) > 0) {
  stop(
    paste(c("Chart builder checks failed:", paste0("- ", failures)),
          collapse = "\n"),
    call. = FALSE
  )
}

cat("Chart builder checks passed.\n")

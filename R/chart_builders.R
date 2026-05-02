# Pure ggplot builders for dashboard charts.

price_series_transform <- function(data, transform = c("levels", "yoy", "index")) {
  transform <- match.arg(transform)
  if (nrow(data) == 0) return(data)

  if (identical(transform, "yoy")) {
    data %>%
      group_by(city) %>%
      arrange(date) %>%
      mutate(value = 100 * (value / lag(value, 4) - 1)) %>%
      filter(!is.na(value)) %>%
      ungroup()
  } else if (identical(transform, "index")) {
    data %>%
      group_by(city) %>%
      arrange(date) %>%
      mutate(value = 100 * value / first(value)) %>%
      ungroup()
  } else {
    data
  }
}

build_dwelling_price_plot <- function(data, transform = c("levels", "yoy", "index"),
                                      dark = FALSE) {
  transform <- match.arg(transform)
  y_lab <- switch(transform,
                  levels = "Index",
                  yoy = "YoY %",
                  index = "Index (start=100)")

  ggplot(data, aes(x = date, y = value, color = city)) +
    geom_line(linewidth = 1, alpha = 0.9) +
    scale_color_manual(values = city_colours, na.value = "grey50") +
    scale_x_date(date_labels = "%Y", date_breaks = "3 years") +
    scale_y_continuous(labels = label_number(big.mark = ",", accuracy = 0.1)) +
    labs(x = NULL, y = y_lab, color = NULL) +
    theme_afford(dark)
}

rent_cpi_series_transform <- function(data, data_type = c("index", "yoy", "qoq")) {
  data_type <- match.arg(data_type)
  if (nrow(data) == 0) return(data)

  if (identical(data_type, "yoy")) {
    data %>%
      group_by(city) %>%
      arrange(date) %>%
      mutate(value = 100 * (value / lag(value, 4) - 1)) %>%
      filter(!is.na(value)) %>%
      ungroup()
  } else if (identical(data_type, "qoq")) {
    data %>%
      group_by(city) %>%
      arrange(date) %>%
      mutate(value = 100 * (value / lag(value, 1) - 1)) %>%
      filter(!is.na(value)) %>%
      ungroup()
  } else {
    data
  }
}

build_rent_cpi_plot <- function(data, data_type = c("index", "yoy", "qoq"),
                                dark = FALSE) {
  data_type <- match.arg(data_type)
  y_lab <- switch(data_type,
                  index = "Index",
                  yoy = "Annual change (%)",
                  qoq = "Quarterly change (%)")

  datatype_label <- switch(data_type,
                           index = "index numbers",
                           yoy = "annual change (%)",
                           qoq = "quarterly change (%)")

  date_range_label <- paste(
    format(min(data$date), "%b %Y"), "to", format(max(data$date), "%b %Y"))

  ggplot(data, aes(x = date, y = value, color = city)) +
    geom_line(linewidth = 1, alpha = 0.9) +
    scale_color_manual(values = city_colours, na.value = "grey50") +
    scale_x_date(date_labels = "%Y", date_breaks = "3 years") +
    scale_y_continuous(labels = label_number(big.mark = ",", accuracy = 0.1)) +
    labs(
      x = NULL,
      y = y_lab,
      color = NULL,
      title = paste0("Rent CPI, ", datatype_label,
                     ", by greater capital city, ", date_range_label)
    ) +
    theme_afford(dark)
}

build_context_rates_plot <- function(data, dark = FALSE) {
  rate_colours <- c(
    "RBA Cash Rate" = "#1B5E20",
    "Owner-occ Variable (Discounted)" = "#2196F3",
    "Owner-occ 3yr Fixed" = "#1565C0",
    "Investor Variable (Discounted)" = "#FF9800",
    "Investor 3yr Fixed" = "#E65100"
  )

  ggplot(data, aes(x = date, y = value, color = series)) +
    geom_line(linewidth = 1, alpha = 0.9) +
    scale_color_manual(values = rate_colours) +
    scale_x_date(date_labels = "%Y", date_breaks = "5 years") +
    scale_y_continuous(labels = label_number(big.mark = ",", accuracy = 0.1)) +
    labs(x = NULL, y = "%", color = NULL) +
    theme_afford(dark)
}

build_context_labour_plot <- function(data, dark = FALSE) {
  labour_fills <- c(
    "Unemployment Rate" = "#2196F3",
    "Underemployment Rate" = "#AB47BC",
    "Labour Underutilisation Rate" = "#78909C"
  )

  data <- data %>%
    mutate(series = factor(
      series,
      levels = c("Labour Underutilisation Rate", "Underemployment Rate",
                 "Unemployment Rate")
    ))

  ggplot(data, aes(x = date, y = value, fill = series)) +
    geom_area(alpha = 0.6, linewidth = 0.5, colour = "white") +
    scale_fill_manual(values = labour_fills) +
    scale_x_date(date_labels = "%Y", date_breaks = "5 years") +
    scale_y_continuous(labels = label_percent(scale = 1, accuracy = 0.1)) +
    labs(x = NULL, y = NULL, fill = NULL) +
    theme_afford(dark)
}

build_context_population_plot <- function(data, dark = FALSE) {
  ggplot(data, aes(x = date, y = value)) +
    geom_col(fill = "#29B6F6", alpha = 0.85, width = 60) +
    scale_x_date(date_labels = "%Y", date_breaks = "5 years") +
    scale_y_continuous(labels = label_number(big.mark = ",", accuracy = 1)) +
    labs(x = NULL, y = "Thousands") +
    theme_afford(dark)
}

build_supply_approvals_plot <- function(data, title, dark = FALSE) {
  ggplot(data, aes(x = date, y = value, color = approval_label)) +
    geom_line(linewidth = 0.8, alpha = 0.85) +
    scale_x_date(date_labels = "%Y", date_breaks = "5 years") +
    scale_y_continuous(labels = label_number(big.mark = ",")) +
    labs(
      x = NULL,
      y = "Number of Dwellings",
      color = NULL,
      title = title
    ) +
    theme_afford(dark) +
    theme(legend.position = "bottom")
}

build_supply_construction_cpi_plot <- function(data, dark = FALSE) {
  ggplot(data, aes(x = date, y = value)) +
    geom_line(linewidth = 1, color = "#0E5A8A") +
    scale_x_date(date_labels = "%Y", date_breaks = "5 years") +
    labs(x = NULL, y = "Index") +
    theme_afford(dark)
}

build_rental_stress_state_plot <- function(data, national_value = NULL,
                                           dark = FALSE) {
  reference_layer <- NULL
  if (!is.null(national_value) && length(national_value) > 0 &&
      is.finite(national_value[[1]])) {
    reference_layer <- geom_hline(
      yintercept = national_value[[1]],
      linetype = "dashed",
      color = semantic_colour("reference")
    )
  }

  ggplot(data, aes(x = reorder(geography, -value), y = value,
                   text = hover_text)) +
    geom_col(fill = semantic_colour("worse"), alpha = 0.85, width = 0.7) +
    geom_errorbar(
      data = data %>%
        filter(!is.na(estimate_lower_95), !is.na(estimate_upper_95)),
      aes(ymin = estimate_lower_95, ymax = estimate_upper_95),
      inherit.aes = TRUE,
      width = 0.22,
      linewidth = 0.75,
      color = if (dark) "#F8FAFC" else "#172033",
      alpha = 0.9
    ) +
    geom_text(
      data = data %>% filter(nzchar(reliability_marker)),
      aes(x = reorder(geography, -value), y = value,
          label = reliability_marker),
      inherit.aes = FALSE,
      vjust = -0.45,
      size = 4.2,
      fontface = "bold",
      color = semantic_colour("caution")
    ) +
    reference_layer +
    scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
    labs(x = NULL, y = "% in Rental Stress (>30% of income)") +
    theme_afford(dark)
}

build_rental_stress_trend_plot <- function(data, dark = FALSE) {
  rental_stress_cols <- rental_stress_gradient_colours()

  ggplot(data, aes(x = survey_year, y = geography, fill = value,
                   text = hover_text)) +
    geom_tile(color = if (dark) "#1B2A44" else "#FFFFFF", linewidth = 1.5) +
    geom_text(aes(label = tile_label, color = tile_text_colour),
              size = 2.35, fontface = "bold", show.legend = FALSE) +
    scale_x_discrete(
      breaks = function(x) x[seq(1, length(x), by = 3)],
      labels = function(x) sub("-.*", "", x)
    ) +
    scale_color_identity() +
    scale_fill_gradient2(
      low = rental_stress_cols[["low"]],
      mid = rental_stress_cols[["mid"]],
      high = rental_stress_cols[["high"]],
      midpoint = 40, limits = c(10, 60),
      name = "% in Stress"
    ) +
    labs(x = NULL, y = NULL) +
    theme_afford(dark) +
    theme(
      panel.grid = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
}

build_rental_affordability_index_plot <- function(data, dark = FALSE) {
  ggplot(data, aes(x = date, y = value)) +
    geom_line(linewidth = 1, color = semantic_colour("worse")) +
    scale_x_date(date_labels = "%Y", date_breaks = "5 years") +
    labs(x = NULL, y = "Index (CPI Rents / WPI)") +
    theme_afford(dark)
}

build_rental_costs_demographic_plot <- function(data, dark = FALSE) {
  ggplot(data, aes(x = breakdown_label, y = value, fill = tenure_label)) +
    geom_col(position = "dodge", alpha = 0.85) +
    scale_y_continuous(
      labels = label_number(big.mark = ","),
      breaks = scales::breaks_width(250),
      minor_breaks = NULL
    ) +
    labs(x = NULL, y = "Mean Weekly Rent ($)", fill = NULL) +
    coord_flip() +
    theme_afford(dark)
}

build_geo_state_trend_plot <- function(data, metric_label, axis_label,
                                       value_labels, dark = FALSE) {
  ggplot(data, aes(x = survey_year, y = value,
                   color = geography, group = geography)) +
    geom_line(linewidth = 0.9, alpha = 0.9) +
    geom_point(size = 1.6, alpha = 0.85) +
    scale_y_continuous(labels = value_labels) +
    labs(x = NULL, y = axis_label, color = NULL, title = metric_label) +
    theme_afford(dark) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "bottom")
}

build_geo_state_latest_plot <- function(data, latest_year, axis_label,
                                        value_labels, dark = FALSE) {
  ggplot(data, aes(x = reorder(geography, value), y = value)) +
    geom_col(fill = "#0E5A8A", alpha = 0.85, width = 0.7) +
    coord_flip() +
    scale_y_continuous(labels = value_labels) +
    labs(x = NULL, y = axis_label,
         title = paste("Latest survey year:", latest_year)) +
    theme_afford(dark)
}

build_geo_lower_income_plot <- function(data, national_value = NULL,
                                        metric_label, axis_label,
                                        value_labels, dark = FALSE) {
  reference_layer <- NULL
  if (!is.null(national_value) && length(national_value) > 0 &&
      is.finite(national_value[[1]])) {
    reference_layer <- geom_hline(
      yintercept = national_value[[1]],
      linetype = "dashed",
      color = "#333333"
    )
  }

  ggplot(data, aes(x = reorder(geography, value), y = value,
                   text = hover_text)) +
    geom_col(fill = "#1F9D8C", alpha = 0.85, width = 0.7) +
    geom_errorbar(
      data = data %>%
        filter(!is.na(estimate_lower_95), !is.na(estimate_upper_95)),
      aes(ymin = estimate_lower_95, ymax = estimate_upper_95),
      inherit.aes = TRUE,
      width = 0.22,
      linewidth = 0.75,
      color = if (dark) "#F8FAFC" else "#172033",
      alpha = 0.9
    ) +
    geom_text(
      data = data %>% filter(nzchar(reliability_marker)),
      aes(x = reorder(geography, value), y = value,
          label = reliability_marker),
      inherit.aes = FALSE,
      hjust = -0.35,
      size = 4.2,
      fontface = "bold",
      color = "#FFB74D"
    ) +
    reference_layer +
    coord_flip() +
    scale_y_continuous(labels = value_labels,
                       expand = expansion(mult = c(0, 0.12))) +
    labs(x = NULL, y = axis_label, title = metric_label) +
    theme_afford(dark)
}

build_geo_gcc_comparison_plot <- function(data, metric_label, axis_label,
                                          value_labels, dark = FALSE) {
  ggplot(data, aes(x = reorder(geography, value), y = value)) +
    geom_col(fill = "#C44E52", alpha = 0.85, width = 0.7) +
    coord_flip() +
    scale_y_continuous(labels = value_labels) +
    labs(x = NULL, y = axis_label, title = metric_label) +
    theme_afford(dark)
}

overview_price_series_transform <- function(data,
                                            transform = c("nominal", "index")) {
  transform <- match.arg(transform)
  if (nrow(data) == 0) return(data)

  if (identical(transform, "index")) {
    data %>%
      group_by(city) %>%
      arrange(date) %>%
      mutate(plot_value = 100 * value / first(value)) %>%
      ungroup()
  } else {
    data %>% mutate(plot_value = value * 1000)
  }
}

build_overview_median_prices_plot <- function(data, is_index, price_colours,
                                              show_cities, dark = FALSE) {
  y_scale <- if (is_index) {
    scale_y_continuous(labels = label_number(big.mark = ",", accuracy = 0.1))
  } else {
    scale_y_continuous(labels = label_dollar(prefix = "$", suffix = "k",
                                             scale = 1/1000, big.mark = ","))
  }

  ggplot(data, aes(x = date, y = plot_value, color = city)) +
    geom_line(aes(linetype = city), linewidth = 1.1, alpha = 0.9) +
    scale_color_manual(values = price_colours) +
    scale_linetype_manual(
      values = setNames(
        ifelse(show_cities == "National Avg", "dashed", "solid"),
        show_cities
      )
    ) +
    scale_x_date(date_labels = "%Y", date_breaks = "3 years") +
    y_scale +
    labs(x = NULL, y = NULL, color = NULL, linetype = NULL) +
    theme_afford(dark) +
    theme(legend.position = "none")
}

build_overview_affordability_plot <- function(data, colours, dark = FALSE) {
  latest_vals <- data %>%
    group_by(indicator_label) %>%
    filter(date == max(date)) %>%
    ungroup()

  ggplot(data, aes(x = date, y = value, color = indicator_label)) +
    geom_line(linewidth = 1.1, alpha = 0.9) +
    geom_point(data = latest_vals, size = 3) +
    scale_color_manual(values = colours) +
    scale_x_date(date_labels = "%Y", date_breaks = "3 years") +
    scale_y_continuous(labels = label_number(big.mark = ",", accuracy = 0.1)) +
    labs(x = NULL, y = "Index value", color = NULL) +
    theme_afford(dark)
}

build_affordability_indices_plot <- function(data, dark = FALSE) {
  ggplot(data, aes(x = date, y = value, color = indicator_label)) +
    geom_line(linewidth = 1) +
    facet_wrap(~indicator_label, scales = "free_y") +
    scale_color_manual(values = cost_pressure_palette()) +
    scale_x_date(date_labels = "%Y", date_breaks = "5 years") +
    labs(x = NULL, y = NULL, color = NULL) +
    theme_afford(dark) +
    theme(legend.position = "none")
}

build_market_entry_serviceability_plot <- function(data, dark = FALSE) {
  ggplot(data, aes(x = date, y = serviceability_pct,
                   color = scenario)) +
    geom_line(linewidth = 1.1) +
    geom_hline(yintercept = 30, linetype = "dashed",
               color = semantic_colour("caution"), linewidth = 0.8) +
    annotate("text", x = max(data$date) - 2500, y = 31,
             label = "30% stress reference",
             color = semantic_colour("caution"), size = 3.5,
             hjust = 0, vjust = 0) +
    scale_color_manual(values = c(
      "Nominal rate" = semantic_colour("categorical_navy"),
      "Assessed rate" = semantic_colour("worse")
    )) +
    scale_x_date(date_labels = "%Y", date_breaks = "3 years") +
    scale_y_continuous(labels = label_percent(scale = 1, accuracy = 0.1)) +
    labs(x = NULL, y = NULL, color = NULL) +
    theme_afford(dark)
}

build_housing_stress_bands_plot <- function(data, dark = FALSE) {
  ggplot(data, aes(x = breakdown_val, y = value, fill = stress_band,
                   text = hover_text)) +
    geom_col(position = "stack", alpha = 0.9) +
    geom_text(
      data = data %>% filter(nzchar(reliability_marker)),
      aes(label = reliability_marker),
      position = position_stack(vjust = 0.5),
      size = 4,
      fontface = "bold",
      color = "#172033",
      show.legend = FALSE
    ) +
    scale_fill_manual(values = stress_band_palette()) +
    labs(x = NULL, y = "% of Households", fill = "Cost/Income") +
    coord_flip() +
    theme_afford(dark)
}

build_cost_burden_heatmap_plot <- function(data, dark = FALSE) {
  burden_cols <- burden_gradient_colours()

  ggplot(data, aes(x = tenure_label, y = breakdown_val, fill = value)) +
    geom_tile(color = "white", linewidth = 0.5) +
    geom_text(aes(label = round(value, 1)), size = 3.5) +
    scale_fill_gradient2(low = burden_cols[["low"]],
                         mid = burden_cols[["mid"]],
                         high = burden_cols[["high"]],
                         midpoint = 25, name = "Cost/Income %") +
    labs(x = NULL, y = NULL) +
    theme_afford(dark) +
    theme(axis.text.x = element_text(angle = 30, hjust = 1))
}

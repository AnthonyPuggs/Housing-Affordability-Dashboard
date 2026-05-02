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

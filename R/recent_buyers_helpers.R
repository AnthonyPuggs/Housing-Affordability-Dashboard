# Helpers for SIH File 9 recent home buyer evidence.

recent_buyers_metric_map <- function() {
  data.frame(
    metric = c(
      "Mean value of dwelling",
      "Mean amount of mortgage outstanding",
      "Median amount of mortgage outstanding",
      "Mean equity in dwelling",
      "Median equity in dwelling",
      "Median housing costs per week",
      "Median ratio of housing costs to gross household income",
      "Estimated number of households"
    ),
    metric_id = c(
      "mean_dwelling_value",
      "mean_mortgage_outstanding",
      "median_mortgage_outstanding",
      "mean_equity",
      "median_equity",
      "weekly_housing_cost",
      "cost_income_ratio",
      "households"
    ),
    metric_label = c(
      "Mean dwelling value",
      "Mean mortgage outstanding",
      "Median mortgage outstanding",
      "Mean equity",
      "Median equity",
      "Median weekly housing costs",
      "Median cost-to-income ratio",
      "Estimated households"
    ),
    unit_label = c("$000", "$000", "$000", "$000", "$000", "$/week", "%", "000"),
    display_order = seq_len(8),
    stringsAsFactors = FALSE
  )
}

recent_buyers_metric_choices <- function(data = NULL) {
  metric_map <- recent_buyers_metric_map()
  if (!is.null(data) && is.data.frame(data) && "metric_id" %in% names(data)) {
    metric_map <- metric_map %>% filter(metric_id %in% unique(data$metric_id))
  }
  stats::setNames(metric_map$metric_id, metric_map$metric_label)
}

recent_buyers_format_value <- function(value, metric_id) {
  if (metric_id %in% c("mean_dwelling_value", "mean_mortgage_outstanding",
                       "median_mortgage_outstanding", "mean_equity",
                       "median_equity")) {
    return(fmt_dollar_k(value * 1000))
  }
  if (identical(metric_id, "weekly_housing_cost")) {
    return(fmt_dollar(value))
  }
  if (identical(metric_id, "cost_income_ratio")) {
    return(fmt_pct(value, 0.1))
  }
  if (identical(metric_id, "households")) {
    return(paste0(fmt_number(value), "k"))
  }
  fmt_number(value)
}

normalise_recent_buyers <- function(data) {
  if (!is.data.frame(data) || nrow(data) == 0) {
    return(data.frame())
  }

  metric_map <- recent_buyers_metric_map()
  buyer_labels <- c(
    first_home = "First-home buyers",
    changeover = "Changeover buyers",
    all_recent = "All recent buyers"
  )
  dwelling_labels <- c(
    new = "New dwellings",
    established = "Established dwellings",
    total = "Total dwellings"
  )

  data %>%
    mutate(.row_id = row_number()) %>%
    filter(
      metric %in% metric_map$metric,
      grepl("^buyer_(first_home|changeover|all_recent)_(new|established|total)$",
            breakdown_var)
    ) %>%
    group_by(survey_year, metric, breakdown_var, geography) %>%
    arrange(.row_id, .by_group = TRUE) %>%
    slice(1) %>%
    ungroup() %>%
    mutate(
      buyer_type = sub("^buyer_(.*)_(new|established|total)$", "\\1",
                       breakdown_var),
      dwelling_type = sub("^buyer_(.*)_(new|established|total)$", "\\2",
                          breakdown_var)
    ) %>%
    left_join(metric_map, by = "metric") %>%
    mutate(
      buyer_type_label = unname(buyer_labels[buyer_type]),
      dwelling_type_label = unname(dwelling_labels[dwelling_type]),
      formatted_value = vapply(
        seq_len(n()),
        function(i) recent_buyers_format_value(value[[i]], metric_id[[i]]),
        character(1)
      ),
      measure_class = "official_survey",
      source = "ABS SIH File 9"
    ) %>%
    arrange(display_order, buyer_type, dwelling_type) %>%
    select(-.row_id)
}

recent_buyers_summary <- function(data) {
  if (!is.data.frame(data) || nrow(data) == 0) {
    return(data.frame())
  }

  wanted <- data.frame(
    summary_id = c(
      "first_home_mean_value",
      "first_home_mortgage",
      "first_home_weekly_cost",
      "first_home_cost_income_ratio"
    ),
    metric_id = c(
      "mean_dwelling_value",
      "mean_mortgage_outstanding",
      "weekly_housing_cost",
      "cost_income_ratio"
    ),
    title = c(
      "First-home mean dwelling value",
      "First-home mean mortgage",
      "First-home weekly housing cost",
      "First-home cost-to-income"
    ),
    stringsAsFactors = FALSE
  )

  data %>%
    filter(buyer_type == "first_home", dwelling_type == "total") %>%
    inner_join(wanted, by = "metric_id") %>%
    arrange(match(summary_id, wanted$summary_id)) %>%
    transmute(
      metric_id = summary_id,
      title,
      value,
      formatted_value,
      subtitle = paste0(
        "2019-20 SIH recent first-home buyer households; ",
        metric_label,
        "."
      ),
      source,
      measure_class
    )
}

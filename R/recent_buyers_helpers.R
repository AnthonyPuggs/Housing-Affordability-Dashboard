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

# Household profile dimensions published in SIH File 9 as proportions of
# recent buyer households (each dimension sums to ~100% per buyer type).
recent_buyers_profile_map <- function() {
  data.frame(
    profile_value = c(
      "15 to 24", "25 to 34", "35 to 44", "45 to 54", "55 to 64",
      "65 and over",
      "25% or less", "More than 25% to 30%", "More than 30% to 50%",
      "More than 50%",
      "Couple family with dependent children", "Couple only",
      "One parent family with dependent children", "Other one family households",
      "Lone person households", "Group households",
      "Multiple family households",
      "Lowest quintile", "Second quintile", "Third quintile",
      "Fourth quintile", "Highest quintile",
      "Separate house", "Semi-detached, row or terrace house, townhouse",
      "Flat or apartment"
    ),
    profile_dimension = c(
      rep("age_band", 6),
      rep("cost_income_band", 4),
      rep("family_type", 7),
      rep("income_quintile", 5),
      rep("dwelling_structure", 3)
    ),
    profile_order = c(
      seq_len(6), seq_len(4), seq_len(7), seq_len(5), seq_len(3)
    ),
    stringsAsFactors = FALSE
  )
}

recent_buyers_profile_choices <- function() {
  c(
    "Age of reference person" = "age_band",
    "Housing cost-to-income band" = "cost_income_band",
    "Family type" = "family_type",
    "Equivalised income quintile" = "income_quintile",
    "Dwelling structure" = "dwelling_structure"
  )
}

recent_buyers_profile_dimension_label <- function(dimension) {
  choices <- recent_buyers_profile_choices()
  labels <- stats::setNames(names(choices), unname(choices))
  out <- unname(labels[dimension])
  out[is.na(out)] <- dimension[is.na(out)]
  out
}

# Proportion-of-households profile rows by buyer type (dwelling type = total).
# Like normalise_recent_buyers(), keeps the first row per key as a workaround
# for the known SIH parser duplicate-row artifact (see pipeline ratchet gate).
normalise_recent_buyers_profile <- function(data) {
  if (!is.data.frame(data) || nrow(data) == 0) {
    return(data.frame())
  }

  profile_map <- recent_buyers_profile_map()
  buyer_labels <- c(
    first_home = "First-home buyers",
    changeover = "Changeover buyers",
    all_recent = "All recent buyers"
  )

  data %>%
    mutate(.row_id = row_number()) %>%
    filter(
      stat_type == "proportion",
      metric %in% profile_map$profile_value,
      grepl("^buyer_(first_home|changeover|all_recent)_total$", breakdown_var)
    ) %>%
    group_by(survey_year, metric, breakdown_var, geography) %>%
    arrange(.row_id, .by_group = TRUE) %>%
    slice(1) %>%
    ungroup() %>%
    mutate(
      buyer_type = sub("^buyer_(.*)_total$", "\\1", breakdown_var),
      buyer_type_label = unname(buyer_labels[buyer_type])
    ) %>%
    left_join(profile_map, by = c("metric" = "profile_value")) %>%
    mutate(
      profile_label = metric,
      measure_class = "official_survey",
      source = "ABS SIH File 9"
    ) %>%
    arrange(profile_dimension, profile_order, buyer_type) %>%
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

repo_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
failures <- character()

check <- function(condition, message) {
  if (!isTRUE(condition)) {
    failures <<- c(failures, message)
  }
}

helper_path <- file.path(repo_root, "R", "recent_buyers_helpers.R")
check(file.exists(helper_path), "R/recent_buyers_helpers.R does not exist")

if (file.exists(helper_path)) {
  suppressPackageStartupMessages({
    library(dplyr)
    library(scales)
  })
  source(file.path(repo_root, "R", "dashboard_formatting.R"))
  source(helper_path)

  required_functions <- c(
    "normalise_recent_buyers",
    "recent_buyers_metric_choices",
    "recent_buyers_summary"
  )
  missing_functions <- required_functions[
    !vapply(required_functions, exists, logical(1), mode = "function")
  ]
  check(length(missing_functions) == 0,
        paste("Missing recent-buyer helper functions:",
              paste(missing_functions, collapse = ", ")))

  raw <- data.frame(
    survey_year = "2019-20",
    value = c(696.3, 3.0, 489, 2.5, 15.5, 1.2, 384.4, 3.5),
    metric = c(
      "Mean value of dwelling",
      "Mean value of dwelling",
      "Median housing costs per week",
      "Median housing costs per week",
      "Median ratio of housing costs to gross household income",
      "Median ratio of housing costs to gross household income",
      "Mean amount of mortgage outstanding",
      "Mean amount of mortgage outstanding"
    ),
    tenure = "owner_mortgage",
    breakdown_var = c(
      "buyer_first_home_total",
      "buyer_first_home_total",
      "buyer_first_home_total",
      "buyer_first_home_total",
      "buyer_first_home_total",
      "buyer_first_home_total",
      "buyer_all_recent_total",
      "buyer_all_recent_total"
    ),
    breakdown_val = c(
      "Mean value of dwelling",
      "Mean value of dwelling",
      "Median housing costs per week",
      "Median housing costs per week",
      "Median ratio of housing costs to gross household income",
      "Median ratio of housing costs to gross household income",
      "Mean amount of mortgage outstanding",
      "Mean amount of mortgage outstanding"
    ),
    geography = "National",
    stat_type = c("count", "count", "dollars", "count",
                  "proportion", "count", "count", "count"),
    stringsAsFactors = FALSE
  )

  normalised <- normalise_recent_buyers(raw)
  check(nrow(normalised) == 4,
        "normalise_recent_buyers() must keep one estimate row per metric and buyer segment")
  check(all(c("buyer_type", "buyer_type_label", "dwelling_type",
              "dwelling_type_label", "metric_id", "metric_label",
              "unit_label", "formatted_value") %in% names(normalised)),
        "normalise_recent_buyers() missing expected columns")
  check(all(c("first_home", "all_recent") %in% normalised$buyer_type),
        "normalise_recent_buyers() must parse buyer_type from breakdown_var")
  check(all(normalised$dwelling_type == "total"),
        "normalise_recent_buyers() must parse dwelling_type from breakdown_var")
  check(normalised$value[normalised$metric_id == "mean_dwelling_value"] == 696.3,
        "Mean dwelling value must retain the estimate row, not the RSE row")

  choices <- recent_buyers_metric_choices(normalised)
  check("Mean dwelling value" %in% names(choices),
        "recent_buyers_metric_choices() must expose clear display labels")
  check(unname(choices[["Mean dwelling value"]]) == "mean_dwelling_value",
        "recent_buyers_metric_choices() must map labels to stable metric IDs")

  summary <- recent_buyers_summary(normalised)
  check(all(c("first_home_mean_value", "first_home_weekly_cost",
              "first_home_cost_income_ratio") %in% summary$metric_id),
        "recent_buyers_summary() must expose first-home buyer headline metrics")
  check(summary$formatted_value[
    summary$metric_id == "first_home_cost_income_ratio"
  ] == "15.5%",
  "recent_buyers_summary() must format cost-to-income ratios as percentages")
}

if (length(failures) > 0) {
  stop(
    paste(c("Recent buyers helper checks failed:",
            paste0("- ", failures)), collapse = "\n"),
    call. = FALSE
  )
}

cat("Recent buyers helper checks passed.\n")

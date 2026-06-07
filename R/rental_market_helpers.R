# Rental-market helper functions for SIH demographic rental measures.

rental_cost_measure_choices <- function() {
  c("Weekly rent ($)" = "weekly_rent",
    "Rent-to-income ratio (%)" = "rent_to_income")
}

rental_cost_measure_source_note <- function(measure = c("weekly_rent",
                                                        "rent_to_income")) {
  measure <- match.arg(measure)
  if (identical(measure, "weekly_rent")) {
    paste(
      "ABS Survey of Income and Housing official SIH survey estimate.",
      "Nominal weekly rent dollars describe cost levels, not affordability by themselves.",
      "Use the rent-to-income ratio option for the gross-income burden measure."
    )
  } else {
    paste(
      "ABS Survey of Income and Housing official SIH survey estimate.",
      "Rent-to-income ratio is mean rent as a share of gross income for the household.",
      "Interpret with SIH sampling uncertainty where reliability metadata are available."
    )
  }
}

rental_filter_renter_rows <- function(data, breakdown) {
  if (nrow(data) == 0) {
    return(data)
  }
  data[data$tenure %in% c("renter_private", "renter_total") &
         data$breakdown_var == breakdown &
         data$stat_type == "mean" &
         data$breakdown_val != "Total", , drop = FALSE]
}

rental_demographic_measure_data <- function(measure = c("weekly_rent",
                                                        "rent_to_income"),
                                            breakdown,
                                            sih_costs,
                                            sih_cost_ratios,
                                            sih_quality = NULL) {
  measure <- match.arg(measure)

  if (identical(measure, "weekly_rent")) {
    d <- rental_filter_renter_rows(sih_costs, breakdown)
    measure_label <- "Weekly rent ($)"
    axis_label <- "Mean weekly rent ($)"
    feature_id <- "rental_market_weekly_rent"
  } else {
    d <- rental_filter_renter_rows(sih_cost_ratios, breakdown)
    if ("metric" %in% names(d)) {
      d <- d[d$metric == "cost_income_ratio", , drop = FALSE]
    }
    measure_label <- "Rent-to-income ratio (%)"
    axis_label <- "Mean rent-to-gross-income ratio (%)"
    feature_id <- "rental_market_rent_to_income"
  }

  if (nrow(d) == 0) {
    return(d)
  }

  if (!is.null(sih_quality) && nrow(sih_quality) > 0 &&
      exists("join_sih_quality", mode = "function", inherits = TRUE)) {
    d <- join_sih_quality(d, sih_quality)
  }

  tenure_label <- if (exists("label_tenure", mode = "function", inherits = TRUE)) {
    label_tenure(d$tenure)
  } else {
    d$tenure
  }
  breakdown_label <- stringr::str_wrap(d$breakdown_val, width = 22)
  source_note <- rental_cost_measure_source_note(measure)

  d$tenure_label <- tenure_label
  d$breakdown_label <- breakdown_label
  d$measure <- measure
  d$measure_label <- measure_label
  d$axis_label <- axis_label
  d$feature_id <- feature_id
  d$source_note <- source_note

  if ("rse_pct" %in% names(d) &&
      exists("sih_reliability_marker", mode = "function", inherits = TRUE)) {
    d$reliability_marker <- sih_reliability_marker(d$rse_reliability_flag)
  } else if (!"reliability_marker" %in% names(d)) {
    d$reliability_marker <- ""
  }

  quality_text <- if ("rse_pct" %in% names(d) &&
                      exists("sih_quality_hover_text",
                             mode = "function", inherits = TRUE)) {
    sih_quality_hover_text(d$rse_pct, d$moe_95, d$rse_reliability_flag)
  } else {
    "SIH reliability metadata not matched for this row."
  }

  value_label <- if (identical(measure, "weekly_rent")) {
    paste0("$", scales::comma(round(d$value)))
  } else {
    paste0(scales::number(d$value, accuracy = 0.1), "%")
  }

  d$hover_text <- paste0(
    d$breakdown_val,
    "<br>Tenure: ", d$tenure_label,
    "<br>", measure_label, ": ", value_label,
    "<br>", quality_text
  )

  d
}

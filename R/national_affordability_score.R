# National Housing Affordability Score v1 helpers.

NATIONAL_AFFORDABILITY_SCORE_VERSION <- "national_affordability_score_v1"
NATIONAL_AFFORDABILITY_SCORE_START_DATE <- as.Date("2012-07-01")

national_affordability_score_weights <- function() {
  data.frame(
    component = c(
      "mortgage_serviceability",
      "rental_entry",
      "deposit_barrier"
    ),
    component_label = c(
      "Mortgage serviceability",
      "Rental entry",
      "Deposit barrier"
    ),
    weight = c(0.40, 0.35, 0.25),
    display_order = c(1L, 2L, 3L),
    stringsAsFactors = FALSE
  )
}

national_affordability_score_inputs <- function() {
  data.frame(
    component = c(
      "mortgage_serviceability",
      "rental_entry",
      "deposit_barrier"
    ),
    input_indicator = c(
      "Mortgage Serviceability Index",
      "Rental Affordability Index",
      "Deposit Gap (Years)"
    ),
    output_indicator = c(
      "Mortgage Serviceability Component Score",
      "Rental Entry Component Score",
      "Deposit Barrier Component Score"
    ),
    stringsAsFactors = FALSE
  )
}

national_affordability_score_indicators <- function() {
  components <- merge(
    national_affordability_score_inputs(),
    national_affordability_score_weights(),
    by = "component",
    sort = FALSE
  )
  components <- components[order(components$display_order), ]

  rbind(
    data.frame(
      indicator = "National Housing Affordability Score",
      component = NA_character_,
      component_label = NA_character_,
      weight = NA_real_,
      display_order = 0L,
      stringsAsFactors = FALSE
    ),
    data.frame(
      indicator = components$output_indicator,
      component = components$component,
      component_label = components$component_label,
      weight = components$weight,
      display_order = components$display_order,
      stringsAsFactors = FALSE
    )
  )
}

national_affordability_score_metadata_note <- function() {
  paste(
    "Modelled national market-entry score. Higher means more affordable.",
    "Combines mortgage serviceability, rental entry pressure and deposit",
    "barriers using fixed v1 weights of 40, 35 and 25 per cent.",
    "Not an official ABS/NHHA statistic or lender assessment."
  )
}

score_winsorise <- function(values, probs = c(0.05, 0.95)) {
  if (length(values) == 0) {
    return(values)
  }
  bounds <- stats::quantile(values, probs = probs, na.rm = TRUE,
                            names = FALSE, type = 7)
  pmin(pmax(values, bounds[[1]]), bounds[[2]])
}

score_percentile_affordability <- function(burden_values) {
  if (length(burden_values) == 0) {
    return(numeric())
  }
  if (length(burden_values) == 1) {
    return(50)
  }

  burden_values <- score_winsorise(burden_values)
  percentile_rank <- (rank(burden_values, ties.method = "average") - 1) /
    (length(burden_values) - 1)
  score <- 100 * (1 - percentile_rank)
  pmin(pmax(score, 0), 100)
}

national_affordability_score_input_wide <- function(
    affordability_indices,
    start_date = NATIONAL_AFFORDABILITY_SCORE_START_DATE,
    complete_only = TRUE) {
  required_columns <- c("date", "value", "indicator")
  missing_columns <- setdiff(required_columns, names(affordability_indices))
  if (length(missing_columns) > 0) {
    stop(
      "affordability_indices is missing columns: ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }

  inputs <- national_affordability_score_inputs()
  data <- affordability_indices[, required_columns]
  data$date <- as.Date(data$date)
  data <- data[!is.na(data$date) & data$date >= as.Date(start_date), ,
               drop = FALSE]
  data <- data[data$indicator %in% inputs$input_indicator, , drop = FALSE]

  component_frames <- lapply(seq_len(nrow(inputs)), function(i) {
    row <- inputs[i, ]
    component_data <- data[data$indicator == row$input_indicator,
                           c("date", "value"), drop = FALSE]
    names(component_data)[names(component_data) == "value"] <- row$component
    component_data
  })

  if (length(component_frames) == 0) {
    return(data.frame(date = as.Date(character()), stringsAsFactors = FALSE))
  }

  wide <- Reduce(function(left, right) {
    merge(left, right, by = "date", all = !complete_only)
  }, component_frames)
  wide[order(wide$date), , drop = FALSE]
}

calculate_national_affordability_score <- function(
    affordability_indices,
    start_date = NATIONAL_AFFORDABILITY_SCORE_START_DATE) {
  required_columns <- c("date", "value", "indicator")
  missing_columns <- setdiff(required_columns, names(affordability_indices))
  if (length(missing_columns) > 0) {
    stop(
      "affordability_indices is missing columns: ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }

  weights <- national_affordability_score_weights()
  inputs <- national_affordability_score_inputs()

  data <- affordability_indices[, required_columns]
  data$date <- as.Date(data$date)
  data <- data[!is.na(data$date) & data$date >= as.Date(start_date), ,
               drop = FALSE]
  data <- data[data$indicator %in% inputs$input_indicator, , drop = FALSE]

  key <- data[, c("date", "indicator")]
  if (nrow(key) > 0 && any(duplicated(key))) {
    stop("affordability_indices has duplicate date-indicator rows for score inputs.",
         call. = FALSE)
  }

  wide <- national_affordability_score_input_wide(
    affordability_indices,
    start_date = start_date,
    complete_only = TRUE
  )

  if (nrow(wide) == 0) {
    return(data.frame(
      date = as.Date(character()),
      value = numeric(),
      indicator = character(),
      geography = character(),
      unit = character(),
      frequency = character(),
      stringsAsFactors = FALSE
    ))
  }

  for (component in weights$component) {
    score_col <- paste0(component, "_score")
    wide[[score_col]] <- score_percentile_affordability(wide[[component]])
  }

  weighted_values <- numeric(nrow(wide))
  for (i in seq_len(nrow(weights))) {
    weighted_values <- weighted_values +
      wide[[paste0(weights$component[[i]], "_score")]] * weights$weight[[i]]
  }
  wide$headline_score <- weighted_values

  component_outputs <- merge(inputs, weights, by = "component", sort = FALSE)
  component_outputs <- component_outputs[order(component_outputs$display_order), ]

  rows <- list(data.frame(
    date = wide$date,
    value = wide$headline_score,
    indicator = "National Housing Affordability Score",
    geography = "National",
    unit = "Score (0-100)",
    frequency = "Quarter",
    stringsAsFactors = FALSE
  ))

  for (i in seq_len(nrow(component_outputs))) {
    output <- component_outputs[i, ]
    rows[[length(rows) + 1L]] <- data.frame(
      date = wide$date,
      value = wide[[paste0(output$component, "_score")]],
      indicator = output$output_indicator,
      geography = "National",
      unit = "Score (0-100)",
      frequency = "Quarter",
      stringsAsFactors = FALSE
    )
  }

  result <- do.call(rbind, rows)
  result[order(result$indicator, result$date), ]
}

national_affordability_score_pairwise_correlations <- function(wide) {
  components <- national_affordability_score_weights()$component
  components <- components[components %in% names(wide)]
  pairs <- utils::combn(components, 2, simplify = FALSE)
  do.call(rbind, lapply(pairs, function(pair) {
    x <- wide[[pair[[1]]]]
    y <- wide[[pair[[2]]]]
    keep <- !is.na(x) & !is.na(y)
    data.frame(
      component_x = pair[[1]],
      component_y = pair[[2]],
      correlation = if (sum(keep) >= 2) {
        stats::cor(x[keep], y[keep])
      } else {
        NA_real_
      },
      observations = sum(keep),
      stringsAsFactors = FALSE
    )
  }))
}

national_affordability_score_latest_contributions <- function(score) {
  indicators <- national_affordability_score_indicators()
  component_metadata <- indicators[!is.na(indicators$component), ,
                                   drop = FALSE]
  component_rows <- score[score$indicator %in% component_metadata$indicator,
                          c("date", "indicator", "value"), drop = FALSE]
  if (nrow(component_rows) == 0) {
    return(data.frame(
      date = as.Date(character()),
      component = character(),
      component_label = character(),
      score = numeric(),
      weight = numeric(),
      contribution_points = numeric(),
      stringsAsFactors = FALSE
    ))
  }

  latest_date <- max(component_rows$date)
  latest <- component_rows[component_rows$date == latest_date, , drop = FALSE]
  latest <- merge(latest, component_metadata, by = "indicator", sort = FALSE)
  latest <- latest[order(latest$display_order), , drop = FALSE]
  data.frame(
    date = latest$date,
    component = latest$component,
    component_label = latest$component_label,
    score = latest$value,
    weight = latest$weight,
    contribution_points = latest$value * latest$weight,
    stringsAsFactors = FALSE
  )
}

national_affordability_score_sensitivity <- function(latest_contributions) {
  components <- national_affordability_score_weights()$component
  if (nrow(latest_contributions) == 0) {
    return(data.frame(
      scenario = character(),
      score = numeric(),
      difference_from_default = numeric(),
      stringsAsFactors = FALSE
    ))
  }

  scores <- stats::setNames(latest_contributions$score,
                            latest_contributions$component)
  scores <- scores[components]
  default_weights <- stats::setNames(
    national_affordability_score_weights()$weight,
    components
  )
  weighted_score <- function(weights) {
    sum(scores[names(weights)] * weights, na.rm = TRUE)
  }
  leave_one_out <- function(component) {
    weights <- default_weights[names(default_weights) != component]
    weights <- weights / sum(weights)
    weights
  }

  scenario_weights <- list(
    default_40_35_25 = default_weights,
    equal_weights = stats::setNames(rep(1 / 3, 3), components),
    ownership_heavy = c(
      mortgage_serviceability = 0.45,
      rental_entry = 0.20,
      deposit_barrier = 0.35
    ),
    rental_heavy = c(
      mortgage_serviceability = 0.25,
      rental_entry = 0.50,
      deposit_barrier = 0.25
    ),
    leave_out_mortgage_serviceability =
      leave_one_out("mortgage_serviceability"),
    leave_out_rental_entry = leave_one_out("rental_entry"),
    leave_out_deposit_barrier = leave_one_out("deposit_barrier")
  )

  default_score <- weighted_score(default_weights)
  arithmetic_scores <- data.frame(
    scenario = names(scenario_weights),
    score = vapply(scenario_weights, weighted_score, numeric(1)),
    stringsAsFactors = FALSE
  )

  geometric_score <- exp(sum(default_weights * log(pmax(scores, 1e-6))))
  sensitivity <- rbind(
    arithmetic_scores,
    data.frame(
      scenario = "geometric_default",
      score = geometric_score,
      stringsAsFactors = FALSE
    )
  )
  sensitivity$difference_from_default <- sensitivity$score - default_score
  sensitivity
}

national_affordability_score_diagnostics <- function(
    affordability_indices,
    start_date = NATIONAL_AFFORDABILITY_SCORE_START_DATE) {
  score <- calculate_national_affordability_score(
    affordability_indices,
    start_date = start_date
  )
  headline <- score[
    score$indicator == "National Housing Affordability Score",
    c("date", "value"),
    drop = FALSE
  ]
  inputs <- national_affordability_score_inputs()
  weights <- national_affordability_score_weights()

  if (nrow(headline) == 0) {
    empty_date <- data.frame(
      start_date = as.Date(start_date),
      latest_date = as.Date(NA),
      score_rows = 0L,
      latest_score = NA_real_,
      stringsAsFactors = FALSE
    )
    return(list(
      sample_window = empty_date,
      component_correlations = data.frame(
        component_x = character(),
        component_y = character(),
        correlation = numeric(),
        observations = integer(),
        stringsAsFactors = FALSE
      ),
      missingness = data.frame(
        component = weights$component,
        input_indicator = inputs$input_indicator,
        available_rows = integer(nrow(inputs)),
        complete_score_rows = integer(nrow(inputs)),
        stringsAsFactors = FALSE
      ),
      latest_contributions = national_affordability_score_latest_contributions(score),
      sensitivity_scores = data.frame(
        scenario = character(),
        score = numeric(),
        difference_from_default = numeric(),
        stringsAsFactors = FALSE
      ),
      interpretation_warning = paste(
        "The score is historical-relative and not an absolute affordability",
        "threshold. A score near 0 or 100 is low or high versus the score",
        "window, not a universal affordability judgement."
      )
    ))
  }

  input_wide <- national_affordability_score_input_wide(
    affordability_indices,
    start_date = start_date,
    complete_only = FALSE
  )
  complete_wide <- national_affordability_score_input_wide(
    affordability_indices,
    start_date = start_date,
    complete_only = TRUE
  )
  latest_contributions <- national_affordability_score_latest_contributions(score)
  sensitivity <- national_affordability_score_sensitivity(latest_contributions)

  missingness <- merge(inputs, weights, by = "component", sort = FALSE)
  missingness <- missingness[order(missingness$display_order), ]
  missingness$available_rows <- vapply(missingness$component, function(component) {
    if (component %in% names(input_wide)) {
      sum(!is.na(input_wide[[component]]))
    } else {
      0L
    }
  }, integer(1))
  missingness$complete_score_rows <- nrow(complete_wide)
  missingness <- missingness[, c(
    "component",
    "component_label",
    "input_indicator",
    "available_rows",
    "complete_score_rows"
  )]

  list(
    sample_window = data.frame(
      start_date = min(headline$date),
      latest_date = max(headline$date),
      score_rows = nrow(headline),
      latest_score = headline$value[which.max(headline$date)],
      stringsAsFactors = FALSE
    ),
    component_correlations =
      national_affordability_score_pairwise_correlations(complete_wide),
    missingness = missingness,
    latest_contributions = latest_contributions,
    sensitivity_scores = sensitivity,
    interpretation_warning = paste(
      "The score is historical-relative and not an absolute affordability",
      "threshold. A score near 0 or 100 is low or high versus the score",
      "window, not a universal affordability judgement."
    )
  )
}

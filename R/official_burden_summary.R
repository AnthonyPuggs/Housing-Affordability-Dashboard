# Official SIH/NHHA burden summaries for the Overview page.

official_empty_summary <- function() {
  data.frame(
    metric_id = character(),
    title = character(),
    value = numeric(),
    formatted_value = character(),
    subtitle = character(),
    source = character(),
    measure_class = character(),
    survey_year = character(),
    stringsAsFactors = FALSE
  )
}

official_pct_row <- function(metric_id, title, row, subtitle, source) {
  value <- if (nrow(row) == 0) NA_real_ else row$value[[1]]
  survey_year <- if (nrow(row) == 0) NA_character_ else row$survey_year[[1]]
  data.frame(
    metric_id = metric_id,
    title = title,
    value = value,
    formatted_value = fmt_pct(value, 0.1),
    subtitle = subtitle,
    source = source,
    measure_class = "official_survey",
    survey_year = survey_year,
    stringsAsFactors = FALSE
  )
}

official_latest_row <- function(data) {
  if (nrow(data) == 0) return(data)
  data %>%
    arrange(desc(survey_year)) %>%
    slice(1)
}

official_prepare_burden_input <- function(data) {
  required_columns <- c(
    "survey_year", "value", "metric", "tenure", "breakdown_var",
    "breakdown_val", "geography", "stat_type"
  )

  if (!is.data.frame(data) || !all(required_columns %in% names(data))) {
    return(data.frame(
      survey_year = character(),
      value = numeric(),
      metric = character(),
      tenure = character(),
      breakdown_var = character(),
      breakdown_val = character(),
      geography = character(),
      stat_type = character(),
      stringsAsFactors = FALSE
    ))
  }

  data
}

official_burden_summary <- function(sih_nhha, sih_stress,
                                    sih_cost_ratios = data.frame()) {
  sih_nhha <- official_prepare_burden_input(sih_nhha)
  sih_stress <- official_prepare_burden_input(sih_stress)

  nhha <- sih_nhha %>%
    filter(
      metric == "pct_rental_stress_over_30",
      tenure == "renter_lower_income",
      geography == "Aust.",
      breakdown_val == "Total"
    ) %>%
    official_latest_row()

  lower_income <- sih_stress %>%
    filter(
      metric == "pct_over_30",
      stat_type == "lower_income",
      geography == "National",
      tenure == "all",
      breakdown_val == "Total",
      is.finite(value),
      value >= 0,
      value <= 100
    ) %>%
    official_latest_row()

  mortgage_owner <- sih_stress %>%
    filter(
      metric == "pct_over_30",
      stat_type == "lower_income",
      geography == "National",
      tenure == "owner_mortgage",
      breakdown_val == "Owner with a mortgage",
      is.finite(value),
      value >= 0,
      value <= 100
    ) %>%
    official_latest_row()

  group_candidates <- sih_stress %>%
    filter(
      metric == "pct_over_30",
      geography == "National",
      breakdown_var %in% c(
        "age_group",
        "family_type",
        "equiv_income_quintile",
        "dwelling_structure",
        "income_source",
        "number_of_employed_persons_in_household"
      ),
      breakdown_val != "Total",
      is.finite(value),
      value >= 0,
      value <= 100
    )
  if ("stat_type" %in% names(group_candidates)) {
    preferred <- group_candidates %>% filter(stat_type == "lower_income")
    if (nrow(preferred) > 0) {
      group_candidates <- preferred
    }
  }
  highest_group <- group_candidates %>%
    arrange(desc(survey_year), desc(value)) %>%
    slice(1)

  highest_subtitle <- if (nrow(highest_group) == 0) {
    "Official SIH burden measure; highest visible household group unavailable."
  } else {
    paste0(
      "Official SIH burden measure; highest visible group: ",
      highest_group$breakdown_val[[1]],
      "."
    )
  }

  bind_rows(
    official_pct_row(
      "nhha_lower_income_renter_stress",
      "Lower-income renter stress",
      nhha,
      "Official NHHA/SIH: lower-income renters (bottom 40% of equivalised income, excluding Rent Assistance) paying more than 30% of income.",
      "ABS SIH File 13"
    ),
    official_pct_row(
      "lower_income_over_30",
      "Lower-income households >30%",
      lower_income,
      "Official SIH: lower-income households (3rd-40th percentile of equivalised income) with housing costs above 30% of income.",
      "ABS SIH File 5"
    ),
    official_pct_row(
      "mortgage_owner_over_30",
      "Mortgage owners >30%",
      mortgage_owner,
      "Official SIH: lower-income (3rd-40th percentile, equivalised) owner-with-mortgage households above 30% of income.",
      "ABS SIH File 5"
    ),
    official_pct_row(
      "highest_stress_group",
      "Highest visible burden group",
      highest_group,
      highest_subtitle,
      "ABS SIH File 5"
    )
  )
}

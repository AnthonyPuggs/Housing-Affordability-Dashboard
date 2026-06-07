repo_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
failures <- character()

check <- function(condition, message) {
  if (!isTRUE(condition)) {
    failures <<- c(failures, message)
  }
}

helper_path <- file.path(repo_root, "R", "rental_market_helpers.R")
chart_builder_path <- file.path(repo_root, "R", "chart_builders.R")
check(file.exists(helper_path), "R/rental_market_helpers.R does not exist")

if (file.exists(helper_path)) {
  parsed <- tryCatch({
    parse(helper_path)
    TRUE
  }, error = function(e) conditionMessage(e))
  check(identical(parsed, TRUE),
        paste(helper_path, "does not parse:", parsed))

  suppressPackageStartupMessages({
    library(dplyr)
    library(ggplot2)
    library(scales)
    library(stringr)
  })
  source(file.path(repo_root, "R", "dashboard_theme.R"))
  source(helper_path)

  check(exists("rental_cost_measure_choices", mode = "function"),
        "rental_cost_measure_choices() must be defined")
  check(exists("rental_demographic_measure_data", mode = "function"),
        "rental_demographic_measure_data() must be defined")
  check(exists("rental_cost_measure_source_note", mode = "function"),
        "rental_cost_measure_source_note() must be defined")

  if (exists("rental_cost_measure_choices", mode = "function")) {
    choices <- rental_cost_measure_choices()
    check(identical(unname(choices), c("weekly_rent", "rent_to_income")),
          "Rental measure selector must expose weekly rent and rent-to-income ratio values")
    check(all(c("Weekly rent ($)", "Rent-to-income ratio (%)") %in% names(choices)),
          "Rental measure selector must use explicit user-facing labels")
  }

  sih_costs <- data.frame(
    tenure = c("renter_private", "renter_total", "owner_mortgage"),
    breakdown_var = "age_group",
    breakdown_val = c("25 to 34", "25 to 34", "25 to 34"),
    stat_type = "mean",
    value = c(520, 480, 700),
    survey_year = "2019-20",
    stringsAsFactors = FALSE
  )
  sih_cost_ratios <- data.frame(
    tenure = c("renter_private", "renter_total", "owner_mortgage"),
    breakdown_var = "age_group",
    breakdown_val = c("25 to 34", "25 to 34", "25 to 34"),
    stat_type = "mean",
    metric = "cost_income_ratio",
    value = c(24.5, 22.0, 18.0),
    survey_year = "2019-20",
    stringsAsFactors = FALSE
  )

  if (exists("rental_demographic_measure_data", mode = "function")) {
    rent <- rental_demographic_measure_data(
      measure = "weekly_rent",
      breakdown = "age_group",
      sih_costs = sih_costs,
      sih_cost_ratios = sih_cost_ratios
    )
    ratio <- rental_demographic_measure_data(
      measure = "rent_to_income",
      breakdown = "age_group",
      sih_costs = sih_costs,
      sih_cost_ratios = sih_cost_ratios
    )

    check(nrow(rent) == 2,
          "Weekly-rent demographic helper must keep renter tenures only")
    check(identical(unique(rent$measure_label), "Weekly rent ($)"),
          "Weekly-rent helper must label the measure explicitly")
    check(identical(unique(rent$axis_label), "Mean weekly rent ($)"),
          "Weekly-rent helper must use a dollar axis label")
    check(nrow(ratio) == 2,
          "Rent-to-income demographic helper must keep renter tenures only")
    check(identical(unique(ratio$measure_label),
                    "Rent-to-income ratio (%)"),
          "Ratio helper must label the measure explicitly")
    check(identical(unique(ratio$axis_label),
                    "Mean rent-to-gross-income ratio (%)"),
          "Ratio helper must use a percentage burden axis label")
    check(any(grepl("official SIH survey", ratio$source_note,
                    ignore.case = TRUE)),
          "Ratio helper must keep official SIH survey source framing")
  }

  if (exists("rental_cost_measure_source_note", mode = "function")) {
    note <- rental_cost_measure_source_note("rent_to_income")
    check(grepl("gross income", note, ignore.case = TRUE),
          "Ratio source note must explain the gross-income denominator")
  }
}

if (file.exists(chart_builder_path) && file.exists(helper_path)) {
  source(chart_builder_path)
  if (exists("build_rental_costs_demographic_plot", mode = "function") &&
      exists("rental_demographic_measure_data", mode = "function")) {
    plot_data <- rental_demographic_measure_data(
      measure = "rent_to_income",
      breakdown = "age_group",
      sih_costs = data.frame(
        tenure = character(),
        breakdown_var = character(),
        breakdown_val = character(),
        stat_type = character(),
        value = numeric()
      ),
      sih_cost_ratios = data.frame(
        tenure = c("renter_private", "renter_total"),
        breakdown_var = "age_group",
        breakdown_val = c("25 to 34", "25 to 34"),
        stat_type = "mean",
        metric = "cost_income_ratio",
        value = c(24.5, 22.0),
        survey_year = "2019-20",
        stringsAsFactors = FALSE
      )
    )
    p <- build_rental_costs_demographic_plot(plot_data, dark = FALSE)
    check(inherits(p, "ggplot"),
          "build_rental_costs_demographic_plot() must return a ggplot")
    check(identical(p$labels$y, "Mean rent-to-gross-income ratio (%)"),
          "Rental demographic chart builder must respect dynamic axis labels")
  }
}

if (length(failures) > 0) {
  stop(
    paste(c("Rental market ratio measure checks failed:", paste0("- ", failures)),
          collapse = "\n"),
    call. = FALSE
  )
}

cat("Rental market ratio measure checks passed.\n")

failures <- character()

check <- function(condition, message) {
  if (!isTRUE(condition)) {
    failures <<- c(failures, message)
  }
}

source(file.path(getwd(), "R", "indicator_registry.R"))
source(file.path(getwd(), "R", "sih_benchmarks.R"))

read_required_csv <- function(filename) {
  path <- file.path(getwd(), "data", filename)
  check(file.exists(path), paste(filename, "does not exist"))
  if (!file.exists(path)) {
    return(data.frame())
  }
  read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
}

required_columns <- function(df, filename, columns) {
  missing <- setdiff(columns, names(df))
  check(
    length(missing) == 0,
    paste(filename, "is missing required columns:", paste(missing, collapse = ", "))
  )
}

duplicate_count <- function(df, keys) {
  if (nrow(df) == 0 || any(!keys %in% names(df))) {
    return(NA_integer_)
  }
  key_frame <- df[keys]
  sum(duplicated(key_frame))
}

abs_ts <- read_required_csv("abs_timeseries.csv")
rba_rates <- read_required_csv("rba_rates.csv")
afford_idx <- read_required_csv("affordability_indices.csv")
sih_nhha <- read_required_csv("sih_nhha_rental_stress.csv")
sih_quality <- read_required_csv("sih_estimate_quality.csv")

sih_estimate_files <- c(
  "sih_costs_2020.csv",
  "sih_cost_ratios_2020.csv",
  "sih_stress_bands_2020.csv",
  "sih_lower_income_states.csv",
  "sih_age_tenure_2020.csv",
  "sih_nhha_rental_stress.csv"
)

required_columns(
  abs_ts,
  "abs_timeseries.csv",
  c("date", "value", "series", "series_id", "category", "unit", "frequency")
)
required_columns(
  rba_rates,
  "rba_rates.csv",
  c("date", "value", "series", "series_id", "category", "unit", "frequency")
)
required_columns(
  afford_idx,
  "affordability_indices.csv",
  c("date", "value", "indicator", "geography", "unit", "frequency")
)
required_columns(
  sih_nhha,
  "sih_nhha_rental_stress.csv",
  c("survey_year", "value", "metric", "tenure", "breakdown_var",
    "breakdown_val", "geography", "stat_type")
)
required_columns(
  sih_quality,
  "sih_estimate_quality.csv",
  c("source_file", "source_table", "survey_year", "metric", "tenure",
    "breakdown_var", "breakdown_val", "geography", "stat_type",
    "quality_measure", "quality_value", "quality_unit",
    "reliability_flag", "reliability_note")
)

required_abs_series <- c(
  indicator_registry_required_abs_sources()
)

if ("series" %in% names(abs_ts)) {
  missing_abs <- setdiff(required_abs_series, unique(abs_ts$series))
  check(
    length(missing_abs) == 0,
    paste("abs_timeseries.csv is missing required series:",
          paste(missing_abs, collapse = ", "))
  )

  rent_cpi_rows <- abs_ts[startsWith(abs_ts$series, "CPI Rents ;"), ]
  if (nrow(rent_cpi_rows) > 0) {
    rent_cpi_rows$date <- as.Date(rent_cpi_rows$date)
    rent_cpi_rows$city <- trimws(vapply(
      strsplit(rent_cpi_rows$series, ";", fixed = TRUE),
      function(parts) parts[[2]],
      character(1)
    ))
    national_city <- "Weighted average of eight capital cities"
    national_rows <- rent_cpi_rows[rent_cpi_rows$city == national_city, ]
    city_rows <- rent_cpi_rows[rent_cpi_rows$city != national_city, ]
    check(nrow(national_rows) > 0,
          "abs_timeseries.csv must include national weighted-average CPI rents")
    check(nrow(city_rows) > 0,
          "abs_timeseries.csv must include capital-city CPI rents")
    if (nrow(national_rows) > 0) {
      check(min(national_rows$date) < as.Date("2012-01-01"),
            "weighted-average CPI rents must retain long-run pre-2012 history")
    }
    if (nrow(city_rows) > 0) {
      city_starts <- aggregate(date ~ city, city_rows, min)
      check(all(city_starts$date == as.Date("2022-07-01")),
            "capital-city CPI rent series must start at the current post-rebase July 2022 boundary")
    }
  }
}

required_rba_series <- indicator_registry_required_rba_sources()
if ("series" %in% names(rba_rates)) {
  missing_rba <- setdiff(required_rba_series, unique(rba_rates$series))
  check(
    length(missing_rba) == 0,
    paste("rba_rates.csv is missing required series:",
          paste(missing_rba, collapse = ", "))
  )
}

if (all(c("indicator", "geography", "date", "value") %in% names(afford_idx))) {
  dup_afford <- duplicate_count(afford_idx, c("indicator", "geography", "date"))
  check(!is.na(dup_afford) && dup_afford == 0, paste("affordability_indices.csv has", dup_afford, "duplicate key rows"))
  check(
    all(is.finite(afford_idx$value)),
    "affordability_indices.csv contains non-finite values"
  )

  indicator_counts <- table(afford_idx$indicator)
  expected_counts <- indicator_registry_minimum_rows()
  for (indicator in names(expected_counts)) {
    actual <- if (indicator %in% names(indicator_counts)) {
      unname(indicator_counts[[indicator]])
    } else {
      0L
    }
    check(
      actual >= expected_counts[[indicator]],
      paste0(indicator, " has ", actual, " rows; expected at least ",
             expected_counts[[indicator]])
    )
  }

  score_indicators <- c(
    "National Housing Affordability Score",
    "Mortgage Serviceability Component Score",
    "Rental Entry Component Score",
    "Deposit Barrier Component Score"
  )
  missing_score_indicators <- setdiff(score_indicators,
                                      unique(afford_idx$indicator))
  check(length(missing_score_indicators) == 0,
        paste("affordability_indices.csv is missing national score rows:",
              paste(missing_score_indicators, collapse = ", ")))
  score_rows <- afford_idx[afford_idx$indicator %in% score_indicators, ]
  if (nrow(score_rows) > 0) {
    check(all(score_rows$unit == "Score (0-100)"),
          "National score rows must use Score (0-100) units")
    check(all(score_rows$value >= 0 & score_rows$value <= 100),
          "National score rows must stay within the 0-100 range")
    score_dates <- split(score_rows$date, score_rows$indicator)
    score_date_sets_match <- length(score_dates) == length(score_indicators) &&
      length(unique(vapply(score_dates, paste, character(1), collapse = "|"))) == 1
    check(score_date_sets_match,
          "Headline and component score rows must use identical date sets")
  }
}

if (all(c("survey_year", "metric", "tenure", "breakdown_var", "breakdown_val",
          "geography", "stat_type") %in% names(sih_nhha))) {
  dup_nhha <- duplicate_count(
    sih_nhha,
    c("survey_year", "metric", "tenure", "breakdown_var",
      "breakdown_val", "geography", "stat_type")
  )
  check(!is.na(dup_nhha) && dup_nhha == 0, paste("sih_nhha_rental_stress.csv has", dup_nhha, "duplicate key rows"))

  required_nhha_metrics <- c(
    "pct_rental_stress_over_30",
    "number_rental_stress_over_30",
    "number_lower_income_renter_households"
  )
  missing_metrics <- setdiff(required_nhha_metrics, unique(sih_nhha$metric))
  check(
    length(missing_metrics) == 0,
    paste("sih_nhha_rental_stress.csv is missing metrics:",
          paste(missing_metrics, collapse = ", "))
  )
}

sih_key_cols <- c(
  "survey_year",
  "metric",
  "tenure",
  "breakdown_var",
  "breakdown_val",
  "geography",
  "stat_type"
)

for (filename in sih_estimate_files) {
  sih_output <- read_required_csv(filename)
  required_columns(sih_output, filename, c(sih_key_cols, "value"))
  if (all(c(sih_key_cols, "value") %in% names(sih_output))) {
    dup_sih <- duplicate_count(sih_output, sih_key_cols)
    check(
      !is.na(dup_sih) && dup_sih == 0,
      paste(filename, "has", dup_sih, "duplicate SIH estimate key rows")
    )
    check(
      all(is.finite(sih_output$value)),
      paste(filename, "contains non-finite estimate values")
    )
  }
}

if (exists("validate_sih_workbook_benchmarks", mode = "function")) {
  benchmark_failures <- validate_sih_workbook_benchmarks(
    data_dir = file.path(getwd(), "data")
  )
  check(
    length(benchmark_failures) == 0,
    paste(c("SIH workbook benchmarks failed:",
            paste0("- ", benchmark_failures)),
          collapse = "\n")
  )
} else {
  check(FALSE, "validate_sih_workbook_benchmarks() is unavailable")
}

if (all(c("source_file", "source_table", "survey_year", "metric", "tenure",
          "breakdown_var", "breakdown_val", "geography", "stat_type",
          "quality_measure", "quality_value", "quality_unit",
          "reliability_flag") %in% names(sih_quality))) {
  dup_quality <- duplicate_count(
    sih_quality,
    c("source_file", "source_table", "survey_year", "metric", "tenure",
      "breakdown_var", "breakdown_val", "geography", "stat_type",
      "quality_measure")
  )
  check(!is.na(dup_quality) && dup_quality == 0,
        paste("sih_estimate_quality.csv has", dup_quality,
              "duplicate key rows"))
  missing_measures <- setdiff(c("moe_95", "rse_pct"),
                              unique(sih_quality$quality_measure))
  check(length(missing_measures) == 0,
        paste("sih_estimate_quality.csv is missing quality measures:",
              paste(missing_measures, collapse = ", ")))
  check(all(is.finite(sih_quality$quality_value)),
        "sih_estimate_quality.csv contains non-finite quality values")
}

if (length(failures) > 0) {
  stop(
    paste(c("Pipeline output checks failed:", paste0("- ", failures)),
          collapse = "\n"),
    call. = FALSE
  )
}

cat("Pipeline output checks passed.\n")

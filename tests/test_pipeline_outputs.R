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
}

required_rba_series <- indicator_registry_required_rba_sources()
if ("series" %in% names(rba_rates)) {
  check(
    required_rba_series %in% unique(rba_rates$series),
    paste("rba_rates.csv is missing required series:", required_rba_series)
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

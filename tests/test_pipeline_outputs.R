failures <- character()

check <- function(condition, message) {
  if (!isTRUE(condition)) {
    failures <<- c(failures, message)
  }
}

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

required_abs_series <- c(
  "RPPI",
  "WPI",
  "CPI All Groups",
  "CPI Inflation YoY",
  "AWE (AWOTE, Persons)",
  "CPI Rents ; Weighted average of eight capital cities ;"
)

if ("series" %in% names(abs_ts)) {
  missing_abs <- setdiff(required_abs_series, unique(abs_ts$series))
  check(
    length(missing_abs) == 0,
    paste("abs_timeseries.csv is missing required series:",
          paste(missing_abs, collapse = ", "))
  )
}

required_rba_series <- "Lending rates; Housing loans; Banks; Variable; Discounted; Owner-occupier"
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
  expected_counts <- c(
    "Real House Price Growth YoY" = 50L,
    "Real Wage Growth YoY" = 80L,
    "Real Mortgage Rate" = 50L
  )
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

if (length(failures) > 0) {
  stop(
    paste(c("Pipeline output checks failed:", paste0("- ", failures)),
          collapse = "\n"),
    call. = FALSE
  )
}

cat("Pipeline output checks passed.\n")

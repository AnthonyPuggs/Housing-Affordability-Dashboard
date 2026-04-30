# ==============================================================================
# 06_validate_outputs.R — Fail-fast validation for generated pipeline outputs
# ==============================================================================

cat("--- Validating pipeline outputs ---\n")

if (!exists("indicator_registry", mode = "function")) {
  source(project_path("R", "indicator_registry.R"))
}

collect_pipeline_failures <- function(data_dir = DATA_DIR) {
  failures <- character()

  add_failure <- function(message) {
    failures <<- c(failures, message)
  }

  check <- function(condition, message) {
    if (!isTRUE(condition)) {
      add_failure(message)
    }
  }

  read_required_csv <- function(filename) {
    path <- file.path(data_dir, filename)
    if (!file.exists(path)) {
      add_failure(paste(filename, "does not exist"))
      return(tibble())
    }
    readr::read_csv(path, show_col_types = FALSE)
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
    df %>%
      count(across(all_of(keys)), name = "n") %>%
      filter(n > 1) %>%
      summarise(duplicates = sum(n - 1), .groups = "drop") %>%
      pull(duplicates) %>%
      replace_na(0L)
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

  required_abs_series <- indicator_registry_required_abs_sources()

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

  rba_raw_files <- Sys.glob(file.path(data_dir, "rba_*_raw.csv"))
  for (raw_file in rba_raw_files) {
    problem_count <- tryCatch(
      rba_csv_parse_problem_count(raw_file),
      error = function(e) conditionMessage(e)
    )
    check(
      identical(problem_count, 0L),
      paste(basename(raw_file), "has", problem_count,
            "readr parse problems")
    )
  }

  if (all(c("indicator", "geography", "date", "value") %in% names(afford_idx))) {
    dup_afford <- duplicate_count(afford_idx, c("indicator", "geography", "date"))
    check(
      !is.na(dup_afford) && dup_afford == 0,
      paste("affordability_indices.csv has", dup_afford, "duplicate key rows")
    )
    check(
      all(is.finite(afford_idx$value)),
      "affordability_indices.csv contains non-finite values"
    )

    indicator_counts <- afford_idx %>%
      count(indicator, name = "n")
    expected_counts <- indicator_registry_minimum_rows()
    for (indicator in names(expected_counts)) {
      actual <- indicator_counts %>%
        filter(.data$indicator == !!indicator) %>%
        pull(n)
      if (length(actual) == 0) {
        actual <- 0L
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
    check(
      !is.na(dup_nhha) && dup_nhha == 0,
      paste("sih_nhha_rental_stress.csv has", dup_nhha, "duplicate key rows")
    )

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

  failures
}

validation_failures <- collect_pipeline_failures(DATA_DIR)

if (length(validation_failures) > 0) {
  stop(
    paste(c("Pipeline validation failed:", paste0("- ", validation_failures)),
          collapse = "\n"),
    call. = FALSE
  )
}

cat("--- Pipeline validation passed ---\n")

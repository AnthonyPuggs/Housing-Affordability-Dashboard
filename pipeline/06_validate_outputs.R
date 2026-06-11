# ==============================================================================
# 06_validate_outputs.R — Fail-fast validation for generated pipeline outputs
# ==============================================================================

cat("--- Validating pipeline outputs ---\n")

if (!exists("indicator_registry", mode = "function")) {
  source(project_path("R", "indicator_registry.R"))
}
if (!exists("validate_sih_workbook_benchmarks", mode = "function")) {
  source(project_path("R", "sih_benchmarks.R"))
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
  abs_supply <- read_required_csv("abs_supply_demand.csv")
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
    missing_rba <- setdiff(required_rba_series, unique(rba_rates$series))
    check(
      length(missing_rba) == 0,
      paste("rba_rates.csv is missing required series:",
            paste(missing_rba, collapse = ", "))
    )
  }

  # Freshness gate: RBA F-tables publish at least monthly, so a latest
  # observation older than 45 days means the refresh is silently serving a
  # stale cache (the failure mode behind the May-June 2026 freeze, when CI
  # checkout reset cache mtimes and the download branch never ran).
  if ("date" %in% names(rba_rates) && nrow(rba_rates) > 0) {
    rba_max_date <- suppressWarnings(max(as.Date(rba_rates$date), na.rm = TRUE))
    rba_age_days <- if (is.finite(rba_max_date)) {
      as.integer(Sys.Date() - rba_max_date)
    } else {
      NA_integer_
    }
    check(
      is.finite(rba_max_date) && rba_age_days <= 45,
      paste0("rba_rates.csv latest observation (", format(rba_max_date),
             ") is ", rba_age_days,
             " days old; the RBA refresh appears stale - check the raw-cache/download path")
    )
  }

  # Range sanity. Interest-rate categories must sit inside (-25, 50) per cent;
  # the Household Finances category carries E2 ratios of debt to annualised
  # disposable income, which run far above 50 per cent, so it gets its own
  # plausibility band instead of being exempted silently.
  if (all(c("value", "category") %in% names(rba_rates)) &&
      nrow(rba_rates) > 0) {
    rate_values <- rba_rates$value[rba_rates$category != "Household Finances"]
    bad_rates <- sum(!is.finite(rate_values) |
                       rate_values < -25 | rate_values > 50)
    check(
      bad_rates == 0,
      paste("rba_rates.csv has", bad_rates,
            "rate values outside the plausible (-25, 50) per cent range")
    )

    dti_values <- rba_rates$value[rba_rates$category == "Household Finances"]
    check(
      length(dti_values) > 0,
      "rba_rates.csv has no Household Finances (RBA E2 debt-to-income) rows"
    )
    bad_dti <- sum(!is.finite(dti_values) |
                     dti_values < 20 | dti_values > 350)
    check(
      bad_dti == 0,
      paste("rba_rates.csv has", bad_dti,
            "household debt-to-income values outside the plausible (20, 350)",
            "per cent range")
    )
  }

  # Quarterly ABS series publish with up to a ~3 month lag; older than 150
  # days means the live fetch is silently failing or serving stale data.
  freshness_gate <- function(df, filename, max_age_days) {
    if (!"date" %in% names(df) || nrow(df) == 0) {
      return(invisible(NULL))
    }
    max_date <- suppressWarnings(max(as.Date(df$date), na.rm = TRUE))
    age_days <- if (is.finite(max_date)) {
      as.integer(Sys.Date() - max_date)
    } else {
      NA_integer_
    }
    check(
      is.finite(max_date) && age_days <= max_age_days,
      paste0(filename, " latest observation (", format(max_date), ") is ",
             age_days, " days old (limit ", max_age_days,
             "); the live fetch appears stale")
    )
  }
  freshness_gate(abs_ts, "abs_timeseries.csv", 150)
  freshness_gate(abs_supply, "abs_supply_demand.csv", 150)

  # abs_supply_demand.csv previously had no validation at all beyond the
  # structural stage gate.
  required_columns(
    abs_supply,
    "abs_supply_demand.csv",
    c("date", "value", "series", "series_id", "category", "unit", "frequency")
  )
  if (all(c("date", "series") %in% names(abs_supply))) {
    dup_supply <- duplicate_count(abs_supply, c("date", "series"))
    check(
      !is.na(dup_supply) && dup_supply == 0,
      paste("abs_supply_demand.csv has", dup_supply, "duplicate (date, series) rows")
    )
    check(
      all(is.finite(abs_supply$value)),
      "abs_supply_demand.csv contains non-finite values"
    )
  }
  if (all(c("date", "series") %in% names(abs_ts))) {
    dup_abs <- duplicate_count(abs_ts, c("date", "series"))
    check(
      !is.na(dup_abs) && dup_abs == 0,
      paste("abs_timeseries.csv has", dup_abs, "duplicate (date, series) rows")
    )
  }

  # Derived indicator range sanity.
  if (all(c("indicator", "value") %in% names(afford_idx))) {
    deposit_gap_values <- afford_idx$value[
      afford_idx$indicator == "Deposit Gap (Years)"
    ]
    check(
      all(deposit_gap_values > 0 & deposit_gap_values < 60),
      "Deposit Gap (Years) has values outside the plausible (0, 60) year range"
    )
    score_values <- afford_idx$value[
      grepl("Score", afford_idx$indicator, fixed = TRUE)
    ]
    check(
      all(score_values >= 0 & score_values <= 100),
      "Score indicators have values outside the 0-100 range"
    )

    indicator_range_gate <- function(indicator, lower, upper) {
      values <- afford_idx$value[afford_idx$indicator == indicator]
      check(
        all(is.finite(values) & values > lower & values < upper),
        paste0(indicator, " has values outside the plausible (", lower,
               ", ", upper, ") range")
      )
    }
    indicator_range_gate("FHB New Loan Commitments", 5000, 120000)
    indicator_range_gate("FHB Average Loan Size", 50000, 2000000)
    indicator_range_gate("Rent CPI Monthly Growth YoY", -10, 25)
    indicator_range_gate("Household Debt to Income Ratio", 20, 350)
  }

  # SIH outputs that previously had structure-only stage-gate checks.
  #
  # KNOWN PARSER ARTIFACT (Track 3 / review PIPE-12): the positional SIH
  # parser emits duplicate key rows for these four files (overlapping panel
  # parses; e.g. a cost-to-income panel lands under a dollar-cost metric, and
  # some cells are emitted more than once). The app works around it with
  # keep-largest-estimate slices (geo_keep_largest_estimate). Until the
  # header-anchored parser rewrite, this gate RATCHETS: duplicate counts may
  # shrink but must never exceed the measured 2026-06 baseline, so a parser
  # regression fails loudly instead of growing silently.
  sih_duplicate_baseline <- c(
    "sih_timeseries_national.csv" = 1936L,
    "sih_state_timeseries.csv" = 15337L,
    "sih_recent_buyers_2020.csv" = 216L,
    "sih_geographic_2020.csv" = 2858L
  )
  sih_key_columns <- c(
    "survey_year", "metric", "tenure", "breakdown_var", "breakdown_val",
    "geography", "stat_type"
  )
  for (sih_file in names(sih_duplicate_baseline)) {
    sih_df <- read_required_csv(sih_file)
    required_columns(
      sih_df, sih_file,
      c("survey_year", "value", "metric", "tenure", "breakdown_var",
        "breakdown_val", "geography", "stat_type")
    )
    if (all(sih_key_columns %in% names(sih_df)) && nrow(sih_df) > 0) {
      dup_sih <- duplicate_count(sih_df, sih_key_columns)
      baseline <- sih_duplicate_baseline[[sih_file]]
      check(
        !is.na(dup_sih) && dup_sih <= baseline,
        paste0(sih_file, " has ", dup_sih,
               " duplicate key rows, above the known parser-artifact baseline of ",
               baseline, " - a parser change has made duplication worse")
      )
      check(
        all(is.finite(sih_df$value)),
        paste(sih_file, "contains non-finite values")
      )
    }
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

  benchmark_failures <- validate_sih_workbook_benchmarks(data_dir = data_dir)
  for (failure in benchmark_failures) {
    add_failure(failure)
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
    check(
      !is.na(dup_quality) && dup_quality == 0,
      paste("sih_estimate_quality.csv has", dup_quality,
            "duplicate key rows")
    )

    missing_quality_measures <- setdiff(
      c("moe_95", "rse_pct"),
      unique(sih_quality$quality_measure)
    )
    check(
      length(missing_quality_measures) == 0,
      paste("sih_estimate_quality.csv is missing quality measures:",
            paste(missing_quality_measures, collapse = ", "))
    )
    check(
      all(is.finite(sih_quality$quality_value)),
      "sih_estimate_quality.csv contains non-finite quality values"
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

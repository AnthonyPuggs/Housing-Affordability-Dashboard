# ==============================================================================
# 03_fetch_rba.R — Fetch RBA statistical tables
# ==============================================================================
# Source:  RBA website (CSV/Excel downloads)
# Output:  data/rba_rates.csv
#
# Tables:
#   F1 — Interest Rates (cash rate)
#   F5 — Indicator Lending Rates (mortgage rates)
#   F6 — Housing Lending Rates
#   E2 — Household Finances: Selected Ratios (debt-to-income context)
#
# Schema: date | value | series | series_id | category | unit | frequency
# ==============================================================================

if (!exists("project_path", mode = "function")) {
  candidates <- c(
    file.path(getwd(), "R", "project_paths.R"),
    file.path(dirname(getwd()), "R", "project_paths.R")
  )
  candidates <- candidates[file.exists(candidates)]
  if (length(candidates) == 0) {
    stop("Could not locate R/project_paths.R for RBA pipeline stage.",
         call. = FALSE)
  }
  source(candidates[[1]])
}

if (!exists("fetch_rba_table", mode = "function")) {
  source(project_path("pipeline", "00_config.R"))
}
if (!exists("INDICATOR_SOURCE_RBA_NEW_LOAN_RATE")) {
  source(project_path("R", "indicator_registry.R"), local = TRUE)
}

cat("--- Fetching RBA statistical tables ---\n")

all_rba <- list()

# ==============================================================================
# F1 — Cash Rate
# ==============================================================================
cat("  Fetching RBA F1 (Cash Rate)...\n")

f1_file <- tryCatch(fetch_rba_table("f1"), error = function(e) {
  stop("Required source failed for RBA F1: ", conditionMessage(e),
       call. = FALSE)
})

if (is.null(f1_file) || !file.exists(f1_file)) {
  pipeline_problem("RBA F1 acquisition failed: no validated source file was returned")
} else {
  f1_data <- parse_rba_file(f1_file, "F1")

  if (is.null(f1_data) || nrow(f1_data) == 0) {
    pipeline_problem("RBA F1 parsing failed: parsed table has no observations")
  } else {
    cash_rate <- f1_data %>%
      filter(str_detect(series, regex("cash rate|interbank", ignore_case = TRUE)))

    # No broadening fallback: a filter miss means the table layout changed,
    # and silently shipping the entire F1 table would relabel the data.
    assert_selection_nonempty(cash_rate, "RBA F1 cash/interbank rate series")

    all_rba$f1 <- cash_rate
    cat("    F1:", nrow(cash_rate), "obs,",
        length(unique(cash_rate$series)), "series\n")
  }
}

# ==============================================================================
# F5 — Indicator Lending Rates (Mortgage Rates)
# ==============================================================================
cat("  Fetching RBA F5 (Lending Rates)...\n")

f5_file <- tryCatch(fetch_rba_table("f5"), error = function(e) {
  stop("Required source failed for RBA F5: ", conditionMessage(e),
       call. = FALSE)
})

if (is.null(f5_file) || !file.exists(f5_file)) {
  pipeline_problem("RBA F5 acquisition failed: no validated source file was returned")
} else {
  f5_data <- parse_rba_file(f5_file, "F5")

  if (is.null(f5_data) || nrow(f5_data) == 0) {
    pipeline_problem("RBA F5 parsing failed: parsed table has no observations")
  } else {
    mortgage_rates <- f5_data %>%
      filter(str_detect(series, regex(
        "housing|mortgage|variable.*rate|fixed.*rate|owner.?occupier|investor",
        ignore_case = TRUE
      )))

    assert_selection_nonempty(mortgage_rates, "RBA F5 housing lending rate series")

    all_rba$f5 <- mortgage_rates
    cat("    F5:", nrow(mortgage_rates), "obs,",
        length(unique(mortgage_rates$series)), "series\n")
  }
}

# ==============================================================================
# F6 — Housing Lending Rates
# ==============================================================================
cat("  Fetching RBA F6 (Housing Lending)...\n")

f6_file <- tryCatch(fetch_rba_table("f6"), error = function(e) {
  stop("Required source failed for RBA F6: ", conditionMessage(e),
       call. = FALSE)
})

if (is.null(f6_file) || !file.exists(f6_file)) {
  pipeline_problem("RBA F6 acquisition failed: no validated source file was returned")
} else {
  f6_data <- parse_rba_file(f6_file, "F6")

  if (is.null(f6_data) || nrow(f6_data) == 0) {
    pipeline_problem("RBA F6 parsing failed: parsed table has no observations")
  } else {
    f6_contract <- rba_required_series_contract("F6")
    required_new_loan_rate <- f6_data %>%
      filter(
        series == INDICATOR_SOURCE_RBA_NEW_LOAN_RATE,
        series_id == f6_contract$series_id
      )
    assert_selection_nonempty(
      required_new_loan_rate,
      paste0("RBA F6 exact new-loan rate series ",
             INDICATOR_SOURCE_RBA_NEW_LOAN_RATE, " (",
             f6_contract$series_id, ")")
    )

    housing_finance <- f6_data %>%
      filter(str_detect(series, regex(
        "owner.?occupier|housing|loan|commitment|dwelling|variable|fixed|rate",
        ignore_case = TRUE
      )))

    assert_selection_nonempty(housing_finance, "RBA F6 new-loan rate series")

    all_rba$f6 <- housing_finance
    cat("    F6:", nrow(housing_finance), "obs,",
        length(unique(housing_finance$series)), "series\n")
  }
}

# ==============================================================================
# E2 — Household Finances: Selected Ratios (debt-to-income context)
# ==============================================================================
# Roadmap Track 2.6: official household debt-to-income context. Selected by
# exact series ID: BHFDDIT is the ratio of total household debt to annualised
# household disposable income (per cent, quarterly). These are ratios, not
# interest rates - the validation range gate treats the Household Finances
# category separately.
cat("  Fetching RBA E2 (Household Finances)...\n")

e2_file <- tryCatch(fetch_rba_table("e2"), error = function(e) {
  stop("Required source failed for RBA E2: ", conditionMessage(e),
       call. = FALSE)
})

if (is.null(e2_file) || !file.exists(e2_file)) {
  pipeline_problem("RBA E2 acquisition failed: no validated source file was returned")
} else {
  e2_data <- parse_rba_file(e2_file, "E2")

  if (is.null(e2_data) || nrow(e2_data) == 0) {
    pipeline_problem("RBA E2 parsing failed: parsed table has no observations")
  } else {
    household_dti <- e2_data %>%
      filter(series_id == "BHFDDIT")

    assert_selection_nonempty(
      household_dti,
      "RBA E2 household debt to income ratio (series BHFDDIT)"
    )

    all_rba$e2 <- household_dti
    cat("    E2:", nrow(household_dti), "obs,",
        length(unique(household_dti$series)), "series\n")
  }
}

# ==============================================================================
# Combine and write
# ==============================================================================
rba_rates <- combine_series_unique(all_rba, "rba_rates")

if (nrow(rba_rates) > 0) {
  write_pipeline_csv(rba_rates, "rba_rates.csv")
  cat("--- RBA fetch complete ---\n")
  cat("  Total series:", length(unique(rba_rates$series)), "\n")
  cat("  Date range:", as.character(min(rba_rates$date, na.rm = TRUE)),
      "to", as.character(max(rba_rates$date, na.rm = TRUE)), "\n")
} else {
  cat("--- RBA fetch complete (no data retrieved — check network) ---\n")
}

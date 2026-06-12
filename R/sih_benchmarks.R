# ==============================================================================
# SIH workbook benchmark checks
# ==============================================================================
# Fixed benchmark rows from ABS SIH workbooks. These protect the parser from
# silently ingesting sampling-error sections into estimate outputs.
# ==============================================================================

.load_sih_benchmark_project_paths <- function() {
  if (exists("project_path", mode = "function")) {
    return(invisible(TRUE))
  }

  this_file <- tryCatch(
    normalizePath(sys.frame(1)$ofile, winslash = "/", mustWork = TRUE),
    error = function(e) NA_character_
  )
  candidates <- c(
    if (!is.na(this_file)) file.path(dirname(this_file), "project_paths.R"),
    file.path(getwd(), "R", "project_paths.R"),
    file.path(dirname(getwd()), "R", "project_paths.R")
  )
  candidates <- unique(candidates[file.exists(candidates)])

  if (length(candidates) == 0) {
    stop("Could not locate R/project_paths.R for SIH benchmark checks.",
         call. = FALSE)
  }

  source(candidates[[1]])
  invisible(TRUE)
}

.load_sih_benchmark_project_paths()
rm(.load_sih_benchmark_project_paths)

sih_workbook_benchmarks <- function() {
  data.frame(
    output_file = c(
      "sih_cost_ratios_2020.csv",
      "sih_cost_ratios_2020.csv",
      "sih_stress_bands_2020.csv",
      "sih_stress_bands_2020.csv",
      "sih_lower_income_states.csv",
      "sih_lower_income_states.csv",
      "sih_lower_income_states.csv",
      "sih_nhha_rental_stress.csv",
      "sih_nhha_rental_stress.csv",
      "sih_nhha_rental_stress.csv",
      "sih_timeseries_national.csv",
      "sih_costs_2020.csv",
      "sih_age_tenure_2020.csv",
      "sih_recent_buyers_2020.csv",
      "sih_geographic_2020.csv",
      "sih_state_timeseries.csv",
      "sih_estimate_quality.csv"
    ),
    source_file = c(
      "4. Housing costs as a proportion of income.xlsx",
      "4. Housing costs as a proportion of income.xlsx",
      "5. Housing costs as a proportion of income ranges.xlsx",
      "5. Housing costs as a proportion of income ranges.xlsx",
      "8. Lower income households, state and territory.xlsx",
      "8. Lower income households, state and territory.xlsx",
      "8. Lower income households, state and territory.xlsx",
      "13. Rental affordability, lower income renter households, national housing and homelessness agreement basis.xlsx",
      "13. Rental affordability, lower income renter households, national housing and homelessness agreement basis.xlsx",
      "13. Rental affordability, lower income renter households, national housing and homelessness agreement basis.xlsx",
      "1. Housing occupancy and costs, Australia, 1994–95 to 2019–20.xlsx",
      "3. Housing costs.xlsx",
      "6. Age of household reference person.xlsx",
      "9. Recent home buyer households.xlsx",
      "11. Greater capital city statistical areas, states and territories.xlsx",
      "12. Housing occupancy costs, states and territories.xlsx",
      "5. Housing costs as a proportion of income ranges.xlsx"
    ),
    source_table = c(
      "Table 4.1",
      "Table 4.2",
      "Table 5.1",
      "Table 5.1",
      "Table 8.1",
      "Table 8.1",
      "Table 8.1",
      "Table 13.1",
      "Table 13.1",
      "Table 13.1",
      "Table 1.1",
      "Table 3.1",
      "Table 6.1",
      "Table 9.1",
      "Table 11.1",
      "Table 12.1",
      "Table 5.1"
    ),
    survey_year = rep("2019-20", 17),
    metric = c(
      "cost_income_ratio",
      "cost_income_ratio",
      "pct_over_30",
      "households_000",
      "median_weekly_cost",
      "pct_over_30",
      "households_000",
      "pct_rental_stress_over_30",
      "number_rental_stress_over_30",
      "number_lower_income_renter_households",
      "mean_weekly_cost_real",
      "weekly_housing_cost",
      "households_000",
      "Mean value of dwelling",
      "median_weekly_cost",
      "mean_weekly_cost_real",
      "pct_over_30"
    ),
    tenure = c(
      "owner_mortgage",
      "owner_mortgage",
      "owner_mortgage",
      "owner_mortgage",
      "renter_total",
      "renter_total",
      "renter_total",
      "renter_lower_income",
      "renter_lower_income",
      "renter_lower_income",
      "all",
      "all",
      "all",
      "owner_mortgage",
      "all",
      "all",
      "owner_mortgage"
    ),
    breakdown_var = c(
      "family_type",
      "family_type",
      "owner",
      "owner",
      "lower_income_state",
      "lower_income_state",
      "lower_income_state",
      "nhha_location",
      "nhha_location",
      "nhha_location",
      "tenure",
      "family_type",
      "number_of_employed_persons_in_household_by_age_group",
      "buyer_all_recent_total",
      "renter",
      "tenure",
      "owner"
    ),
    breakdown_val = c(
      "Couple family with dependent children",
      "Couple family with dependent children",
      "Owner with a mortgage",
      "Owner with a mortgage",
      "Total renters",
      "Total renters",
      "Total renters",
      "Total",
      "Total",
      "Total",
      "Total",
      "Total",
      "Estimated number of households | all households",
      "Mean value of dwelling",
      "Total",
      "Total",
      "Owner with a mortgage"
    ),
    geography = c(
      "National",
      "National",
      "National",
      "National",
      "New South Wales",
      "New South Wales",
      "New South Wales",
      "Aust.",
      "Aust.",
      "Aust.",
      "National",
      "National",
      "National",
      "National",
      "Total GCC",
      "New South Wales",
      "National"
    ),
    stat_type = c(
      "mean",
      "median",
      "all_households",
      "all_households",
      "lower_income",
      "lower_income",
      "lower_income",
      "proportion",
      "count",
      "count",
      "mean",
      "mean",
      "count_000",
      "count",
      "median",
      "mean",
      "all_households"
    ),
    value_column = c(rep("value", 16), "quality_value"),
    expected_value = c(
      15.2,
      15.9,
      17.4,
      3554.074,
      320.0,
      50.5,
      415.2,
      42.0,
      618846,
      1473599,
      317,
      317,
      9732.4,
      701.4,
      327,
      356,
      1.4
    ),
    tolerance = c(1e-9, 1e-9, 1e-9, 1e-3, 1e-9, 1e-9, 1e-9, 1e-9, 1e-9, 1e-9,
                  1e-9, 1e-9, 1e-9, 1e-9, 1e-9, 1e-9, 1e-9),
    benchmark_note = c(
      "File 4 Table 4.1 estimate block, owner with a mortgage, couple family with dependent children.",
      "File 4 Table 4.2 estimate block, owner with a mortgage, couple family with dependent children.",
      "File 5 Table 5.1 estimate block, owner with a mortgage, more than 30 per cent of gross income.",
      "File 5 Table 5.1 estimate block, owner with a mortgage, household count in thousands.",
      "File 8 Table 8.1 estimate block, NSW lower-income total renters, median weekly housing cost.",
      "File 8 Table 8.1 estimate block, NSW lower-income total renters, more than 30 per cent of gross income.",
      "File 8 Table 8.1 estimate block, NSW lower-income total renters, household count in thousands.",
      "File 13 Table 13.1 estimate block, Australia total NHHA lower-income rental stress proportion.",
      "File 13 Table 13.1 estimate block, Australia total NHHA stressed lower-income renter household count.",
      "File 13 Table 13.1 estimate block, Australia total NHHA lower-income renter household denominator.",
      "File 1 Table 1.1 estimate block, all-tenure total mean weekly housing cost (2019-20 dollars), workbook cell R15xC18.",
      "File 3 Table 3.1 estimate block, family-composition total mean weekly housing cost, workbook cell R19xC10.",
      "File 6 Table 6.1 estimate block, estimated number of households (all households column), workbook cell R61xC10.",
      "File 9 Table 9.1 estimate block, mean value of dwelling for all recent buyers (total dwellings), workbook cell R68xC13.",
      "File 11 Table 11.1 estimate block, total-GCC all-tenure median weekly housing cost, workbook cell R18xC11.",
      "File 12 Table 12.1 estimate block, NSW all-tenure total mean weekly housing cost (2019-20 dollars), workbook cell R15xC18.",
      "File 5 Table 5.1 MOE block, 95% margin of error for owner-with-mortgage stress over 30 per cent, workbook cell R87xC7."
    ),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

validate_sih_workbook_benchmarks <- function(data_dir = project_path("data"),
                                             benchmarks = sih_workbook_benchmarks()) {
  failures <- character()
  add_failure <- function(message) {
    failures <<- c(failures, message)
  }

  required_columns <- c(
    "output_file",
    "source_file",
    "source_table",
    "survey_year",
    "metric",
    "tenure",
    "breakdown_var",
    "breakdown_val",
    "geography",
    "stat_type",
    "value_column",
    "expected_value",
    "tolerance",
    "benchmark_note"
  )
  missing_benchmark_columns <- setdiff(required_columns, names(benchmarks))
  if (length(missing_benchmark_columns) > 0) {
    add_failure(paste("Benchmark table is missing columns:",
                      paste(missing_benchmark_columns, collapse = ", ")))
    return(failures)
  }

  key_columns <- c(
    "survey_year",
    "metric",
    "tenure",
    "breakdown_var",
    "breakdown_val",
    "geography",
    "stat_type"
  )
  cached_outputs <- new.env(parent = emptyenv())

  for (i in seq_len(nrow(benchmarks))) {
    benchmark <- benchmarks[i, , drop = FALSE]
    output_file <- benchmark$output_file[[1]]
    path <- file.path(data_dir, output_file)

    if (!file.exists(path)) {
      add_failure(paste(output_file, "does not exist for benchmark",
                        benchmark$source_table[[1]]))
      next
    }

    if (!exists(output_file, envir = cached_outputs, inherits = FALSE)) {
      assign(output_file, read.csv(path, stringsAsFactors = FALSE,
                                   check.names = FALSE),
             envir = cached_outputs)
    }
    output <- get(output_file, envir = cached_outputs, inherits = FALSE)

    value_column <- benchmark$value_column[[1]]
    missing_output_columns <- setdiff(c(key_columns, value_column),
                                      names(output))
    if (length(missing_output_columns) > 0) {
      add_failure(paste(output_file, "is missing benchmark columns:",
                        paste(missing_output_columns, collapse = ", ")))
      next
    }

    matched <- output
    for (key in key_columns) {
      matched <- matched[as.character(matched[[key]]) ==
                           as.character(benchmark[[key]][[1]]),
                         , drop = FALSE]
    }

    benchmark_label <- paste(
      output_file,
      benchmark$source_table[[1]],
      benchmark$metric[[1]],
      benchmark$geography[[1]]
    )

    if (nrow(matched) == 0) {
      add_failure(paste("No output row matched benchmark:", benchmark_label))
      next
    }
    if (nrow(matched) > 1) {
      add_failure(paste(nrow(matched),
                        "output rows matched benchmark:", benchmark_label))
      next
    }

    actual <- suppressWarnings(as.numeric(matched[[value_column]][[1]]))
    expected <- as.numeric(benchmark$expected_value[[1]])
    tolerance <- as.numeric(benchmark$tolerance[[1]])

    if (!is.finite(actual)) {
      add_failure(paste("Benchmark output value is non-finite:",
                        benchmark_label))
      next
    }
    if (abs(actual - expected) > tolerance) {
      add_failure(paste0(
        "Benchmark mismatch for ", benchmark_label,
        ": expected ", expected,
        " +/- ", tolerance,
        ", got ", actual
      ))
    }
  }

  failures
}

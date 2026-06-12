# Runs standalone via `Rscript tests/test_sih_workbook_benchmarks.R` (repo root) and under
# testthat::test_dir("tests"); see tests/helper-contracts.R.
if (!exists("contracts_harness_loaded", mode = "function")) {
  source(file.path(if (basename(getwd()) == "tests") "." else "tests",
                   "helper-contracts.R"))
}

test_that("sih_workbook_benchmarks contracts", {
  repo_root <- repo_root_path()


  benchmark_helper <- file.path(repo_root, "R", "sih_benchmarks.R")
  check(file.exists(benchmark_helper), "R/sih_benchmarks.R does not exist")

  if (file.exists(benchmark_helper)) {
    source(benchmark_helper, local = TRUE)

    required_functions <- c(
      "sih_workbook_benchmarks",
      "validate_sih_workbook_benchmarks"
    )
    for (fn in required_functions) {
      check(exists(fn, mode = "function"),
            paste("R/sih_benchmarks.R is missing function:", fn))
    }

    if (exists("sih_workbook_benchmarks", mode = "function")) {
      benchmarks <- sih_workbook_benchmarks()

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
      missing_columns <- setdiff(required_columns, names(benchmarks))
      check(length(missing_columns) == 0,
            paste("Benchmark table is missing columns:",
                  paste(missing_columns, collapse = ", ")))

      if (length(missing_columns) == 0) {
        check(nrow(benchmarks) >= 17,
              "Benchmark table must include at least 17 rows")
        check(!anyDuplicated(benchmarks[setdiff(required_columns,
                                                "benchmark_note")]),
              "Benchmark table contains duplicate benchmark keys")
        check(all(is.finite(benchmarks$expected_value)),
              "Benchmark expected values must be finite")
        check(all(is.finite(benchmarks$tolerance) & benchmarks$tolerance >= 0),
              "Benchmark tolerances must be finite and non-negative")
        check(all(nzchar(benchmarks$benchmark_note)),
              "Every benchmark row must include a benchmark note")
        check(all(benchmarks$value_column %in% c("value", "quality_value")),
              "Benchmark value_column must be value or quality_value")

        required_outputs <- c(
          "sih_cost_ratios_2020.csv",
          "sih_stress_bands_2020.csv",
          "sih_lower_income_states.csv",
          "sih_nhha_rental_stress.csv",
          "sih_timeseries_national.csv",
          "sih_costs_2020.csv",
          "sih_age_tenure_2020.csv",
          "sih_recent_buyers_2020.csv",
          "sih_geographic_2020.csv",
          "sih_state_timeseries.csv",
          "sih_estimate_quality.csv"
        )
        missing_outputs <- setdiff(required_outputs, unique(benchmarks$output_file))
        check(length(missing_outputs) == 0,
              paste("Benchmark table is missing output coverage:",
                    paste(missing_outputs, collapse = ", ")))

        required_tables <- c("Table 4.1", "Table 4.2", "Table 5.1", "Table 8.1",
                             "Table 13.1", "Table 1.1", "Table 3.1", "Table 6.1",
                             "Table 9.1", "Table 11.1", "Table 12.1")
        missing_tables <- setdiff(required_tables, unique(benchmarks$source_table))
        check(length(missing_tables) == 0,
              paste("Benchmark table is missing source table coverage:",
                    paste(missing_tables, collapse = ", ")))

        required_file_prefixes <- c("1.", "3.", "4.", "5.", "6.", "8.", "9.",
                                    "11.", "12.", "13.")
        missing_file_prefixes <- required_file_prefixes[
          !vapply(required_file_prefixes, function(prefix) {
            any(startsWith(benchmarks$source_file, prefix))
          }, logical(1))
        ]
        check(length(missing_file_prefixes) == 0,
              paste("Benchmark table is missing source file prefixes:",
                    paste(missing_file_prefixes, collapse = ", ")))
      }
    }

    if (exists("validate_sih_workbook_benchmarks", mode = "function")) {
      benchmark_failures <- validate_sih_workbook_benchmarks(
        data_dir = file.path(repo_root, "data")
      )
      check(length(benchmark_failures) == 0,
            paste(c("SIH workbook benchmarks failed:",
                    paste0("- ", benchmark_failures)),
                  collapse = "\n"))
    }
  }
})
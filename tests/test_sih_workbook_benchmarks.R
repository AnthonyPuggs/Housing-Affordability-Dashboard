repo_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
failures <- character()

check <- function(condition, message) {
  if (!isTRUE(condition)) {
    failures <<- c(failures, message)
  }
}

benchmark_helper <- file.path(repo_root, "R", "sih_benchmarks.R")
check(file.exists(benchmark_helper), "R/sih_benchmarks.R does not exist")

if (file.exists(benchmark_helper)) {
  source(benchmark_helper)

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
      "expected_value",
      "tolerance",
      "benchmark_note"
    )
    missing_columns <- setdiff(required_columns, names(benchmarks))
    check(length(missing_columns) == 0,
          paste("Benchmark table is missing columns:",
                paste(missing_columns, collapse = ", ")))

    if (length(missing_columns) == 0) {
      check(nrow(benchmarks) >= 10,
            "Benchmark table must include at least 10 rows")
      check(!anyDuplicated(benchmarks[required_columns[-13]]),
            "Benchmark table contains duplicate benchmark keys")
      check(all(is.finite(benchmarks$expected_value)),
            "Benchmark expected values must be finite")
      check(all(is.finite(benchmarks$tolerance) & benchmarks$tolerance >= 0),
            "Benchmark tolerances must be finite and non-negative")
      check(all(nzchar(benchmarks$benchmark_note)),
            "Every benchmark row must include a benchmark note")

      required_outputs <- c(
        "sih_cost_ratios_2020.csv",
        "sih_stress_bands_2020.csv",
        "sih_lower_income_states.csv",
        "sih_nhha_rental_stress.csv"
      )
      missing_outputs <- setdiff(required_outputs, unique(benchmarks$output_file))
      check(length(missing_outputs) == 0,
            paste("Benchmark table is missing output coverage:",
                  paste(missing_outputs, collapse = ", ")))

      required_tables <- c("Table 4.1", "Table 4.2", "Table 5.1", "Table 8.1", "Table 13.1")
      missing_tables <- setdiff(required_tables, unique(benchmarks$source_table))
      check(length(missing_tables) == 0,
            paste("Benchmark table is missing source table coverage:",
                  paste(missing_tables, collapse = ", ")))

      required_file_prefixes <- c("4.", "5.", "8.", "13.")
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

if (length(failures) > 0) {
  stop(
    paste(c("SIH workbook benchmark checks failed:",
            paste0("- ", failures)),
          collapse = "\n"),
    call. = FALSE
  )
}

cat("SIH workbook benchmark checks passed.\n")

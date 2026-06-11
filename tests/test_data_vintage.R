# Runs standalone via `Rscript tests/test_data_vintage.R` (repo root) and under
# testthat::test_dir("tests"); see tests/helper-contracts.R.
if (!exists("contracts_harness_loaded", mode = "function")) {
  source(file.path(if (basename(getwd()) == "tests") "." else "tests",
                   "helper-contracts.R"))
}

test_that("data_vintage contracts", {
  repo_root <- repo_root_path()


  path <- function(...) file.path(repo_root, ...)

  project_paths_path <- path("R", "project_paths.R")
  data_vintage_path <- path("R", "data_vintage.R")
  data_vintage_csv <- path("data", "data_vintage.csv")

  check(file.exists(project_paths_path), "R/project_paths.R does not exist")
  check(file.exists(data_vintage_path), "R/data_vintage.R does not exist")

  if (file.exists(data_vintage_path)) {
    parsed <- tryCatch({
      parse(data_vintage_path)
      TRUE
    }, error = function(e) conditionMessage(e))
    check(identical(parsed, TRUE),
          paste("R/data_vintage.R does not parse:", parsed))
  }

  if (all(file.exists(c(project_paths_path, data_vintage_path)))) {
    source(project_paths_path, local = TRUE)
    source(data_vintage_path, local = TRUE)

    required_functions <- c(
      "build_data_vintage",
      "write_data_vintage",
      "read_data_vintage",
      "data_vintage_summary",
      "data_vintage_detail"
    )
    missing_functions <- required_functions[
      !exists_in(required_functions, environment())
    ]
    check(length(missing_functions) == 0,
          paste("R/data_vintage.R missing functions:",
                paste(missing_functions, collapse = ", ")))

    if (exists("build_data_vintage", mode = "function")) {
      vintage <- build_data_vintage(
        data_dir = project_path("data"),
        refreshed_at = as.POSIXct("2026-05-23 21:12:00", tz = "UTC")
      )
      expected_schema <- c(
        "dataset",
        "file",
        "rows",
        "period_min",
        "period_max",
        "modified_utc",
        "refreshed_at_utc",
        "source_group"
      )
      check(identical(names(vintage), expected_schema),
            paste("build_data_vintage() must return schema:",
                  paste(expected_schema, collapse = " | ")))
      check(nrow(vintage) > 0, "build_data_vintage() must return rows")

      required_period_files <- c(
        "abs_timeseries.csv",
        "abs_supply_demand.csv",
        "rba_rates.csv",
        "affordability_indices.csv"
      )
      for (filename in required_period_files) {
        row <- vintage[vintage$file == file.path("data", filename), , drop = FALSE]
        check(nrow(row) == 1, paste("Missing vintage row for", filename))
        check(nzchar(row$period_max[1]),
              paste("period_max must be populated for", filename))
      }

      sih_rows <- vintage[grepl("^data/sih_", vintage$file), , drop = FALSE]
      check(nrow(sih_rows) > 0, "SIH files must be represented in vintage data")
      check(all(grepl("^[0-9]{4}-[0-9]{2}$", sih_rows$period_max)),
            "SIH period_max must use survey-year values such as 2019-20")
      check(all(sih_rows$source_group == "Static ABS SIH workbook outputs"),
            "SIH rows must be identified as static ABS SIH workbook outputs")

      if (exists("data_vintage_summary", mode = "function")) {
        summary <- data_vintage_summary(vintage)
        required_summary_text <- c(
          "Data refreshed 24 May 2026 07:12 AEST",
          "Latest ABS/RBA observation:",
          "SIH: 2019-20"
        )
        missing_summary_text <- required_summary_text[
          !vapply(required_summary_text, grepl, logical(1),
                  summary, fixed = TRUE)
        ]
        check(length(missing_summary_text) == 0,
              paste("data_vintage_summary() missing text:",
                    paste(missing_summary_text, collapse = "; ")))

        fallback_summary <- data_vintage_summary(vintage, fallback = TRUE)
        check(grepl("Data vintage derived from bundled CSVs",
                    fallback_summary, fixed = TRUE),
              "Fallback summary must say the vintage was derived from bundled CSVs")
      }

      if (exists("data_vintage_detail", mode = "function")) {
        detail <- data_vintage_detail(vintage)
        check(grepl("abs_timeseries.csv", detail, fixed = TRUE),
              "data_vintage_detail() must include ABS time-series file detail")
        check(grepl("rba_rates.csv", detail, fixed = TRUE),
              "data_vintage_detail() must include RBA rates file detail")
        check(!grepl("/Users/", detail, fixed = TRUE),
              "data_vintage_detail() must not expose local /Users/ paths")
      }
    }

    check(file.exists(data_vintage_csv),
          "data/data_vintage.csv must exist after a successful pipeline run")
    if (file.exists(data_vintage_csv) &&
        exists("read_data_vintage", mode = "function")) {
      persisted <- read_data_vintage(project_path("data"), fallback = FALSE)
      check(identical(names(persisted), c(
        "dataset",
        "file",
        "rows",
        "period_min",
        "period_max",
        "modified_utc",
        "refreshed_at_utc",
        "source_group"
      )), "Persisted data_vintage.csv has the wrong schema")

      persisted_from_file <- read_data_vintage(data_vintage_csv, fallback = FALSE)
      check(identical(nrow(persisted_from_file), nrow(persisted)),
            "read_data_vintage() must also accept data/data_vintage.csv paths")
    }
  }
})
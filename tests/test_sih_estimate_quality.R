failures <- character()

check <- function(condition, message) {
  if (!isTRUE(condition)) {
    failures <<- c(failures, message)
  }
}

repo_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
pipeline_path <- file.path(repo_root, "pipeline", "01_process_sih.R")
quality_path <- file.path(repo_root, "data", "sih_estimate_quality.csv")

check(file.exists(pipeline_path), "pipeline/01_process_sih.R does not exist")
if (file.exists(pipeline_path)) {
  parsed <- tryCatch({
    parse(pipeline_path)
    TRUE
  }, error = function(e) conditionMessage(e))
  check(identical(parsed, TRUE),
        paste("pipeline/01_process_sih.R does not parse:", parsed))
}

check(file.exists(quality_path), "data/sih_estimate_quality.csv does not exist")

required_columns <- c(
  "source_file",
  "source_table",
  "survey_year",
  "metric",
  "tenure",
  "breakdown_var",
  "breakdown_val",
  "geography",
  "stat_type",
  "quality_measure",
  "quality_value",
  "quality_unit",
  "reliability_flag",
  "reliability_note"
)

if (file.exists(quality_path)) {
  quality <- read.csv(quality_path, stringsAsFactors = FALSE,
                      check.names = FALSE)
  missing_columns <- setdiff(required_columns, names(quality))
  check(length(missing_columns) == 0,
        paste("sih_estimate_quality.csv is missing columns:",
              paste(missing_columns, collapse = ", ")))

  if (length(missing_columns) == 0) {
    check(nrow(quality) > 0,
          "sih_estimate_quality.csv must contain at least one row")
    check(all(is.finite(quality$quality_value)),
          "quality_value must be finite for every parsed quality row")

    key_cols <- c(
      "source_file",
      "source_table",
      "survey_year",
      "metric",
      "tenure",
      "breakdown_var",
      "breakdown_val",
      "geography",
      "stat_type",
      "quality_measure"
    )
    dup_count <- sum(duplicated(quality[key_cols]))
    check(dup_count == 0,
          paste("sih_estimate_quality.csv has", dup_count,
                "duplicate key rows"))

    required_tables <- c("Table 5.1", "Table 5.2", "Table 8.1", "Table 13.1")
    missing_tables <- setdiff(required_tables, unique(quality$source_table))
    check(length(missing_tables) == 0,
          paste("sih_estimate_quality.csv is missing tables:",
                paste(missing_tables, collapse = ", ")))

    required_files <- c(
      "5. Housing costs as a proportion of income ranges.xlsx",
      "8. Lower income households, state and territory.xlsx",
      "13. Rental affordability, lower income renter households, national housing and homelessness agreement basis.xlsx"
    )
    missing_files <- required_files[
      !vapply(required_files, function(file) {
        any(grepl(file, quality$source_file, fixed = TRUE))
      }, logical(1))
    ]
    check(length(missing_files) == 0,
          paste("sih_estimate_quality.csv is missing source files:",
                paste(missing_files, collapse = ", ")))

    required_measures <- c("moe_95", "rse_pct")
    missing_measures <- setdiff(required_measures, unique(quality$quality_measure))
    check(length(missing_measures) == 0,
          paste("sih_estimate_quality.csv is missing quality measures:",
                paste(missing_measures, collapse = ", ")))

    allowed_units <- c("percentage_points", "per_cent")
    bad_units <- setdiff(unique(quality$quality_unit), allowed_units)
    check(length(bad_units) == 0,
          paste("sih_estimate_quality.csv has unexpected quality units:",
                paste(bad_units, collapse = ", ")))

    allowed_flags <- c("standard", "use_with_caution", "too_unreliable")
    bad_flags <- setdiff(unique(quality$reliability_flag), allowed_flags)
    check(length(bad_flags) == 0,
          paste("sih_estimate_quality.csv has unexpected reliability flags:",
                paste(bad_flags, collapse = ", ")))

    if ("rse_pct" %in% quality$quality_measure) {
      rse_rows <- quality[quality$quality_measure == "rse_pct", ]
      check(all(rse_rows$quality_unit == "per_cent"),
            "rse_pct rows must use per_cent units")
      check(all(rse_rows$reliability_flag[rse_rows$quality_value < 25] == "standard"),
            "RSE values below 25 must be standard")
      check(all(rse_rows$reliability_flag[
        rse_rows$quality_value >= 25 & rse_rows$quality_value <= 50
      ] == "use_with_caution"),
      "RSE values from 25 to 50 must be use_with_caution")
      check(all(rse_rows$reliability_flag[rse_rows$quality_value > 50] == "too_unreliable"),
            "RSE values above 50 must be too_unreliable")
    }

    if ("moe_95" %in% quality$quality_measure) {
      moe_rows <- quality[quality$quality_measure == "moe_95", ]
      check(all(moe_rows$quality_unit == "percentage_points"),
            "moe_95 rows must use percentage_points units")
    }
  }
}

if (length(failures) > 0) {
  stop(
    paste(c("SIH estimate quality checks failed:", paste0("- ", failures)),
          collapse = "\n"),
    call. = FALSE
  )
}

cat("SIH estimate quality checks passed.\n")

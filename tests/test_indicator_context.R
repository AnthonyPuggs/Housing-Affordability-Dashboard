# Runs standalone via `Rscript tests/test_indicator_context.R` (repo root) and under
# testthat::test_dir("tests"); see tests/helper-contracts.R.
if (!exists("contracts_harness_loaded", mode = "function")) {
  source(file.path(if (basename(getwd()) == "tests") "." else "tests",
                   "helper-contracts.R"))
}

test_that("indicator_context contracts", {
  repo_root <- repo_root_path()


  registry_path <- file.path(repo_root, "R", "indicator_registry.R")
  context_path <- file.path(repo_root, "R", "indicator_context.R")
  vintage_path <- file.path(repo_root, "R", "data_vintage.R")
  contracts_path <- file.path(repo_root, "R", "pipeline_contracts.R")

  check(file.exists(registry_path), "R/indicator_registry.R does not exist")
  check(file.exists(context_path), "R/indicator_context.R does not exist")
  check(file.exists(vintage_path), "R/data_vintage.R does not exist")
  check(file.exists(contracts_path), "R/pipeline_contracts.R does not exist")

  if (file.exists(context_path)) {
    parsed <- tryCatch({
      parse(context_path)
      TRUE
    }, error = function(e) conditionMessage(e))
    check(identical(parsed, TRUE),
          paste("R/indicator_context.R does not parse:", parsed))
  }

  if (all(file.exists(c(registry_path, context_path, vintage_path,
                        contracts_path)))) {
    source(registry_path, local = TRUE)
    source(vintage_path, local = TRUE)
    source(contracts_path, local = TRUE)
    source(context_path, local = TRUE)

    required_functions <- c(
      "indicator_context_table",
      "indicator_confidence_note",
      "indicator_context_badges",
      "indicator_quality_coverage_summary"
    )
    missing_functions <- required_functions[
      !exists_in(required_functions, environment())
    ]
    check(length(missing_functions) == 0,
          paste("Missing indicator context helpers:",
                paste(missing_functions, collapse = ", ")))

    if (exists("indicator_context_table", mode = "function")) {
      context <- indicator_context_table(data_dir = file.path(repo_root, "data"))
      required_columns <- c(
        "indicator",
        "chart_label",
        "measure_class",
        "methodology_version",
        "primary_source",
        "quality_note",
        "vintage_dataset",
        "period_min",
        "period_max",
        "source_group",
        "public_caveat"
      )
      missing_columns <- setdiff(required_columns, names(context))
      check(length(missing_columns) == 0,
            paste("indicator_context_table() missing columns:",
                  paste(missing_columns, collapse = ", ")))
      check(nrow(context) == nrow(indicator_registry()),
            "indicator_context_table() must return one row per registry indicator")
      check(all(context$measure_class %in%
                  c("official_survey", "official_aggregate", "derived_index",
                    "stylised_scenario", "context_series")),
            "indicator_context_table() returned an unsupported measure_class")
      check(all(!is.na(context$period_max) & nzchar(context$period_max)),
            "Every indicator context row must expose a period_max from data vintage")
    }

    if (exists("indicator_confidence_note", mode = "function")) {
      note <- indicator_confidence_note("National Housing Affordability Score")
      required_note_text <- c(
        "Stylised scenario",
        "national_affordability_score_v2",
        "Latest observation"
      )
      missing_note_text <- required_note_text[
        !vapply(required_note_text, grepl, logical(1), note, fixed = TRUE)
      ]
      check(length(missing_note_text) == 0,
            paste("indicator_confidence_note() missing text:",
                  paste(missing_note_text, collapse = "; ")))
    }

    if (exists("indicator_quality_coverage_summary", mode = "function")) {
      summary <- indicator_quality_coverage_summary(
        data_dir = file.path(repo_root, "data")
      )
      required_summary_cols <- c("measure_class", "indicators", "latest_period")
      check(all(required_summary_cols %in% names(summary)),
            "indicator_quality_coverage_summary() has the wrong schema")
      check(any(summary$measure_class == "stylised_scenario"),
            "Quality summary must include stylised_scenario rows")
      check(any(summary$measure_class == "derived_index"),
            "Quality summary must include derived_index rows")
      check(any(summary$measure_class == "official_aggregate"),
            "Quality summary must include official_aggregate rows")
    }
  }
})
# Runs standalone via `Rscript tests/test_pipeline_driver_stage_gates.R` (repo root) and under
# testthat::test_dir("tests"); see tests/helper-contracts.R.
if (!exists("contracts_harness_loaded", mode = "function")) {
  source(file.path(if (basename(getwd()) == "tests") "." else "tests",
                   "helper-contracts.R"))
}

test_that("pipeline_driver_stage_gates contracts", {
  repo_root <- repo_root_path()


  driver_path <- file.path(repo_root, "pipeline", "05_driver.R")
  check(file.exists(driver_path), "pipeline/05_driver.R does not exist")

  driver_text <- if (file.exists(driver_path)) {
    paste(readLines(driver_path, warn = FALSE), collapse = "\n")
  } else {
    ""
  }

  check(grepl('source(project_path("R", "pipeline_contracts.R"), local = TRUE)',
              driver_text, fixed = TRUE),
        "pipeline/05_driver.R must source R/pipeline_contracts.R after config")

  required_stage_gates <- c(
    'validate_pipeline_stage_outputs("sih", fail = TRUE)',
    'validate_pipeline_stage_outputs("abs_timeseries", fail = TRUE)',
    'validate_pipeline_stage_outputs("abs_supply", fail = TRUE)',
    'validate_pipeline_stage_outputs("rba", fail = TRUE)',
    'validate_pipeline_stage_outputs("indicators", fail = TRUE)',
    # Run-freshness manifest: each stage's outputs must be rewritten by the
    # current run, not just exist from the checkout.
    'validate_pipeline_stage_freshness("sih", start_time)',
    'validate_pipeline_stage_freshness("abs_timeseries", start_time)',
    'validate_pipeline_stage_freshness("abs_supply", start_time)',
    'validate_pipeline_stage_freshness("rba", start_time)',
    'validate_pipeline_stage_freshness("indicators", start_time)',
    # Driver runs promote downgraded warnings (parser/write-lock) to errors.
    'PIPELINE_STRICT <- TRUE'
  )
  missing_stage_gates <- required_stage_gates[
    !vapply(required_stage_gates, grepl, logical(1), driver_text, fixed = TRUE)
  ]
  check(length(missing_stage_gates) == 0,
        paste("pipeline/05_driver.R missing stage gates:",
              paste(missing_stage_gates, collapse = "; ")))

  position <- function(needle) {
    pos <- regexpr(needle, driver_text, fixed = TRUE)[[1]]
    if (pos < 0) NA_integer_ else pos
  }

  ordered_pairs <- list(
    c('run_step("Step 1"', 'validate_pipeline_stage_outputs("sih", fail = TRUE)'),
    c('run_step("Step 2"', 'validate_pipeline_stage_outputs("abs_timeseries", fail = TRUE)'),
    c('run_step("Step 2b"', 'validate_pipeline_stage_outputs("abs_supply", fail = TRUE)'),
    c('run_step("Step 3"', 'validate_pipeline_stage_outputs("rba", fail = TRUE)'),
    c('run_step("Step 4"', 'validate_pipeline_stage_outputs("indicators", fail = TRUE)')
  )

  for (pair in ordered_pairs) {
    first <- position(pair[[1]])
    second <- position(pair[[2]])
    check(!is.na(first) && !is.na(second) && first < second,
          paste(pair[[2]], "must appear after", pair[[1]]))
  }

  final_validation_pos <- position('run_step("Step 5", project_path("pipeline", "06_validate_outputs.R"))')
  vintage_pos <- position('run_step("Step 6", project_path("pipeline", "07_write_data_vintage.R"))')
  complete_pos <- position("Pipeline complete")
  check(!is.na(final_validation_pos),
        "pipeline/05_driver.R must keep final pipeline/06_validate_outputs.R validation")
  check(!is.na(vintage_pos),
        "pipeline/05_driver.R must write data vintage metadata after validation")
  check(!is.na(final_validation_pos) && !is.na(vintage_pos) &&
          final_validation_pos < vintage_pos,
        "data vintage metadata must be written after final output validation")
  check(!is.na(final_validation_pos) && !is.na(complete_pos) &&
          !is.na(vintage_pos) && vintage_pos < complete_pos,
        "Pipeline complete must appear after data vintage metadata is written")
})
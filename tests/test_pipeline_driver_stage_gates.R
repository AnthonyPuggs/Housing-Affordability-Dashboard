repo_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
failures <- character()

check <- function(condition, message) {
  if (!isTRUE(condition)) {
    failures <<- c(failures, message)
  }
}

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
  'validate_pipeline_stage_outputs("indicators", fail = TRUE)'
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
complete_pos <- position("Pipeline complete")
check(!is.na(final_validation_pos),
      "pipeline/05_driver.R must keep final pipeline/06_validate_outputs.R validation")
check(!is.na(final_validation_pos) && !is.na(complete_pos) &&
        final_validation_pos < complete_pos,
      "Pipeline complete must appear after final output validation")

if (length(failures) > 0) {
  stop(
    paste(c("Pipeline driver stage gate checks failed:", paste0("- ", failures)),
          collapse = "\n"),
    call. = FALSE
  )
}

cat("Pipeline driver stage gate checks passed.\n")

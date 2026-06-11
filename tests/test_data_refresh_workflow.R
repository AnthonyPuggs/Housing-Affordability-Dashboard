repo_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
failures <- character()

check <- function(condition, message) {
  if (!isTRUE(condition)) {
    failures <<- c(failures, message)
  }
}

workflow_path <- file.path(repo_root, ".github", "workflows", "data-refresh.yml")
check(file.exists(workflow_path),
      ".github/workflows/data-refresh.yml does not exist")

workflow_text <- if (file.exists(workflow_path)) {
  paste(readLines(workflow_path, warn = FALSE), collapse = "\n")
} else {
  ""
}

required_workflow_text <- c(
  "workflow_dispatch:",
  "schedule:",
  "cron: '0 21 * * 0-4'",
  "':(exclude)data/data_vintage.csv'",
  "permissions:",
  "contents: write",
  "concurrency:",
  "group: data-refresh-${{ github.ref }}",
  "Rscript pipeline/05_driver.R",
  "Rscript tests/test_data_vintage.R",
  "Rscript tests/test_abs_labour_api_contracts.R",
  "Rscript tests/test_pipeline_driver_stage_gates.R",
  "Rscript tests/test_provenance_report.R",
  "Rscript tests/test_ui_smoke_contracts.R",
  "git commit -m \"data: refresh dashboard inputs\"",
  "data/*.csv"
)

missing_workflow_text <- required_workflow_text[
  !vapply(required_workflow_text, grepl, logical(1),
          workflow_text, fixed = TRUE)
]
check(length(missing_workflow_text) == 0,
      paste("data-refresh workflow missing text:",
            paste(missing_workflow_text, collapse = "; ")))

forbidden_text <- c(
  "CONNECT_API_KEY",
  "CONNECT_SERVER",
  "rsconnect::deployApp",
  "rstudio/actions/connect-publish"
)
present_forbidden <- forbidden_text[
  vapply(forbidden_text, grepl, logical(1), workflow_text, fixed = TRUE)
]
check(length(present_forbidden) == 0,
      paste("data-refresh workflow must not embed deployment secrets or manual deployment:",
            paste(present_forbidden, collapse = "; ")))

# 'timezone:' is not a GitHub Actions schedule key (schedules are UTC-only);
# a stray key silently shifts the run to the wrong local time.
check(!grepl("timezone:", workflow_text, fixed = TRUE),
      "data-refresh workflow must not use the invalid 'timezone:' schedule key (Actions schedules are UTC-only)")

if (length(failures) > 0) {
  stop(
    paste(c("Data-refresh workflow checks failed:", paste0("- ", failures)),
          collapse = "\n"),
    call. = FALSE
  )
}

cat("Data-refresh workflow checks passed.\n")

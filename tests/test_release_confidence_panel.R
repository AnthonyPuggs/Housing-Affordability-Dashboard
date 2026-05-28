repo_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
failures <- character()

check <- function(condition, message) {
  if (!isTRUE(condition)) {
    failures <<- c(failures, message)
  }
}

release_path <- file.path(repo_root, "R", "release_checklist.R")
methodology_path <- file.path(repo_root, "R", "methodology_module.R")
provenance_path <- file.path(repo_root, "R", "provenance_report.R")
ui_helper_path <- file.path(repo_root, "R", "app_ui_helpers.R")

check(file.exists(release_path), "R/release_checklist.R does not exist")
check(file.exists(methodology_path), "R/methodology_module.R does not exist")
check(file.exists(provenance_path), "R/provenance_report.R does not exist")
check(file.exists(ui_helper_path), "R/app_ui_helpers.R does not exist")

if (file.exists(release_path)) {
  source(release_path)
  check(exists("release_confidence_summary", mode = "function"),
        "release_confidence_summary() must be defined")

  if (exists("release_confidence_summary", mode = "function")) {
    summary <- release_confidence_summary(
      repo_root = repo_root,
      data_dir = file.path(repo_root, "data")
    )
    required_cols <- c("label", "value", "status", "detail")
    missing_cols <- setdiff(required_cols, names(summary))
    check(length(missing_cols) == 0,
          paste("release_confidence_summary() missing columns:",
                paste(missing_cols, collapse = ", ")))
    required_labels <- c(
      "Release checks",
      "Latest ABS/RBA observation",
      "SIH survey period",
      "External source manifest",
      "Known caveats"
    )
    missing_labels <- setdiff(required_labels, summary$label)
    check(length(missing_labels) == 0,
          paste("Release confidence summary missing labels:",
                paste(missing_labels, collapse = ", ")))
    check(all(summary$status %in% c("pass", "warn", "fail")),
          "Release confidence status values must be pass, warn or fail")
  }
}

if (file.exists(ui_helper_path)) {
  helper_text <- paste(readLines(ui_helper_path, warn = FALSE), collapse = "\n")
  check(grepl("data_vintage_detail", helper_text, fixed = TRUE),
        "Data vintage badge must expose detailed vintage text")
  check(grepl("data-vintage-badge-detail", helper_text, fixed = TRUE),
        "Data vintage badge must include a detail popover/tooltip class")
}

if (file.exists(methodology_path)) {
  module_text <- paste(readLines(methodology_path, warn = FALSE), collapse = "\n")
  required_module_text <- c(
    "Quality & Coverage",
    "Release Confidence",
    'tableOutput(ns("quality_coverage_table"))',
    'tableOutput(ns("release_confidence_table"))',
    "indicator_quality_coverage_summary(",
    "release_confidence_summary("
  )
  missing_module_text <- required_module_text[
    !vapply(required_module_text, grepl, logical(1), module_text, fixed = TRUE)
  ]
  check(length(missing_module_text) == 0,
        paste("Methodology module missing quality/release panel text:",
              paste(missing_module_text, collapse = "; ")))
}

if (file.exists(provenance_path)) {
  provenance_text <- paste(readLines(provenance_path, warn = FALSE),
                           collapse = "\n")
  required_provenance_text <- c(
    "## Quality & Coverage",
    "## Release Confidence",
    "indicator_quality_coverage_summary",
    "release_confidence_summary"
  )
  missing_provenance_text <- required_provenance_text[
    !vapply(required_provenance_text, grepl, logical(1), provenance_text,
            fixed = TRUE)
  ]
  check(length(missing_provenance_text) == 0,
        paste("Provenance report missing quality/release sections:",
              paste(missing_provenance_text, collapse = "; ")))
}

if (length(failures) > 0) {
  stop(
    paste(c("Release confidence panel checks failed:",
            paste0("- ", failures)),
          collapse = "\n"),
    call. = FALSE
  )
}

cat("Release confidence panel checks passed.\n")

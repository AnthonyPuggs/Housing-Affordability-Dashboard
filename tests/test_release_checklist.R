repo_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
failures <- character()

check <- function(condition, message) {
  if (!isTRUE(condition)) {
    failures <<- c(failures, message)
  }
}

path <- function(...) file.path(repo_root, ...)

project_paths_path <- path("R", "project_paths.R")
checklist_path <- path("R", "release_checklist.R")
readme_path <- path("README.md")

check(file.exists(project_paths_path), "R/project_paths.R does not exist")
check(file.exists(checklist_path), "R/release_checklist.R does not exist")

if (file.exists(checklist_path)) {
  parsed <- tryCatch({
    parse(checklist_path)
    TRUE
  }, error = function(e) conditionMessage(e))
  check(identical(parsed, TRUE),
        paste("R/release_checklist.R does not parse:", parsed))
}

if (all(file.exists(c(project_paths_path, checklist_path)))) {
  source(project_paths_path)
  source(checklist_path)

  required_functions <- c("release_checklist", "validate_release_checklist")
  missing_functions <- required_functions[
    !vapply(required_functions, exists, logical(1), mode = "function")
  ]
  check(length(missing_functions) == 0,
        paste("R/release_checklist.R missing functions:",
              paste(missing_functions, collapse = ", ")))

  if (exists("release_checklist", mode = "function")) {
    checks <- release_checklist()
    expected_schema <- c(
      "check_id", "category", "status", "detail", "recommendation"
    )
    check(identical(names(checks), expected_schema),
          paste("release_checklist() must return schema:",
                paste(expected_schema, collapse = " | ")))
    check(nrow(checks) > 0, "release_checklist() must return at least one check")
    check(all(checks$status %in% c("pass", "warn", "fail")),
          "release_checklist() status values must be pass, warn or fail")
    check(all(c("data", "methodology", "reproducibility", "hygiene") %in%
                unique(checks$category)),
          "release_checklist() missing required categories")
    check("methodology_pipeline_contracts" %in% checks$check_id,
          "release_checklist() must include the pipeline-contract surface")
    check("methodology_external_source_manifest" %in% checks$check_id,
          "release_checklist() must include the external-source manifest")
    check(!any(checks$status == "fail"),
          paste("Current release checklist must not have fail statuses:",
                paste(checks$check_id[checks$status == "fail"],
                      collapse = ", ")))
    check(!any(grepl("/Users/", checks$detail, fixed = TRUE)),
          "release_checklist() details must not expose local /Users/ paths")
    check(!any(grepl("/Users/", checks$recommendation, fixed = TRUE)),
          "release_checklist() recommendations must not expose local /Users/ paths")
  }

  if (exists("validate_release_checklist", mode = "function")) {
    validation <- tryCatch({
      validate_release_checklist()
    }, error = function(e) e)
    check(!inherits(validation, "error"),
          paste("validate_release_checklist() must not fail for current repo:",
                conditionMessage(validation)))
  }
}

check(file.exists(readme_path), "README.md does not exist")
if (file.exists(readme_path)) {
  readme_text <- paste(readLines(readme_path, warn = FALSE), collapse = "\n")
  required_readme_text <- c(
    "Rscript -e \"source('R/release_checklist.R'); validate_release_checklist()\"",
    "warnings can be acceptable for known data vintage",
    "failures block public release",
    "per-stage output gates",
    "fixed external-source manifest"
  )
  missing_readme_text <- required_readme_text[
    !vapply(required_readme_text, grepl, logical(1),
            readme_text, fixed = TRUE)
  ]
  check(length(missing_readme_text) == 0,
        paste("README.md missing release checklist text:",
              paste(missing_readme_text, collapse = "; ")))
}

if (length(failures) > 0) {
  stop(
    paste(c("Release checklist checks failed:", paste0("- ", failures)),
          collapse = "\n"),
    call. = FALSE
  )
}

cat("Release checklist checks passed.\n")

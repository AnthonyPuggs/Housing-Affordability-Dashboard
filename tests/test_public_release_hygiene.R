repo_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
failures <- character()

check <- function(condition, message) {
  if (!isTRUE(condition)) {
    failures <<- c(failures, message)
  }
}

read_text <- function(path) {
  paste(readLines(file.path(repo_root, path), warn = FALSE), collapse = "\n")
}

readme_text <- read_text("README.md")
gitignore_text <- read_text(".gitignore")
gitignore_lines <- trimws(readLines(file.path(repo_root, ".gitignore"), warn = FALSE))
gitignore_patterns <- gitignore_lines[nzchar(gitignore_lines) & !startsWith(gitignore_lines, "#")]

check(!grepl("/Users/", readme_text, fixed = TRUE),
      "README.md contains an absolute /Users/ path")
check(!grepl("New project", readme_text, fixed = TRUE),
      "README.md contains the stale New project path")

required_readme <- c(
  "Rscript -e \"shiny::runApp('.')\"",
  "Rscript pipeline/05_driver.R",
  "Rscript tests/test_pipeline_outputs.R",
  "stylised scenarios"
)

for (needle in required_readme) {
  check(grepl(needle, readme_text, fixed = TRUE),
        paste("README.md is missing required text:", needle))
}

required_ignores <- c(
  ".DS_Store",
  ".Rhistory",
  ".RData",
  ".Rproj.user/",
  ".Ruserdata",
  ".claude/",
  "AGENTS.md",
  "quality_reports/",
  "resources/ABS_data/**/~$*",
  "renv/library/",
  "renv/staging/"
)

for (pattern in required_ignores) {
  check(pattern %in% gitignore_patterns,
        paste(".gitignore is missing local artefact pattern:", pattern))
}

tracked_source_patterns <- c(
  "pipeline/",
  "_check_cpi.R",
  "app_old.R",
  "save_plots.R",
  "plots/",
  "resources/*.pdf",
  "resources/**/*.pdf"
)

for (pattern in tracked_source_patterns) {
  check(!(pattern %in% gitignore_patterns),
        paste(".gitignore still ignores tracked project asset:", pattern))
}

ignored_tracked <- system2(
  "git",
  c("ls-files", "-ci", "--exclude-standard"),
  stdout = TRUE,
  stderr = TRUE
)
ignored_status <- attr(ignored_tracked, "status")
check(is.null(ignored_status) || ignored_status == 0,
      "git ls-files -ci --exclude-standard failed")
check(length(ignored_tracked) == 0,
      paste("Tracked files are ignored:", paste(ignored_tracked, collapse = ", ")))

tracked_r_files <- system2("git", c("ls-files", "*.R"), stdout = TRUE)
tracked_r_files <- tracked_r_files[nzchar(tracked_r_files)]
check("_check_cpi.R" %in% tracked_r_files,
      "_check_cpi.R must remain tracked and parse-valid")

for (r_file in tracked_r_files) {
  parsed <- tryCatch(
    {
      parse(file.path(repo_root, r_file))
      TRUE
    },
    error = function(e) conditionMessage(e)
  )
  check(identical(parsed, TRUE),
        paste("R file does not parse:", r_file, parsed))
}

if (length(failures) > 0) {
  stop(
    paste(c("Public release hygiene checks failed:", paste0("- ", failures)),
          collapse = "\n"),
    call. = FALSE
  )
}

cat("Public release hygiene checks passed.\n")

repo_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
failures <- character()

check <- function(condition, message) {
  if (!isTRUE(condition)) {
    failures <<- c(failures, message)
  }
}

path <- function(...) file.path(repo_root, ...)

required_files <- c(
  "DESCRIPTION",
  ".Rprofile",
  "renv.lock",
  "renv/activate.R"
)

for (file in required_files) {
  check(file.exists(path(file)), paste(file, "does not exist"))
}

required_packages <- c(
  "shiny",
  "bslib",
  "ggplot2",
  "plotly",
  "dplyr",
  "tidyr",
  "purrr",
  "stringr",
  "scales",
  "readr",
  "readxl",
  "readabs",
  "lubridate",
  "httr",
  "rlang",
  "renv",
  "watcher"
)

if (file.exists(path("DESCRIPTION"))) {
  desc <- read.dcf(path("DESCRIPTION"))[1, ]
  desc_text <- paste(desc, collapse = "\n")
  for (pkg in required_packages) {
    check(grepl(paste0("\\b", pkg, "\\b"), desc_text),
          paste("DESCRIPTION is missing required package:", pkg))
  }
}

if (file.exists(path("renv.lock"))) {
  lock_text <- paste(readLines(path("renv.lock"), warn = FALSE), collapse = "\n")
  check(grepl('"R"', lock_text, fixed = TRUE),
        "renv.lock is missing R metadata")
  check(grepl('"Version"', lock_text, fixed = TRUE),
        "renv.lock is missing version metadata")

  for (pkg in required_packages) {
    check(grepl(paste0('"', pkg, '"'), lock_text, fixed = TRUE),
          paste("renv.lock is missing required package record:", pkg))
  }
}

if (file.exists(path("README.md"))) {
  readme_text <- paste(readLines(path("README.md"), warn = FALSE), collapse = "\n")
  required_readme_text <- c(
    "renv::restore()",
    "renv.lock pins package versions",
    "install.packages"
  )
  for (needle in required_readme_text) {
    check(grepl(needle, readme_text, fixed = TRUE),
          paste("README.md is missing dependency setup text:", needle))
  }
}

gitignore_patterns <- if (file.exists(path(".gitignore"))) {
  gitignore_lines <- trimws(readLines(path(".gitignore"), warn = FALSE))
  gitignore_lines[nzchar(gitignore_lines) & !startsWith(gitignore_lines, "#")]
} else {
  character()
}

must_not_ignore <- c(
  "renv.lock",
  ".Rprofile",
  "renv/activate.R",
  "renv/settings.json",
  "DESCRIPTION"
)

for (pattern in must_not_ignore) {
  check(!(pattern %in% gitignore_patterns),
        paste(".gitignore must not ignore tracked dependency file:", pattern))
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

if (length(failures) > 0) {
  stop(
    paste(c("Dependency reproducibility checks failed:", paste0("- ", failures)),
          collapse = "\n"),
    call. = FALSE
  )
}

cat("Dependency reproducibility checks passed.\n")

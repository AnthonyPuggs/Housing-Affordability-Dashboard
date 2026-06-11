# Runs standalone via `Rscript tests/test_public_release_hygiene.R` (repo root) and under
# testthat::test_dir("tests"); see tests/helper-contracts.R.
if (!exists("contracts_harness_loaded", mode = "function")) {
  source(file.path(if (basename(getwd()) == "tests") "." else "tests",
                   "helper-contracts.R"))
}

test_that("public_release_hygiene contracts", {
  repo_root <- repo_root_path()


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
    "archive/",
    "resources/*.pdf",
    "resources/**/*.pdf"
  )

  for (pattern in tracked_source_patterns) {
    check(!(pattern %in% gitignore_patterns),
          paste(".gitignore still ignores tracked project asset:", pattern))
  }

  # -C repo_root keeps git output repo-root-relative regardless of the
  # runner's working directory (testthat::test_dir runs from tests/).
  ignored_tracked <- system2(
    "git",
    c("-C", repo_root, "ls-files", "-ci", "--exclude-standard"),
    stdout = TRUE,
    stderr = TRUE
  )
  ignored_status <- attr(ignored_tracked, "status")
  check(is.null(ignored_status) || ignored_status == 0,
        "git ls-files -ci --exclude-standard failed")
  check(length(ignored_tracked) == 0,
        paste("Tracked files are ignored:", paste(ignored_tracked, collapse = ", ")))

  # No glob pathspec here: system2() goes through sh on unix, which expands
  # an unquoted *.R in the runner's working directory (tests/ under test_dir)
  # before git ever sees it. List everything and filter in R instead.
  tracked_files <- system2("git", c("-C", repo_root, "ls-files"),
                           stdout = TRUE)
  tracked_r_files <- grep("\\.R$", tracked_files, value = TRUE)
  check("archive/_check_cpi.R" %in% tracked_r_files,
        "archive/_check_cpi.R must remain tracked and parse-valid")

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

  pipeline_contracts_path <- file.path(repo_root, "R", "pipeline_contracts.R")
  check(file.exists(pipeline_contracts_path),
        "R/pipeline_contracts.R must exist and parse")
  if (file.exists(pipeline_contracts_path)) {
    parsed <- tryCatch(
      {
        parse(pipeline_contracts_path)
        TRUE
      },
      error = function(e) conditionMessage(e)
    )
    check(identical(parsed, TRUE),
          paste("R/pipeline_contracts.R does not parse:", parsed))
  }
})
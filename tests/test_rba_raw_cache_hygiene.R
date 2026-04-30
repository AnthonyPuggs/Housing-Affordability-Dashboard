repo_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
failures <- character()

check <- function(condition, message) {
  if (!isTRUE(condition)) {
    failures <<- c(failures, message)
  }
}

source(file.path(repo_root, "pipeline", "00_config.R"))

required_helpers <- c(
  "normalise_rba_csv_cache",
  "rba_csv_parse_problem_count"
)
for (helper in required_helpers) {
  check(exists(helper, mode = "function"),
        paste("Missing RBA cache helper:", helper))
}

if (all(vapply(required_helpers, exists, logical(1), mode = "function"))) {
  fixture <- tempfile(fileext = ".csv")
  writeLines(
    c(
      "Title,A,B,C",
      "Description,Only one description",
      "Series ID,FIRMMCRTD,,",
      "Units,Per cent,Per cent,Per cent",
      "01-Jan-2024,1.00,2.00",
      "02-Jan-2024,1.10,2.10,3.10"
    ),
    fixture
  )

  before_count <- rba_csv_parse_problem_count(fixture)
  check(before_count > 0,
        "Ragged RBA fixture should have readr parse problems before normalisation")

  normalise_rba_csv_cache(fixture)
  after_count <- rba_csv_parse_problem_count(fixture)
  check(identical(after_count, 0L),
        "normalise_rba_csv_cache() should remove readr parse problems")

  normalised_once <- readLines(fixture, warn = FALSE)
  normalise_rba_csv_cache(fixture)
  normalised_twice <- readLines(fixture, warn = FALSE)
  check(identical(normalised_once, normalised_twice),
        "normalise_rba_csv_cache() should be idempotent")
}

raw_cache_files <- system2(
  "git",
  c("ls-files", "data/rba_*_raw.csv"),
  stdout = TRUE,
  stderr = TRUE
)
raw_cache_files <- raw_cache_files[nzchar(raw_cache_files)]
check(length(raw_cache_files) > 0,
      "No tracked RBA raw cache files found")

if (exists("rba_csv_parse_problem_count", mode = "function")) {
  for (cache_file in raw_cache_files) {
    problem_count <- rba_csv_parse_problem_count(file.path(repo_root, cache_file))
    check(identical(problem_count, 0L),
          paste(cache_file, "has", problem_count, "readr parse problems"))
  }
}

rba_stage_text <- paste(
  readLines(file.path(repo_root, "pipeline", "03_fetch_rba.R"), warn = FALSE),
  collapse = "\n"
)
required_stage_text <- c(
  'source(project_path("pipeline", "00_config.R"))',
  'normalise_rba_csv_cache(file)'
)
for (needle in required_stage_text) {
  check(grepl(needle, rba_stage_text, fixed = TRUE),
        paste("pipeline/03_fetch_rba.R missing required text:", needle))
}

description_lines <- readLines(file.path(repo_root, "DESCRIPTION"), warn = FALSE)
check(!any(grepl("^\\s*data\\.table\\s*,?\\s*$", description_lines)),
      "DESCRIPTION must not add data.table for RBA cache hygiene")
check(!any(grepl("^\\s*vroom\\s*,?\\s*$", description_lines)),
      "DESCRIPTION must not add vroom for RBA cache hygiene")

if (length(failures) > 0) {
  stop(
    paste(c("RBA raw cache hygiene checks failed:", paste0("- ", failures)),
          collapse = "\n"),
    call. = FALSE
  )
}

cat("RBA raw cache hygiene checks passed.\n")

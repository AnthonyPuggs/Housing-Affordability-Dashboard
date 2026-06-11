# Runs standalone via `Rscript tests/test_abs_labour_api_contracts.R` (repo root) and under
# testthat::test_dir("tests"); see tests/helper-contracts.R.
if (!exists("contracts_harness_loaded", mode = "function")) {
  source(file.path(if (basename(getwd()) == "tests") "." else "tests",
                   "helper-contracts.R"))
}

test_that("abs_labour_api_contracts contracts", {
  repo_root <- repo_root_path()


  script_path <- file.path(repo_root, "pipeline", "02_fetch_abs_timeseries.R")
  check(file.exists(script_path), "pipeline/02_fetch_abs_timeseries.R does not exist")

  script_text <- if (file.exists(script_path)) {
    paste(readLines(script_path, warn = FALSE), collapse = "\n")
  } else {
    ""
  }

  required_text <- c(
    "fetch_abs_lf_series <- function",
    "https://data.api.abs.gov.au/rest/data/",
    "\"LF\",\n  \"M12.3.1599.20.AUS.M\"",
    "\"LF\",\n  \"M13.3.1599.20.AUS.M\"",
    "\"LF_UNDER\",\n  \"M23.3.1599.20.AUS.M\"",
    "\"LF_UNDER\",\n  \"M24.3.1599.20.AUS.M\"",
    "M12.3.1599.20.AUS.M",
    "M13.3.1599.20.AUS.M",
    "M23.3.1599.20.AUS.M",
    "M24.3.1599.20.AUS.M"
  )

  missing_text <- required_text[
    !vapply(required_text, grepl, logical(1), script_text, fixed = TRUE)
  ]
  check(length(missing_text) == 0,
        paste("ABS labour SDMX fetch contract missing text:",
              paste(missing_text, collapse = "; ")))

  forbidden_text <- c(
    'read_abs(cat_no = "6202.0", tables = "1")',
    'read_abs(cat_no = "6202.0", tables = "22")'
  )

  present_forbidden <- forbidden_text[
    vapply(forbidden_text, grepl, logical(1), script_text, fixed = TRUE)
  ]
  check(length(present_forbidden) == 0,
        paste("ABS labour fetch must not depend on brittle readabs catalogue lookups:",
              paste(present_forbidden, collapse = "; ")))
})
repo_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
failures <- character()

check <- function(condition, message) {
  if (!isTRUE(condition)) {
    failures <<- c(failures, message)
  }
}

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

if (length(failures) > 0) {
  stop(
    paste(c("ABS labour API contract checks failed:", paste0("- ", failures)),
          collapse = "\n"),
    call. = FALSE
  )
}

cat("ABS labour API contract checks passed.\n")

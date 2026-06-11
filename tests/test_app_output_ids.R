# Runs standalone via `Rscript tests/test_app_output_ids.R` (repo root) and under
# testthat::test_dir("tests"); see tests/helper-contracts.R.
if (!exists("contracts_harness_loaded", mode = "function")) {
  source(file.path(if (basename(getwd()) == "tests") "." else "tests",
                   "helper-contracts.R"))
}

test_that("app_output_ids contracts", {
  repo_root <- repo_root_path()
  app_path <- file.path(repo_root, "app.R")
  check(file.exists(app_path), "app.R not found")

  module_paths <- list.files(file.path(repo_root, "R"),
                             pattern = "_module[.]R$",
                             full.names = TRUE)
  source_paths <- c(app_path, module_paths)
  app_text <- paste(vapply(source_paths, function(path) {
    paste(readLines(path, warn = FALSE), collapse = "\n")
  }, character(1)), collapse = "\n")

  extract_matches <- function(pattern, text) {
    matches <- gregexpr(pattern, text, perl = TRUE)
    values <- regmatches(text, matches)[[1]]
    if (identical(values, character(0)) || length(values) == 0) {
      return(character())
    }
    sub(pattern, "\\1", values, perl = TRUE)
  }

  ui_plotly_ids <- unique(c(extract_matches(
    "plotlyOutput\\(\\s*[\"']([A-Za-z0-9_]+)[\"']",
    app_text
  ), extract_matches(
    "plotlyOutput\\(\\s*ns\\(\\s*[\"']([A-Za-z0-9_]+)[\"']",
    app_text
  )))

  server_plotly_ids <- unique(extract_matches(
    "output\\$([A-Za-z0-9_]+)\\s*<-\\s*renderPlotly\\s*\\(",
    app_text
  ))

  missing_server <- setdiff(ui_plotly_ids, server_plotly_ids)
  check(length(missing_server) == 0,
        paste("plotlyOutput IDs without matching renderPlotly outputs:",
              paste(sort(missing_server), collapse = ", ")))
  check(length(ui_plotly_ids) > 0,
        "App must declare at least one plotlyOutput")
})

repo_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
failures <- character()

check <- function(condition, message) {
  if (!isTRUE(condition)) {
    failures <<- c(failures, message)
  }
}

helper_path <- file.path(repo_root, "R", "chart_builders.R")
description_path <- file.path(repo_root, "DESCRIPTION")

check(file.exists(helper_path), "R/chart_builders.R does not exist")
check(file.exists(description_path), "DESCRIPTION does not exist")

if (file.exists(helper_path)) {
  parsed <- tryCatch({
    parse(helper_path)
    TRUE
  }, error = function(e) conditionMessage(e))
  check(identical(parsed, TRUE),
        paste("R/chart_builders.R does not parse:", parsed))
}

suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(scales)
  library(lubridate)
})

source(file.path(repo_root, "R", "dashboard_formatting.R"))
source(file.path(repo_root, "R", "dashboard_theme.R"))
if (file.exists(helper_path)) {
  source(helper_path)
}

required_functions <- c(
  "price_series_transform",
  "build_dwelling_price_plot",
  "rent_cpi_series_transform",
  "build_rent_cpi_plot",
  "build_context_rates_plot",
  "build_context_labour_plot",
  "build_context_population_plot"
)

for (fn in required_functions) {
  check(exists(fn, mode = "function"), paste(fn, "must be defined"))
}

quarterly_dates <- as.Date(c(
  "2020-01-01", "2020-04-01", "2020-07-01", "2020-10-01", "2021-01-01"
))

price_fixture <- data.frame(
  date = rep(quarterly_dates, 2),
  value = c(100, 110, 120, 130, 150, 80, 82, 84, 86, 88),
  city = rep(c("Sydney", "Melbourne"), each = length(quarterly_dates)),
  stringsAsFactors = FALSE
)

if (exists("price_series_transform", mode = "function")) {
  price_levels <- price_series_transform(price_fixture, "levels")
  check(identical(price_levels$value, price_fixture$value),
        "price_series_transform('levels') must preserve values")

  price_yoy <- price_series_transform(price_fixture, "yoy")
  syd_yoy <- price_yoy$value[price_yoy$city == "Sydney"]
  check(nrow(price_yoy) == 2,
        "price_series_transform('yoy') must keep one fifth-quarter row per city")
  check(isTRUE(all.equal(syd_yoy, 50, tolerance = 1e-8)),
        "price_series_transform('yoy') must use four-quarter percentage change")

  price_index <- price_series_transform(price_fixture, "index")
  syd_index <- price_index$value[price_index$city == "Sydney"]
  check(isTRUE(all.equal(syd_index, c(100, 110, 120, 130, 150),
                         tolerance = 1e-8)),
        "price_series_transform('index') must index each city to its first value")
}

rent_fixture <- price_fixture

if (exists("rent_cpi_series_transform", mode = "function")) {
  rent_qoq <- rent_cpi_series_transform(rent_fixture, "qoq")
  syd_qoq <- rent_qoq$value[rent_qoq$city == "Sydney"]
  check(isTRUE(all.equal(syd_qoq,
                         c(10, 100 * (120 / 110 - 1),
                           100 * (130 / 120 - 1),
                           100 * (150 / 130 - 1)),
                         tolerance = 1e-8)),
        "rent_cpi_series_transform('qoq') must use one-quarter percentage change")

  rent_yoy <- rent_cpi_series_transform(rent_fixture, "yoy")
  check(nrow(rent_yoy) == 2,
        "rent_cpi_series_transform('yoy') must keep one fifth-quarter row per city")
}

if (exists("build_dwelling_price_plot", mode = "function")) {
  p <- build_dwelling_price_plot(price_fixture, "levels", dark = FALSE)
  check(inherits(p, "ggplot"), "build_dwelling_price_plot() must return a ggplot")
}

if (exists("build_rent_cpi_plot", mode = "function")) {
  p <- build_rent_cpi_plot(rent_fixture, "index", dark = TRUE)
  check(inherits(p, "ggplot"), "build_rent_cpi_plot() must return a ggplot")
}

rates_fixture <- data.frame(
  date = quarterly_dates,
  value = c(1, 2, 3, 4, 5),
  series = "RBA Cash Rate",
  stringsAsFactors = FALSE
)

labour_fixture <- data.frame(
  date = rep(quarterly_dates, 3),
  value = rep(c(4, 6, 10, 8, 7), 3),
  series = rep(c("Unemployment Rate", "Underemployment Rate",
                 "Labour Underutilisation Rate"),
               each = length(quarterly_dates)),
  stringsAsFactors = FALSE
)

population_fixture <- data.frame(
  date = quarterly_dates,
  value = c(10, 15, 20, 25, 30),
  stringsAsFactors = FALSE
)

if (exists("build_context_rates_plot", mode = "function")) {
  check(inherits(build_context_rates_plot(rates_fixture, dark = FALSE), "ggplot"),
        "build_context_rates_plot() must return a ggplot")
}

if (exists("build_context_labour_plot", mode = "function")) {
  check(inherits(build_context_labour_plot(labour_fixture, dark = TRUE), "ggplot"),
        "build_context_labour_plot() must return a ggplot")
}

if (exists("build_context_population_plot", mode = "function")) {
  check(inherits(build_context_population_plot(population_fixture, dark = FALSE),
                 "ggplot"),
        "build_context_population_plot() must return a ggplot")
}

if (file.exists(description_path)) {
  desc_text <- paste(readLines(description_path, warn = FALSE), collapse = "\n")
  forbidden_direct_dependencies <- c("ggthemes", "patchwork", "cowplot", "DT")
  unexpected <- forbidden_direct_dependencies[
    vapply(forbidden_direct_dependencies, function(pkg) {
      grepl(paste0("(^|\\n)\\s*", pkg, "\\s*,?\\s*($|\\n)"),
            desc_text, perl = TRUE)
    }, logical(1))
  ]
  check(length(unexpected) == 0,
        paste("DESCRIPTION must not add new direct dependencies:",
              paste(unexpected, collapse = ", ")))
}

if (length(failures) > 0) {
  stop(
    paste(c("Chart builder checks failed:", paste0("- ", failures)),
          collapse = "\n"),
    call. = FALSE
  )
}

cat("Chart builder checks passed.\n")

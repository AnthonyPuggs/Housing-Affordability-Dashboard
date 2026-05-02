repo_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
failures <- character()

check <- function(condition, message) {
  if (!isTRUE(condition)) {
    failures <<- c(failures, message)
  }
}

expected_columns <- function(object, columns, label) {
  missing <- setdiff(columns, names(object))
  check(length(missing) == 0,
        paste(label, "is missing columns:", paste(missing, collapse = ", ")))
}

required_modules <- c(
  "R/data_loader.R",
  "R/dashboard_formatting.R",
  "R/dashboard_theme.R",
  "R/precomputed_series.R"
)

required_functions <- c(
  load_dashboard_csvs = "R/data_loader.R",
  latest_change = "R/dashboard_formatting.R",
  theme_afford = "R/dashboard_theme.R",
  plotly_layout = "R/dashboard_theme.R",
  precompute_dashboard_series = "R/precomputed_series.R"
)

for (path in required_modules) {
  full_path <- file.path(repo_root, path)
  check(file.exists(full_path), paste(path, "does not exist"))
  if (file.exists(full_path)) {
    parsed <- tryCatch({
      parse(full_path)
      TRUE
    }, error = function(e) conditionMessage(e))
    check(identical(parsed, TRUE), paste(path, "does not parse:", parsed))
  }
}

helper_env <- new.env(parent = globalenv())
source(file.path(repo_root, "R", "project_paths.R"), local = helper_env)
for (path in required_modules[file.exists(file.path(repo_root, required_modules))]) {
  source(file.path(repo_root, path), local = helper_env)
}

for (fn in names(required_functions)) {
  check(exists(fn, envir = helper_env, mode = "function", inherits = FALSE),
        paste(fn, "must be defined by", required_functions[[fn]]))
}

plot_env <- new.env(parent = globalenv())
plot_result <- tryCatch({
  source(file.path(repo_root, "plot_setup.R"), local = plot_env)
  TRUE
}, error = function(e) conditionMessage(e))

check(identical(plot_result, TRUE),
      paste("plot_setup.R failed when sourced:", plot_result))

if (identical(plot_result, TRUE)) {
  expected_globals <- c(
    "abs_ts", "rba_rates", "afford_idx", "supply_demand",
    "sih_national", "sih_state_ts", "sih_costs", "sih_cost_ratios",
    "sih_stress", "sih_nhha", "sih_quality",
    "sih_lower_income_states", "sih_geographic",
    "rppi_all", "rppi_houses", "rppi_units", "rppi_combined",
    "median_house_prices", "national_mean_price", "median_prices_combined",
    "awe_ts", "rppi_national_ts", "mortgage_rate_qtr",
    "serviceability_ts", "afford_change", "rent_cpi_combined",
    "rent_cpi_cities", "rppi_cities", "rba_cash_rate",
    "rba_mortgage_var", "rba_mortgage_fixed", "rba_mortgage_std",
    "rba_investor_var", "rba_investor_fixed"
  )

  missing_globals <- expected_globals[
    !vapply(expected_globals, exists, logical(1), envir = plot_env,
            inherits = FALSE)
  ]
  check(length(missing_globals) == 0,
        paste("plot_setup.R no longer creates globals:",
              paste(missing_globals, collapse = ", ")))

  if (exists("abs_ts", envir = plot_env)) {
    expected_columns(get("abs_ts", envir = plot_env),
                     c("date", "value", "series", "series_id", "category",
                       "unit", "frequency"),
                     "abs_ts")
  }
  if (exists("afford_idx", envir = plot_env)) {
    expected_columns(get("afford_idx", envir = plot_env),
                     c("date", "value", "indicator", "geography", "unit",
                       "frequency"),
                     "afford_idx")
  }
  if (exists("serviceability_ts", envir = plot_env)) {
    expected_columns(get("serviceability_ts", envir = plot_env),
                     c("date", "serviceability_pct"),
                     "serviceability_ts")
  }
}

plot_setup_text <- paste(readLines(file.path(repo_root, "plot_setup.R"),
                                   warn = FALSE),
                         collapse = "\n")

required_sources <- c(
  'source(project_path("R", "data_loader.R")',
  'source(project_path("R", "dashboard_formatting.R")',
  'source(project_path("R", "dashboard_theme.R")',
  'source(project_path("R", "precomputed_series.R")'
)

for (needle in required_sources) {
  check(grepl(needle, plot_setup_text, fixed = TRUE),
        paste("plot_setup.R must source", needle))
}

forbidden_direct_blocks <- c(
  "latest_change <- function",
  "theme_afford <- function",
  "plotly_layout <- function",
  "read_csv(path",
  "rppi_all <- abs_ts %>%",
  "serviceability_ts <- rppi_national_ts %>%"
)

for (needle in forbidden_direct_blocks) {
  check(!grepl(needle, plot_setup_text, fixed = TRUE),
        paste("plot_setup.R should delegate this block instead of defining:",
              needle))
}

theme_source_pos <- regexpr('source(project_path("R", "dashboard_theme.R")',
                            plot_setup_text, fixed = TRUE)[[1]]
plotly_helper_source_pos <- regexpr(
  'source(project_path("R", "plotly_helpers.R"), local = TRUE)',
  paste(readLines(file.path(repo_root, "app.R"), warn = FALSE), collapse = "\n"),
  fixed = TRUE
)[[1]]
check(theme_source_pos > 0, "plot_setup.R must source dashboard_theme.R")
check(plotly_helper_source_pos > 0, "app.R must source plotly_helpers.R")

if (length(failures) > 0) {
  stop(
    paste(c("Plot setup extraction checks failed:", paste0("- ", failures)),
          collapse = "\n"),
    call. = FALSE
  )
}

cat("Plot setup extraction checks passed.\n")

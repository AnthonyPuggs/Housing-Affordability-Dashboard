repo_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
data_dir <- file.path(repo_root, "data")
plot_setup_path <- file.path(repo_root, "plot_setup.R")
failures <- character()

check <- function(condition, message) {
  if (!isTRUE(condition)) {
    failures <<- c(failures, message)
  }
}

required_columns <- c(
  "survey_year", "value", "metric", "tenure", "breakdown_var",
  "breakdown_val", "geography", "stat_type"
)

read_dashboard_csv <- function(filename) {
  path <- file.path(data_dir, filename)
  check(file.exists(path), paste(filename, "does not exist"))
  if (!file.exists(path)) {
    return(data.frame())
  }
  suppressWarnings(read.csv(path, stringsAsFactors = FALSE, check.names = FALSE))
}

check_schema <- function(df, filename) {
  missing_cols <- setdiff(required_columns, names(df))
  check(length(missing_cols) == 0,
        paste(filename, "missing required columns:",
              paste(missing_cols, collapse = ", ")))
  if ("value" %in% names(df)) {
    check(all(is.finite(df$value)),
          paste(filename, "contains non-finite values"))
  }
}

duplicate_count <- function(df, key_cols) {
  if (nrow(df) == 0) {
    return(0L)
  }
  sum(duplicated(df[, key_cols, drop = FALSE]))
}

page_estimate_slice <- function(df, key_cols) {
  if (nrow(df) == 0) {
    return(df)
  }
  df <- df[order(df$value, decreasing = TRUE), , drop = FALSE]
  df[!duplicated(df[, key_cols, drop = FALSE]), , drop = FALSE]
}

state_ts <- read_dashboard_csv("sih_state_timeseries.csv")
lower_income <- read_dashboard_csv("sih_lower_income_states.csv")
geographic <- read_dashboard_csv("sih_geographic_2020.csv")

check_schema(state_ts, "sih_state_timeseries.csv")
check_schema(lower_income, "sih_lower_income_states.csv")
check_schema(geographic, "sih_geographic_2020.csv")

if (nrow(state_ts) > 0) {
  check(all(c("cost_income_ratio", "mean_weekly_cost_real") %in%
              unique(state_ts$metric)),
        "sih_state_timeseries.csv must include cost_income_ratio and mean_weekly_cost_real")
  required_states <- c("New South Wales", "Victoria", "Queensland",
                       "Western Australia", "Australian Capital Territory")
  check(all(required_states %in% unique(state_ts$geography)),
        "sih_state_timeseries.csv missing expected states/territories")

  state_page <- state_ts[
    state_ts$breakdown_var == "tenure" &
      state_ts$metric %in% c("cost_income_ratio", "mean_weekly_cost_real") &
      state_ts$tenure %in% c("renter_total", "owner_mortgage", "all"),
  ]
  state_page <- page_estimate_slice(state_page, c("survey_year", "metric",
                                                  "tenure", "geography"))
  check(nrow(state_page) > 0,
        "State trend page subset must not be empty")
  check(duplicate_count(state_page, c("survey_year", "metric",
                                      "tenure", "geography")) == 0,
        "State trend page-ready subset has duplicate logical keys")
}

if (nrow(lower_income) > 0) {
  check(all(c("pct_over_30", "median_cost_income_ratio",
              "median_weekly_cost") %in% unique(lower_income$metric)),
        "sih_lower_income_states.csv missing lower-income affordability metrics")
  check(all(c("Australia", "New South Wales", "Victoria") %in%
              unique(lower_income$geography)),
        "sih_lower_income_states.csv missing expected geographies")

  lower_page <- lower_income[
    lower_income$metric %in% c("pct_over_30", "median_cost_income_ratio",
                               "median_weekly_cost") &
      lower_income$tenure %in% c("renter_total", "owner_mortgage", "all"),
  ]
  lower_page <- page_estimate_slice(lower_page, c("survey_year", "metric",
                                                  "tenure", "geography"))
  check(nrow(lower_page) > 0,
        "Lower-income state page subset must not be empty")
  check(duplicate_count(lower_page, c("survey_year", "metric",
                                      "tenure", "geography")) == 0,
        "Lower-income state page-ready subset has duplicate logical keys")
}

if (nrow(geographic) > 0) {
  check(all(c("median_cost_income_ratio", "median_weekly_cost",
              "mean_cost_income_ratio", "mean_weekly_cost") %in%
              unique(geographic$metric)),
        "sih_geographic_2020.csv missing capital/rest-of-state metrics")
  check(all(c("Gr. Sydney", "Rest of NSW", "Total GCC",
              "Total rest of state") %in% unique(geographic$geography)),
        "sih_geographic_2020.csv missing expected geographic breakdowns")

  geographic_page <- geographic[
    geographic$breakdown_var %in% c("owner", "renter") &
      geographic$metric %in% c("median_cost_income_ratio",
                               "median_weekly_cost",
                               "mean_cost_income_ratio",
                               "mean_weekly_cost") &
      geographic$tenure %in% c("renter_total", "owner_mortgage", "all"),
  ]
  geographic_page <- page_estimate_slice(geographic_page, c("survey_year",
                                                            "metric",
                                                            "tenure",
                                                            "geography"))
  check(nrow(geographic_page) > 0,
        "Capital/rest-of-state page subset must not be empty")
  check(duplicate_count(geographic_page, c("survey_year", "metric",
                                           "tenure", "geography")) == 0,
        "Capital/rest-of-state page-ready subset has duplicate logical keys")
}

check(file.exists(plot_setup_path), "plot_setup.R does not exist")
if (file.exists(plot_setup_path)) {
  plot_setup_text <- paste(readLines(plot_setup_path, warn = FALSE),
                           collapse = "\n")
  required_plot_setup_text <- c(
    'sih_lower_income_states <- load_csv("sih_lower_income_states.csv")',
    'sih_geographic          <- load_csv("sih_geographic_2020.csv")'
  )
  missing_plot_setup <- required_plot_setup_text[
    !vapply(required_plot_setup_text, grepl, logical(1),
            plot_setup_text, fixed = TRUE)
  ]
  check(length(missing_plot_setup) == 0,
        paste("plot_setup.R must load reserved SIH geographic files:",
              paste(missing_plot_setup, collapse = "; ")))
}

if (length(failures) > 0) {
  stop(
    paste(c("Geographic Affordability data contract checks failed:",
            paste0("- ", failures)),
          collapse = "\n"),
    call. = FALSE
  )
}

cat("Geographic Affordability data contract checks passed.\n")

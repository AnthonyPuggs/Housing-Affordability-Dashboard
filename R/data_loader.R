# Dashboard CSV loading helpers.
#
# Fail-loud contract: a missing, unparseable, zero-row or column-broken CSV
# produces a warning naming the file and returns a typed empty tibble (so
# downstream filters and joins degrade to friendly validate() messages instead
# of cryptic "object not found" startup errors), and assert_dashboard_data()
# turns the collected problems into a single named startup failure listing
# every broken file at once.

if (!exists("pipeline_stage_schema", mode = "function")) {
  pipeline_contracts_path <- if (exists("project_path", mode = "function")) {
    project_path("R", "pipeline_contracts.R")
  } else {
    file.path("R", "pipeline_contracts.R")
  }
  if (file.exists(pipeline_contracts_path)) {
    # local = TRUE keeps the helpers in the same environment chain as this
    # file's caller (plot_setup/app), where project_path() is defined - a
    # global-env source would re-resolve paths relative to the working
    # directory and break neutral-cwd startup.
    source(pipeline_contracts_path, local = TRUE)
  }
}

dashboard_csv_files <- function() {
  c(
    abs_ts = "abs_timeseries.csv",
    rba_rates = "rba_rates.csv",
    afford_idx = "affordability_indices.csv",
    supply_demand = "abs_supply_demand.csv",
    sih_national = "sih_timeseries_national.csv",
    sih_state_ts = "sih_state_timeseries.csv",
    sih_costs = "sih_costs_2020.csv",
    sih_cost_ratios = "sih_cost_ratios_2020.csv",
    sih_stress = "sih_stress_bands_2020.csv",
    sih_nhha = "sih_nhha_rental_stress.csv",
    sih_quality = "sih_estimate_quality.csv",
    sih_lower_income_states = "sih_lower_income_states.csv",
    sih_recent_buyers = "sih_recent_buyers_2020.csv",
    sih_geographic = "sih_geographic_2020.csv"
  )
}

# Zero-row tibble carrying the file's contract columns with sensible types,
# so filters, joins and mutate() calls on a broken input fail soft.
empty_dashboard_tibble <- function(filename) {
  columns <- if (exists("pipeline_stage_schema", mode = "function")) {
    pipeline_stage_schema(filename)
  } else {
    character()
  }
  values <- lapply(columns, function(column) {
    if (identical(column, "date")) {
      as.Date(character())
    } else if (column %in% c("value", "quality_value")) {
      numeric()
    } else {
      character()
    }
  })
  names(values) <- columns
  tibble::as_tibble(values)
}

dashboard_csv_problem <- function(filename, data_dir) {
  path <- file.path(data_dir, filename)
  rel <- file.path("data", filename)

  if (!file.exists(path)) {
    return(list(problem = paste(rel, "is missing"), data = NULL))
  }

  data <- tryCatch(
    readr::read_csv(path, show_col_types = FALSE, progress = FALSE),
    error = function(e) e
  )
  if (inherits(data, "error")) {
    return(list(
      problem = paste0(rel, " could not be parsed: ", conditionMessage(data)),
      data = NULL
    ))
  }

  if (nrow(data) == 0) {
    return(list(problem = paste(rel, "has zero rows"), data = NULL))
  }

  required_columns <- if (exists("pipeline_stage_schema", mode = "function")) {
    pipeline_stage_schema(filename)
  } else {
    character()
  }
  missing_columns <- setdiff(required_columns, names(data))
  if (length(missing_columns) > 0) {
    return(list(
      problem = paste(rel, "is missing columns:",
                      paste(missing_columns, collapse = ", ")),
      data = NULL
    ))
  }

  list(problem = NULL, data = data)
}

load_dashboard_csv <- function(filename, data_dir = project_path("data")) {
  result <- dashboard_csv_problem(filename, data_dir)
  if (is.null(result$problem)) {
    return(result$data)
  }

  warning(
    result$problem,
    " - run 'Rscript pipeline/05_driver.R' to regenerate; serving a typed empty placeholder.",
    call. = FALSE
  )
  out <- empty_dashboard_tibble(filename)
  attr(out, "data_problem") <- result$problem
  out
}

load_dashboard_csvs <- function(data_dir = project_path("data")) {
  files <- dashboard_csv_files()
  out <- lapply(unname(files), load_dashboard_csv, data_dir = data_dir)
  names(out) <- names(files)
  problems <- unlist(
    lapply(out, function(d) attr(d, "data_problem")),
    use.names = FALSE
  )
  attr(out, "data_problems") <- problems
  out
}

# Startup assertion: refuse to launch on broken inputs with one message that
# names every problem file, instead of a cryptic downstream error.
assert_dashboard_data <- function(dashboard_data) {
  problems <- attr(dashboard_data, "data_problems")
  if (is.null(problems) || length(problems) == 0) {
    return(invisible(TRUE))
  }
  stop(
    paste(
      c("Dashboard startup failed - broken data inputs:",
        paste0("- ", problems),
        "Run 'Rscript pipeline/05_driver.R' to regenerate data/, or restore the files from git."),
      collapse = "\n"
    ),
    call. = FALSE
  )
}

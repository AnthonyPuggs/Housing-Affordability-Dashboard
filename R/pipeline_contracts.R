# ==============================================================================
# Pipeline stage contracts and external source manifest
# ==============================================================================

if (!exists("project_root", mode = "function") ||
    !exists("project_path", mode = "function")) {
  source(file.path("R", "project_paths.R"))
}

pipeline_stage_contracts <- function() {
  contracts <- data.frame(
    stage_id = c("sih", "abs_timeseries", "abs_supply", "rba", "indicators"),
    label = c(
      "ABS Survey of Income and Housing workbooks",
      "ABS live time series",
      "ABS supply and demand live time series",
      "RBA rates tables",
      "Derived affordability indicators"
    ),
    script = c(
      "pipeline/01_process_sih.R",
      "pipeline/02_fetch_abs_timeseries.R",
      "pipeline/02b_fetch_abs_supply.R",
      "pipeline/03_fetch_rba.R",
      "pipeline/04_derive_indicators.R"
    ),
    stringsAsFactors = FALSE
  )

  contracts$required_outputs <- I(list(
    c(
      "sih_timeseries_national.csv",
      "sih_state_timeseries.csv",
      "sih_costs_2020.csv",
      "sih_cost_ratios_2020.csv",
      "sih_stress_bands_2020.csv",
      "sih_age_tenure_2020.csv",
      "sih_lower_income_states.csv",
      "sih_recent_buyers_2020.csv",
      "sih_geographic_2020.csv",
      "sih_nhha_rental_stress.csv",
      "sih_estimate_quality.csv"
    ),
    "abs_timeseries.csv",
    "abs_supply_demand.csv",
    "rba_rates.csv",
    "affordability_indices.csv"
  ))

  contracts
}

pipeline_external_sources <- function() {
  data.frame(
    source_id = c(
      "abs-6432-table-1",
      "abs-6432-table-2",
      "abs-6401-table-7",
      "abs-6401-table-10",
      "abs-sdmx-cpi-rents",
      "abs-sdmx-cpi-all-groups",
      "abs-6345-table-1",
      "abs-awe-helper",
      "abs-5206-table-2",
      "abs-5206-table-7",
      "abs-6202-table-1",
      "abs-6202-table-22",
      "abs-3101-table-1",
      "abs-3101-table-2",
      "abs-8731-table-1",
      "abs-8731-table-2",
      "rba-f1",
      "rba-f5",
      "rba-f6"
    ),
    provider = c(
      rep("ABS", 16),
      rep("RBA", 3)
    ),
    source_type = c(
      rep("readabs catalogue table", 4),
      rep("ABS SDMX CPI endpoint", 2),
      rep("readabs catalogue table", 2),
      rep("readabs catalogue table", 4),
      rep("readabs catalogue table", 4),
      rep("RBA statistical table CSV/XLSX", 3)
    ),
    source_reference = c(
      "ABS 6432.0 Table 1",
      "ABS 6432.0 Table 2",
      "ABS 6401.0 Table 7",
      "ABS 6401.0 Table 10",
      "ABS SDMX CPI/1.115522.10.50.Q",
      "ABS SDMX CPI/1.10001.10.50.Q",
      "ABS 6345.0 Table 1",
      "ABS AWE readabs helper",
      "ABS 5206.0 Table 2",
      "ABS 5206.0 Table 7",
      "ABS 6202.0 Table 1",
      "ABS 6202.0 Table 22",
      "ABS 3101.0 Table 1",
      "ABS 3101.0 Table 2",
      "ABS 8731.0 Table 1",
      "ABS 8731.0 Table 2",
      "RBA F1",
      "RBA F5",
      "RBA F6"
    ),
    consuming_script = c(
      rep("pipeline/02_fetch_abs_timeseries.R", 12),
      rep("pipeline/02b_fetch_abs_supply.R", 4),
      rep("pipeline/03_fetch_rba.R", 3)
    ),
    output_file = c(
      rep("data/abs_timeseries.csv", 12),
      rep("data/abs_supply_demand.csv", 4),
      rep("data/rba_rates.csv", 3)
    ),
    stringsAsFactors = FALSE
  )
}

pipeline_stage_schema <- function(filename) {
  time_series_schema <- c(
    "date", "value", "series", "series_id", "category", "unit", "frequency"
  )
  affordability_schema <- c(
    "date", "value", "indicator", "geography", "unit", "frequency"
  )
  sih_schema <- c(
    "survey_year", "value", "metric", "tenure", "breakdown_var",
    "breakdown_val", "geography", "stat_type"
  )
  sih_quality_schema <- c(
    "source_file", "source_table", "survey_year", "metric", "tenure",
    "breakdown_var", "breakdown_val", "geography", "stat_type",
    "quality_measure", "quality_value", "quality_unit", "reliability_flag",
    "reliability_note"
  )

  if (filename %in% c("abs_timeseries.csv", "abs_supply_demand.csv",
                      "rba_rates.csv")) {
    return(time_series_schema)
  }
  if (identical(filename, "affordability_indices.csv")) {
    return(affordability_schema)
  }
  if (identical(filename, "sih_estimate_quality.csv")) {
    return(sih_quality_schema)
  }
  sih_schema
}

validate_pipeline_stage_outputs <- function(stage_id,
                                            data_dir = project_path("data"),
                                            fail = FALSE) {
  contracts <- pipeline_stage_contracts()
  if (!stage_id %in% contracts$stage_id) {
    stop("Unknown pipeline stage ID: ", stage_id, call. = FALSE)
  }

  contract <- contracts[contracts$stage_id == stage_id, , drop = FALSE]
  required_outputs <- contract$required_outputs[[1]]
  failures <- character()

  for (filename in required_outputs) {
    path <- file.path(data_dir, filename)
    rel <- file.path("data", filename)
    if (!file.exists(path)) {
      failures <- c(failures, paste(rel, "is missing"))
      next
    }

    data <- tryCatch(
      readr::read_csv(path, show_col_types = FALSE, progress = FALSE),
      error = function(e) e
    )
    if (inherits(data, "error")) {
      failures <- c(
        failures,
        paste(rel, "could not be parsed:", conditionMessage(data))
      )
      next
    }

    if (nrow(data) == 0) {
      failures <- c(failures, paste(rel, "has zero rows"))
    }

    required_columns <- pipeline_stage_schema(filename)
    missing_columns <- setdiff(required_columns, names(data))
    if (length(missing_columns) > 0) {
      failures <- c(
        failures,
        paste(rel, "is missing columns:",
              paste(missing_columns, collapse = ", "))
      )
    }
  }

  if (isTRUE(fail) && length(failures) > 0) {
    stop(
      paste(c(paste0("Pipeline stage '", stage_id, "' failed output contract:"),
              paste0("- ", failures)),
            collapse = "\n"),
      call. = FALSE
    )
  }

  failures
}

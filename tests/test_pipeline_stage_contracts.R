repo_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
failures <- character()

check <- function(condition, message) {
  if (!isTRUE(condition)) {
    failures <<- c(failures, message)
  }
}

path <- function(...) file.path(repo_root, ...)

project_paths_path <- path("R", "project_paths.R")
contracts_path <- path("R", "pipeline_contracts.R")
config_path <- path("pipeline", "00_config.R")

check(file.exists(project_paths_path), "R/project_paths.R does not exist")
check(file.exists(contracts_path), "R/pipeline_contracts.R does not exist")
check(file.exists(config_path), "pipeline/00_config.R does not exist")

if (file.exists(contracts_path)) {
  parsed <- tryCatch({
    parse(contracts_path)
    TRUE
  }, error = function(e) conditionMessage(e))
  check(identical(parsed, TRUE),
        paste("R/pipeline_contracts.R does not parse:", parsed))
}

if (all(file.exists(c(project_paths_path, contracts_path)))) {
  source(project_paths_path)
  source(contracts_path)

  required_functions <- c(
    "pipeline_stage_contracts",
    "pipeline_external_sources",
    "validate_pipeline_stage_outputs"
  )
  missing_functions <- required_functions[
    !vapply(required_functions, exists, logical(1), mode = "function")
  ]
  check(length(missing_functions) == 0,
        paste("R/pipeline_contracts.R missing functions:",
              paste(missing_functions, collapse = ", ")))

  if (exists("pipeline_stage_contracts", mode = "function")) {
    contracts <- pipeline_stage_contracts()
    expected_columns <- c("stage_id", "label", "script", "required_outputs")
    check(identical(names(contracts), expected_columns),
          paste("pipeline_stage_contracts() must return schema:",
                paste(expected_columns, collapse = " | ")))

    required_stage_ids <- c(
      "sih",
      "abs_timeseries",
      "abs_supply",
      "rba",
      "indicators"
    )
    missing_stage_ids <- setdiff(required_stage_ids, contracts$stage_id)
    check(length(missing_stage_ids) == 0,
          paste("pipeline_stage_contracts() missing stage IDs:",
                paste(missing_stage_ids, collapse = ", ")))

    outputs_by_stage <- stats::setNames(
      contracts$required_outputs,
      contracts$stage_id
    )
    expected_stage_outputs <- list(
      sih = c(
        "sih_timeseries_national.csv",
        "sih_state_timeseries.csv",
        "sih_costs_2020.csv",
        "sih_cost_ratios_2020.csv",
        "sih_stress_bands_2020.csv",
        "sih_lower_income_states.csv",
        "sih_geographic_2020.csv",
        "sih_nhha_rental_stress.csv",
        "sih_estimate_quality.csv"
      ),
      abs_timeseries = "abs_timeseries.csv",
      abs_supply = "abs_supply_demand.csv",
      rba = "rba_rates.csv",
      indicators = "affordability_indices.csv"
    )
    for (stage_id in names(expected_stage_outputs)) {
      actual_outputs <- outputs_by_stage[[stage_id]]
      missing_outputs <- setdiff(expected_stage_outputs[[stage_id]],
                                 actual_outputs)
      check(length(missing_outputs) == 0,
            paste(stage_id, "contract missing outputs:",
                  paste(missing_outputs, collapse = ", ")))
    }
  }

  if (exists("validate_pipeline_stage_outputs", mode = "function") &&
      exists("pipeline_stage_contracts", mode = "function")) {
    contracts <- pipeline_stage_contracts()
    for (stage_id in contracts$stage_id) {
      result <- tryCatch(
        validate_pipeline_stage_outputs(stage_id, data_dir = project_path("data")),
        error = function(e) e
      )
      check(!inherits(result, "error"),
            paste(stage_id, "contract errored on current data:",
                  conditionMessage(result)))
      if (!inherits(result, "error")) {
        check(length(result) == 0,
              paste(stage_id, "contract failed on current data:",
                    paste(result, collapse = "; ")))
      }
    }

    empty_dir <- tempfile("empty-stage-contract-")
    dir.create(empty_dir)
    empty_result <- tryCatch(
      validate_pipeline_stage_outputs("abs_timeseries", data_dir = empty_dir),
      error = function(e) conditionMessage(e)
    )
    check(length(empty_result) > 0,
          "validate_pipeline_stage_outputs() must fail on an empty data directory")
  }

  if (exists("pipeline_external_sources", mode = "function")) {
    sources <- pipeline_external_sources()
    expected_source_columns <- c(
      "source_id",
      "provider",
      "source_type",
      "source_reference",
      "consuming_script",
      "output_file"
    )
    check(identical(names(sources), expected_source_columns),
          paste("pipeline_external_sources() must return schema:",
                paste(expected_source_columns, collapse = " | ")))
    source_text <- paste(apply(sources, 1, paste, collapse = " "),
                         collapse = "\n")
    required_sources <- c(
      "ABS 6432.0",
      "ABS 6401.0",
      "ABS 6345.0",
      "ABS 5206.0",
      "ABS 6202.0",
      "ABS 3101.0",
      "ABS 8731.0",
      "ABS SDMX CPI",
      "RBA F1",
      "RBA F5",
      "RBA F6"
    )
    missing_sources <- required_sources[
      !vapply(required_sources, grepl, logical(1), source_text, fixed = TRUE)
    ]
    check(length(missing_sources) == 0,
          paste("pipeline_external_sources() missing sources:",
                paste(missing_sources, collapse = ", ")))
    check(!grepl("/Users/", source_text, fixed = TRUE),
          "pipeline_external_sources() must not contain local /Users/ paths")
  }
}

if (file.exists(config_path)) {
  config_text <- paste(readLines(config_path, warn = FALSE), collapse = "\n")
  check(grepl("safe_read <- function(expr, label, warn = TRUE, required = FALSE)",
              config_text, fixed = TRUE),
        "safe_read() must expose required = FALSE for fail-fast source reads")
  check(grepl("Required source failed for", config_text, fixed = TRUE),
        "safe_read() must stop clearly when a required source fails")
}

required_source_scripts <- c(
  path("pipeline", "02_fetch_abs_timeseries.R"),
  path("pipeline", "02b_fetch_abs_supply.R"),
  path("pipeline", "03_fetch_rba.R")
)
for (script in required_source_scripts) {
  check(file.exists(script), paste(basename(script), "does not exist"))
  if (file.exists(script)) {
    script_text <- paste(readLines(script, warn = FALSE), collapse = "\n")
    check(grepl("required = TRUE", script_text, fixed = TRUE) ||
            grepl("Required source failed for", script_text, fixed = TRUE),
          paste(basename(script), "must mark core live sources as required"))
  }
}

if (length(failures) > 0) {
  stop(
    paste(c("Pipeline stage contract checks failed:", paste0("- ", failures)),
          collapse = "\n"),
    call. = FALSE
  )
}

cat("Pipeline stage contract checks passed.\n")

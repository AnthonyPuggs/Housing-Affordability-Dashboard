repo_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
failures <- character()

check <- function(condition, message) {
  if (!isTRUE(condition)) {
    failures <<- c(failures, message)
  }
}

project_paths_path <- file.path(repo_root, "R", "project_paths.R")
registry_path <- file.path(repo_root, "R", "indicator_registry.R")
report_path <- file.path(repo_root, "R", "provenance_report.R")
contracts_path <- file.path(repo_root, "R", "pipeline_contracts.R")
data_vintage_path <- file.path(repo_root, "R", "data_vintage.R")

check(file.exists(project_paths_path), "R/project_paths.R does not exist")
check(file.exists(registry_path), "R/indicator_registry.R does not exist")
check(file.exists(report_path), "R/provenance_report.R does not exist")
check(file.exists(contracts_path), "R/pipeline_contracts.R does not exist")
check(file.exists(data_vintage_path), "R/data_vintage.R does not exist")

if (file.exists(report_path)) {
  parsed <- tryCatch({
    parse(report_path)
    TRUE
  }, error = function(e) conditionMessage(e))
  check(identical(parsed, TRUE),
        paste("R/provenance_report.R does not parse:", parsed))
}

if (all(file.exists(c(project_paths_path, registry_path, report_path,
                      contracts_path, data_vintage_path)))) {
  source(project_paths_path)
  source(registry_path)
  source(contracts_path)
  source(data_vintage_path)
  source(report_path)

  required_functions <- c(
    "dashboard_data_inventory",
    "methodology_provenance_report",
    "methodology_provenance_filename"
  )
  missing_functions <- required_functions[
    !vapply(required_functions, exists, logical(1), mode = "function")
  ]
  check(length(missing_functions) == 0,
        paste("R/provenance_report.R missing functions:",
              paste(missing_functions, collapse = ", ")))

  if (exists("dashboard_data_inventory", mode = "function")) {
    inventory <- dashboard_data_inventory(project_path("data"))
    required_inventory_columns <- c(
      "file",
      "rows",
      "columns",
      "date_min",
      "date_max",
      "modified_utc"
    )
    missing_inventory_columns <- setdiff(required_inventory_columns,
                                         names(inventory))
    check(length(missing_inventory_columns) == 0,
          paste("dashboard_data_inventory() missing columns:",
                paste(missing_inventory_columns, collapse = ", ")))

    required_files <- file.path("data", c(
      "affordability_indices.csv",
      "abs_timeseries.csv",
      "rba_rates.csv",
      "sih_estimate_quality.csv",
      "sih_nhha_rental_stress.csv",
      "sih_recent_buyers_2020.csv"
    ))
    missing_files <- setdiff(required_files, inventory$file)
    check(length(missing_files) == 0,
          paste("dashboard_data_inventory() missing key files:",
                paste(missing_files, collapse = ", ")))
  }

  if (exists("methodology_provenance_report", mode = "function")) {
    report <- methodology_provenance_report(
      generated_at = as.POSIXct("2026-05-01 10:00:00", tz = "UTC"),
      data_dir = project_path("data")
    )
    required_report_text <- c(
      "R/indicator_registry.R",
      "pipeline/05_driver.R",
      "pipeline/06_validate_outputs.R",
      "data/*.csv",
      "## Data Vintage",
      "Data refreshed",
      "Latest ABS/RBA observation",
      "SIH:",
      "data/data_vintage.csv",
      "Stylised scenario",
      "Assessment buffer and expense inputs are sensitivity assumptions, not a lender assessment",
      "Deposit, LVR and loan-term controls are stylised serviceability assumptions",
      "serviceability chart uses AWE individual earnings as the income proxy",
      "KPI colours encode economic interpretation",
      "better, worse or neutral/contextual",
      "R/market_entry_scenarios.R",
      "## External Data Sources",
      "## Data Source Audit",
      "Household disposable income",
      "New-tenancy or advertised rents",
      "Residual income / living costs",
      "ABS Household income and wealth",
      "NSW rental bond lodgement data",
      "Queensland RTA median rents",
      "No new live external data ingestion is activated by this audit",
      "ABS 6432.0",
      "ABS SDMX CPI",
      "RBA F1",
      "RBA F5",
      "RBA F6",
      "Chart-level reliability markers",
      "visible error bars and interval hover text use 95% margin-of-error metadata",
      "data/sih_estimate_quality.csv",
      "Price-to-Income Cost Pressure",
      "Modelled Mortgage Cost Pressure",
      "Rent Cost Pressure",
      "Stylised Deposit Gap (Years)",
      "National Housing Affordability Score",
      "National Market-Entry Affordability Score",
      "Overview official SIH/NHHA burden snapshot",
      "Recent buyer empirical evidence",
      "data/sih_recent_buyers_2020.csv",
      "renter-entry calculator pathway",
      "40 per cent mortgage serviceability component",
      "historical-relative market-entry composite",
      "front-page market-entry interpretation layer over the existing v1 composite",
      "not the share of households who can afford housing",
      "not an absolute affordability threshold",
      "A score near 0 or 100 is low or high versus the score window",
      "Rental-entry stress may be understated relative to advertised-rent or new-lease evidence",
      "Not an official ABS/NHHA statistic or lender assessment"
    )
    missing_report_text <- required_report_text[
      !vapply(required_report_text, grepl, logical(1), report, fixed = TRUE)
    ]
    check(length(missing_report_text) == 0,
          paste("methodology_provenance_report() missing text:",
                paste(missing_report_text, collapse = "; ")))
    check(!grepl("/Users/", report, fixed = TRUE),
          "methodology_provenance_report() must not include local absolute paths")
  }

  if (exists("methodology_provenance_filename", mode = "function")) {
    filename <- methodology_provenance_filename(as.Date("2026-05-01"))
    check(identical(filename,
                    "housing_dashboard_methodology_provenance_2026-05-01.md"),
          "methodology_provenance_filename() must use the requested date pattern")
    check(grepl("\\.md$", filename),
          "methodology_provenance_filename() must return a Markdown filename")
  }
}

if (length(failures) > 0) {
  stop(
    paste(c("Provenance report checks failed:", paste0("- ", failures)),
          collapse = "\n"),
    call. = FALSE
  )
}

cat("Provenance report checks passed.\n")

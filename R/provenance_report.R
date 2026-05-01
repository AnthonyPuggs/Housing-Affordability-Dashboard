# ==============================================================================
# Methodology and data provenance report helpers
# ==============================================================================
# Generates an on-demand Markdown summary from saved CSV metadata and the
# indicator registry. These helpers intentionally avoid writing persistent files.
# ==============================================================================

dashboard_markdown_table <- function(data) {
  if (nrow(data) == 0) {
    return(character(0))
  }

  escaped <- as.data.frame(
    lapply(data, function(column) {
      values <- as.character(column)
      values[is.na(values)] <- ""
      values <- gsub("\r?\n", " ", values)
      gsub("\\|", "\\\\|", values)
    }),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  header <- paste0("| ", paste(names(escaped), collapse = " | "), " |")
  divider <- paste0("| ", paste(rep("---", ncol(escaped)), collapse = " | "), " |")
  rows <- apply(
    escaped,
    1,
    function(row) paste0("| ", paste(row, collapse = " | "), " |")
  )

  c(header, divider, rows)
}

dashboard_csv_inventory_row <- function(path) {
  file_info <- file.info(path)
  data <- tryCatch(
    readr::read_csv(path, show_col_types = FALSE, progress = FALSE),
    error = function(e) NULL
  )

  if (is.null(data)) {
    return(data.frame(
      file = file.path("data", basename(path)),
      rows = NA_integer_,
      columns = NA_integer_,
      date_min = "",
      date_max = "",
      modified_utc = format(
        file_info$mtime,
        "%Y-%m-%d %H:%M:%S UTC",
        tz = "UTC"
      ),
      stringsAsFactors = FALSE
    ))
  }

  date_min <- ""
  date_max <- ""
  if ("date" %in% names(data)) {
    parsed_dates <- suppressWarnings(as.Date(data$date))
    parsed_dates <- parsed_dates[!is.na(parsed_dates)]
    if (length(parsed_dates) > 0) {
      date_min <- format(min(parsed_dates), "%Y-%m-%d")
      date_max <- format(max(parsed_dates), "%Y-%m-%d")
    }
  }

  data.frame(
    file = file.path("data", basename(path)),
    rows = nrow(data),
    columns = ncol(data),
    date_min = date_min,
    date_max = date_max,
    modified_utc = format(
      file_info$mtime,
      "%Y-%m-%d %H:%M:%S UTC",
      tz = "UTC"
    ),
    stringsAsFactors = FALSE
  )
}

dashboard_data_inventory <- function(data_dir = project_path("data")) {
  csv_files <- sort(list.files(
    data_dir,
    pattern = "\\.csv$",
    full.names = TRUE
  ))

  if (length(csv_files) == 0) {
    return(data.frame(
      file = character(),
      rows = integer(),
      columns = integer(),
      date_min = character(),
      date_max = character(),
      modified_utc = character(),
      stringsAsFactors = FALSE
    ))
  }

  do.call(rbind, lapply(csv_files, dashboard_csv_inventory_row))
}

methodology_provenance_filename <- function(date = Sys.Date()) {
  paste0(
    "housing_dashboard_methodology_provenance_",
    format(as.Date(date), "%Y-%m-%d"),
    ".md"
  )
}

methodology_provenance_report <- function(generated_at = Sys.time(),
                                          data_dir = project_path("data")) {
  generated_at_utc <- format(
    as.POSIXct(generated_at, tz = "UTC"),
    "%Y-%m-%d %H:%M:%S UTC",
    tz = "UTC"
  )
  methodology <- indicator_registry_methodology_table()
  inventory <- dashboard_data_inventory(data_dir)

  lines <- c(
    "# Housing Affordability Dashboard Methodology And Provenance",
    "",
    paste0("Generated: ", generated_at_utc),
    "",
    "## Provenance Chain",
    "",
    "- `pipeline/05_driver.R` is the canonical local data-refresh entrypoint.",
    "- `pipeline/06_validate_outputs.R` gates required schemas, source series and minimum row counts.",
    "- `data/*.csv` stores the dashboard-ready saved outputs read by the Shiny app.",
    "- `R/indicator_registry.R` documents derived indicator formulas, source series, units, interpretation direction and caveats.",
    "- The Methodology page and this download expose that registry metadata to dashboard users.",
    "",
    "## Interpretation Caveats",
    "",
    "- Official SIH/NHHA burden and stress measures should be interpreted separately from price-index and market-entry proxy indicators.",
    "- AWE is individual earnings, not household disposable income.",
    "- WPI is a wage price index, not an income distribution measure.",
    "- CPI rents and CPI new dwelling indexes are price indexes, not household burden measures.",
    "- RBA mortgage-rate series are market-rate inputs, not lender assessment outcomes.",
    "- SIH estimates are survey estimates; relative standard error and 95% margin of error metadata are saved in `data/sih_estimate_quality.csv`, and users should interpret with caution when estimates have high RSE values.",
    "- Stylised scenario, not an official ABS measure or lender assessment.",
    "- `R/market_entry_scenarios.R` defines app-only market-entry scenarios for the calculator and assessed-rate sensitivity chart.",
    "- Assessment buffer and expense inputs are sensitivity assumptions, not a lender assessment.",
    "",
    "## Derived Indicator Methodology",
    "",
    dashboard_markdown_table(methodology),
    "",
    "## Saved Data Inventory",
    "",
    dashboard_markdown_table(inventory),
    ""
  )

  paste(lines, collapse = "\n")
}

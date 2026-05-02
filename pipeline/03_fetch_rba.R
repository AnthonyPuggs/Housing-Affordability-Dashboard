# ==============================================================================
# 03_fetch_rba.R — Fetch RBA statistical tables
# ==============================================================================
# Source:  RBA website (CSV/Excel downloads)
# Output:  data/rba_rates.csv
#
# Tables:
#   F1 — Interest Rates (cash rate)
#   F5 — Indicator Lending Rates (mortgage rates)
#   F6 — Housing Lending Rates
#
# Schema: date | value | series | series_id | category | unit | frequency
# ==============================================================================

if (!exists("project_path", mode = "function")) {
  candidates <- c(
    file.path(getwd(), "R", "project_paths.R"),
    file.path(dirname(getwd()), "R", "project_paths.R")
  )
  candidates <- candidates[file.exists(candidates)]
  if (length(candidates) == 0) {
    stop("Could not locate R/project_paths.R for RBA pipeline stage.",
         call. = FALSE)
  }
  source(candidates[[1]])
}

if (!exists("fetch_rba_table", mode = "function")) {
  source(project_path("pipeline", "00_config.R"))
}

cat("--- Fetching RBA statistical tables ---\n")

all_rba <- list()

#' Parse an RBA statistical table file (CSV or Excel)
#'
#' RBA CSV format:
#'   Row 1: "Title", series_name_1, series_name_2, ...
#'   Row 2: "Description", desc_1, desc_2, ...
#'   Row 3 (sometimes): "Type", type_1, type_2, ...
#'   Row N: "Series ID", id_1, id_2, ...
#'   Row N+1: "Units", unit_1, unit_2, ...
#'   Row N+2+: date, value_1, value_2, ... (dates as DD-Mon-YYYY)
#'
#' RBA Excel format:
#'   Similar header rows, then data with Excel date serials
parse_rba_file <- function(file, table_id) {
  is_csv <- str_detect(file, "\\.csv$")

  if (is_csv) {
    normalise_rba_csv_cache(file)
    # Raw CSVs are already cleaned by fetch_rba_table() (BOM, title row,
    # blank lines removed). Read directly; defensive cleanup if needed.
    lines <- readLines(file, warn = FALSE)
    lines[1] <- sub("^\uFEFF", "", lines[1])
    # If a non-CSV title line is still present, skip it
    if (!str_detect(lines[1], ",")) lines <- lines[-1]
    # Find first row with "Title," — that's where headers begin
    title_line <- which(str_detect(lines, "^Title,"))[1]
    if (is.na(title_line)) title_line <- 1
    csv_lines <- lines[title_line:length(lines)]
    csv_lines <- csv_lines[nchar(trimws(csv_lines)) > 0]
    tmp <- tempfile(fileext = ".csv")
    writeLines(csv_lines, tmp)
    raw <- read_csv(tmp, col_names = FALSE, show_col_types = FALSE,
                    col_types = cols(.default = "c"))
    unlink(tmp)
  } else {
    sheets <- excel_sheets(file)
    data_sheet <- sheets[str_detect(sheets, regex("data", ignore_case = TRUE))]
    if (length(data_sheet) == 0) data_sheet <- sheets[1]
    raw <- read_excel(file, sheet = data_sheet[1], col_names = FALSE,
                      col_types = "text")
  }

  if (nrow(raw) < 5 || ncol(raw) < 2) {
    warning("RBA table ", table_id, " has too few rows/columns")
    return(tibble())
  }

  # Find metadata rows by first column label
  find_row <- function(label) {
    idx <- which(str_detect(as.character(raw[[1]]),
                            regex(paste0("^", label), ignore_case = TRUE)))
    if (length(idx) > 0) idx[1] else NA
  }

  title_row     <- find_row("title")
  desc_row      <- find_row("description")
  series_id_row <- find_row("series.?id")
  unit_row      <- find_row("unit")

  # Extract series names (from Title row)
  series_names <- if (!is.na(title_row)) {
    as.character(raw[title_row, -1])
  } else {
    paste0("Series_", seq_len(ncol(raw) - 1))
  }

  series_ids <- if (!is.na(series_id_row)) {
    as.character(raw[series_id_row, -1])
  } else {
    rep(NA_character_, ncol(raw) - 1)
  }

  units <- if (!is.na(unit_row)) {
    as.character(raw[unit_row, -1])
  } else {
    rep(NA_character_, ncol(raw) - 1)
  }

  # Data starts after the last metadata row
  last_meta <- max(c(title_row, desc_row, series_id_row, unit_row, 0), na.rm = TRUE)

  # Find first data row (has a parseable date)
  data_start <- NA
  for (i in (last_meta + 1):min(last_meta + 10, nrow(raw))) {
    date_str <- as.character(raw[[1]][i])
    if (is.na(date_str)) next

    # Try multiple date formats used by RBA
    test_date <- NA
    for (fmt in c("%d-%b-%Y", "%d/%m/%Y", "%Y-%m-%d")) {
      test_date <- tryCatch(as.Date(date_str, format = fmt), error = function(e) NA)
      if (!is.na(test_date)) break
    }
    # Try Excel date serial as fallback
    if (is.na(test_date)) {
      test_date <- tryCatch(
        as.Date(as.numeric(date_str), origin = "1899-12-30"),
        error = function(e) NA
      )
    }

    if (!is.na(test_date) && test_date > as.Date("1950-01-01")) {
      data_start <- i
      break
    }
  }

  if (is.na(data_start)) {
    warning("Could not find data start in RBA table ", table_id)
    return(tibble())
  }

  data_raw <- raw[data_start:nrow(raw), ]

  # Parse dates — try multiple formats
  date_strings <- as.character(data_raw[[1]])
  dates <- rep(as.Date(NA), length(date_strings))

  for (fmt in c("%d-%b-%Y", "%d/%m/%Y", "%Y-%m-%d")) {
    na_idx <- is.na(dates)
    if (!any(na_idx)) break
    parsed <- tryCatch(
      as.Date(date_strings[na_idx], format = fmt),
      error = function(e) rep(NA, sum(na_idx))
    )
    dates[na_idx] <- parsed
  }
  # Fallback to Excel serial for remaining NAs
  na_dates <- is.na(dates)
  if (any(na_dates)) {
    excel_dates <- tryCatch(
      as.Date(as.numeric(date_strings[na_dates]), origin = "1899-12-30"),
      error = function(e) rep(NA, sum(na_dates))
    )
    dates[na_dates] <- excel_dates
  }

  results <- list()
  for (j in 2:ncol(data_raw)) {
    sname <- series_names[j - 1]
    if (is.na(sname) || sname == "" || sname == "NA") next

    sid <- series_ids[j - 1]
    u <- units[j - 1]

    values <- as_numeric_clean(as.character(data_raw[[j]]))

    valid <- !is.na(dates) & !is.na(values)
    if (sum(valid) == 0) next

    results[[length(results) + 1]] <- tibble(
      date      = dates[valid],
      value     = values[valid],
      series    = str_trim(sname),
      series_id = ifelse(is.na(sid) || sid == "NA", NA_character_, str_trim(sid)),
      category  = classify_rba_category(sname, table_id),
      unit      = ifelse(is.na(u) || u == "NA", NA_character_, str_trim(u)),
      frequency = infer_frequency_from_dates(dates[valid])
    )
  }

  bind_rows(results)
}

#' Classify RBA series into categories
classify_rba_category <- function(series_name, table_id) {
  tid <- toupper(table_id)
  case_when(
    tid == "F1" ~ "Interest Rates",
    tid == "F5" ~ "Mortgage Rates",
    tid == "F6" ~ "Housing Finance",
    str_detect(series_name, regex("cash rate", ignore_case = TRUE)) ~ "Interest Rates",
    str_detect(series_name, regex("mortgage|housing|lending", ignore_case = TRUE)) ~ "Mortgage Rates",
    TRUE ~ "RBA"
  )
}

#' Infer frequency string from date vector
infer_frequency_from_dates <- function(dates) {
  d <- sort(unique(dates))
  if (length(d) < 3) return("Unknown")
  mg <- median(as.numeric(diff(d)), na.rm = TRUE)
  if (mg <= 5) return("Day")
  if (mg <= 40) return("Month")
  if (mg <= 120) return("Quarter")
  "Year"
}

# ==============================================================================
# F1 — Cash Rate
# ==============================================================================
cat("  Fetching RBA F1 (Cash Rate)...\n")

f1_file <- tryCatch(fetch_rba_table("f1"), error = function(e) {
  stop("Required source failed for RBA F1: ", conditionMessage(e),
       call. = FALSE)
})

if (!is.null(f1_file) && file.exists(f1_file)) {
  f1_data <- parse_rba_file(f1_file, "F1")

  if (nrow(f1_data) > 0) {
    cash_rate <- f1_data %>%
      filter(str_detect(series, regex("cash rate|interbank", ignore_case = TRUE)))

    if (nrow(cash_rate) == 0) cash_rate <- f1_data

    all_rba$f1 <- cash_rate
    cat("    F1:", nrow(cash_rate), "obs,",
        length(unique(cash_rate$series)), "series\n")
  }
}

# ==============================================================================
# F5 — Indicator Lending Rates (Mortgage Rates)
# ==============================================================================
cat("  Fetching RBA F5 (Lending Rates)...\n")

f5_file <- tryCatch(fetch_rba_table("f5"), error = function(e) {
  stop("Required source failed for RBA F5: ", conditionMessage(e),
       call. = FALSE)
})

if (!is.null(f5_file) && file.exists(f5_file)) {
  f5_data <- parse_rba_file(f5_file, "F5")

  if (nrow(f5_data) > 0) {
    mortgage_rates <- f5_data %>%
      filter(str_detect(series, regex(
        "housing|mortgage|variable.*rate|fixed.*rate|owner.?occupier|investor",
        ignore_case = TRUE
      )))

    if (nrow(mortgage_rates) == 0) mortgage_rates <- f5_data

    all_rba$f5 <- mortgage_rates
    cat("    F5:", nrow(mortgage_rates), "obs,",
        length(unique(mortgage_rates$series)), "series\n")
  }
}

# ==============================================================================
# F6 — Housing Lending Rates
# ==============================================================================
cat("  Fetching RBA F6 (Housing Lending)...\n")

f6_file <- tryCatch(fetch_rba_table("f6"), error = function(e) {
  stop("Required source failed for RBA F6: ", conditionMessage(e),
       call. = FALSE)
})

if (!is.null(f6_file) && file.exists(f6_file)) {
  f6_data <- parse_rba_file(f6_file, "F6")

  if (nrow(f6_data) > 0) {
    housing_finance <- f6_data %>%
      filter(str_detect(series, regex(
        "owner.?occupier|housing|loan|commitment|dwelling|variable|fixed|rate",
        ignore_case = TRUE
      )))

    if (nrow(housing_finance) == 0) housing_finance <- f6_data

    all_rba$f6 <- housing_finance
    cat("    F6:", nrow(housing_finance), "obs,",
        length(unique(housing_finance$series)), "series\n")
  }
}

# ==============================================================================
# Combine and write
# ==============================================================================
rba_rates <- bind_rows(all_rba) %>%
  distinct(date, series, .keep_all = TRUE) %>%
  arrange(category, series, date)

if (nrow(rba_rates) > 0) {
  write_pipeline_csv(rba_rates, "rba_rates.csv")
  cat("--- RBA fetch complete ---\n")
  cat("  Total series:", length(unique(rba_rates$series)), "\n")
  cat("  Date range:", as.character(min(rba_rates$date, na.rm = TRUE)),
      "to", as.character(max(rba_rates$date, na.rm = TRUE)), "\n")
} else {
  cat("--- RBA fetch complete (no data retrieved — check network) ---\n")
}

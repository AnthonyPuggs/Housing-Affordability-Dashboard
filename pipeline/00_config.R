# ==============================================================================
# 00_config.R — Shared configuration, paths, packages, and helper functions
# ==============================================================================

# --- Packages -----------------------------------------------------------------
library(readxl)
library(readabs)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(readr)
library(lubridate)
library(httr)
library(here)

# --- Paths --------------------------------------------------------------------
# PROJECT_ROOT is the project directory containing resources/ and pipeline/
# The driver script (05_driver.R) sets working directory to project root before
# sourcing this file. If run standalone, try getwd() then parent.
# Ensure PROJECT_ROOT is set (use here() when available, otherwise fallback to cwd)
if (!exists("PROJECT_ROOT")) {
  if (requireNamespace("here", quietly = TRUE)) {
    PROJECT_ROOT <- here::here()
  } else {
    PROJECT_ROOT <- normalizePath(getwd(), winslash = "/")
  }
}

SIH_DIR      <- file.path(PROJECT_ROOT, "resources", "ABS_data",
                           "housing_occupancy_and_costs_SIH")
RESOURCES_DIR <- file.path(PROJECT_ROOT, "resources")
DATA_DIR     <- file.path(PROJECT_ROOT, "data")

# R
# Replace the vectorised ifelse(...) used for side-effects with a normal if/else
if (Sys.getenv("R_READABS_PATH") == file.path(PROJECT_ROOT, "data")) {
  cat("R_READABS_PATH is set correctly.\n")
} else {
  Sys.setenv(R_READABS_PATH = tempdir())
  cat("R_READABS_PATH is not set correctly. Using tempdir() for this session.\n")
}

cat("Project root:", PROJECT_ROOT, "\n")
cat("SIH data dir:", SIH_DIR, "\n")
cat("Output dir:  ", DATA_DIR, "\n")

# --- Utility ------------------------------------------------------------------

ensure_dir <- function(path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE)
  invisible(path)
}

ensure_dir(DATA_DIR)

# --- Helpers ported from app_old.R --------------------------------------------

safe_read <- function(expr, label, warn = TRUE) {
  tryCatch(
    expr,
    error = function(e) {
      if (isTRUE(warn)) {
        warning(paste0("Failed to load ", label, ": ", conditionMessage(e)))
      }
      tibble()
    }
  )
}

normalize_abs <- function(df, label = NULL, category = "Other",
                          units = NA_character_, freq_hint = NA_character_) {
  if (nrow(df) == 0) return(tibble())
  if (!"date" %in% names(df)) return(tibble())
  if (!"value" %in% names(df)) return(tibble())

  out <- df %>%
    mutate(
      date      = as.Date(date),
      value     = as.numeric(value),
      series    = if ("series" %in% names(df)) as.character(series)
                  else ifelse(is.null(label), "Series", label),
      series_id = if ("series_id" %in% names(df)) as.character(series_id)
                  else NA_character_,
      unit      = if ("unit" %in% names(df)) as.character(unit) else units,
      frequency = if ("frequency" %in% names(df)) as.character(frequency)
                  else freq_hint
    ) %>%
    filter(!is.na(date), !is.na(value))

  if (!is.null(label)) out$series <- label

  out %>%
    mutate(category = category) %>%
    select(date, value, series, series_id, category, unit, frequency)
}

select_series <- function(df, pattern, label, category, units = NA_character_) {
  if (nrow(df) == 0 || !"series" %in% names(df)) return(tibble())

  matched <- df %>%
    filter(str_detect(series, regex(pattern, ignore_case = TRUE)))

  if (nrow(matched) == 0) return(tibble())

  normalize_abs(matched, label = label, category = category, units = units)
}

infer_lag <- function(freq_label) {
  case_when(
    str_detect(freq_label %||% "", regex("quarter", ignore_case = TRUE)) ~ 4,
    str_detect(freq_label %||% "", regex("month",   ignore_case = TRUE)) ~ 12,
    str_detect(freq_label %||% "", regex("year",    ignore_case = TRUE)) ~ 1,
    TRUE ~ 12
  )
}

infer_lag_from_dates <- function(dates, fallback = 12) {
  d <- sort(unique(as.Date(dates)))
  if (length(d) < 3) return(fallback)

  median_gap <- median(as.numeric(diff(d)), na.rm = TRUE)
  if (is.na(median_gap)) return(fallback)

  if (median_gap <= 40)  return(12)   # monthly
  if (median_gap <= 120) return(4)    # quarterly
  1                                    # annual
}

# --- New helpers for SIH parsing ----------------------------------------------

#' Read an ABS SIH table from Excel, handling multi-row headers and footnotes
#'
#' @param file Path to Excel workbook
#' @param sheet Sheet name (e.g. "Table 1.1")
#' @param skip Number of header rows to skip before data
#' @param col_names Character vector of column names to assign
#' @param max_rows Maximum data rows to read (NULL = all)
#' @return A tibble with assigned column names and footnote rows removed
read_sih_table <- function(file, sheet, skip, col_names = NULL, max_rows = NULL) {
  raw <- read_excel(
    file, sheet = sheet, skip = skip,
    col_names = FALSE, col_types = "text",
    n_max = if (!is.null(max_rows)) max_rows else NA
  )

  # Remove completely empty rows

raw <- raw[rowSums(!is.na(raw) & raw != "") > 0, , drop = FALSE]

  # Assign column names if provided
  if (!is.null(col_names)) {
    # Trim to actual columns present
    n <- min(length(col_names), ncol(raw))
    names(raw)[seq_len(n)] <- col_names[seq_len(n)]
    if (ncol(raw) > n) {
      raw <- raw[, seq_len(n)]
    }
  }

  raw
}

#' Clean ABS suppressed values: convert "..", "np", "na", "n.a.", "–" to NA
clean_abs_values <- function(x) {
  x <- str_trim(x)
  x[x %in% c("..", "np", "na", "n.a.", "n.a", "n.p.", "\u2013", "\u2014",
              "-", "*", "**", "***", "—")] <- NA_character_
  x
}

#' Convert cleaned text column to numeric, suppressing warnings for non-numeric
as_numeric_clean <- function(x) {
  suppressWarnings(as.numeric(clean_abs_values(x)))
}

#' Detect if a row is a footnote/annotation (non-data) row
#' Checks if at least one expected numeric column actually has a numeric value
is_data_row <- function(row_values) {
  any(!is.na(suppressWarnings(as.numeric(clean_abs_values(row_values)))))
}

#' Download an RBA statistical table (CSV format)
#'
#' @param table_id RBA table identifier (e.g. "f5", "f6", "f1")
#' @param cache_dir Directory to cache downloaded files
#' @return Path to the cached CSV file, or NULL on failure
fetch_rba_table <- function(table_id, cache_dir = DATA_DIR) {
  table_id_lower <- tolower(table_id)

  # RBA CSV URL pattern: f1-data.csv, f5-data.csv, etc.
  url <- paste0("https://www.rba.gov.au/statistics/tables/csv/",
                table_id_lower, "-data.csv")

  cache_file <- file.path(cache_dir, paste0("rba_", table_id_lower, "_raw.csv"))

  if (!file.exists(cache_file) ||
      difftime(Sys.time(), file.mtime(cache_file), units = "hours") > 24) {
    cat("  Downloading RBA table", toupper(table_id), "from CSV endpoint...\n")
    resp <- tryCatch(
      GET(url, write_disk(cache_file, overwrite = TRUE)),
      error = function(e) NULL
    )
    if (!is.null(resp) && !http_error(resp) && str_detect(cache_file, "\\.csv$")) {
      # Clean the downloaded CSV: remove BOM, non-CSV title row, blank/trailing lines
      lines <- readLines(cache_file, warn = FALSE)
      lines[1] <- sub("^\uFEFF", "", lines[1])
      # Remove non-CSV title row (first line with no commas)
      if (!str_detect(lines[1], ",")) lines <- lines[-1]
      # Remove blank lines
      lines <- lines[nchar(trimws(lines)) > 0]
      writeLines(lines, cache_file)
    }
    if (is.null(resp) || http_error(resp)) {
      # Try Excel as fallback with various naming patterns
      xlsx_urls <- c(
        paste0("https://www.rba.gov.au/statistics/tables/xls/",
               table_id_lower, "hist.xlsx"),
        paste0("https://www.rba.gov.au/statistics/tables/xls/",
               str_replace(table_id_lower, "f(\\d)", "f0\\1"), "hist.xlsx"),
        paste0("https://www.rba.gov.au/statistics/tables/xls/",
               table_id_lower, "d.xlsx"),
        paste0("https://www.rba.gov.au/statistics/tables/xls/",
               str_replace(table_id_lower, "f(\\d)", "f0\\1"), "d.xlsx")
      )
      cache_xlsx <- file.path(cache_dir, paste0("rba_", table_id_lower, "_raw.xlsx"))
      success <- FALSE
      for (u in xlsx_urls) {
        resp <- tryCatch(
          GET(u, write_disk(cache_xlsx, overwrite = TRUE)),
          error = function(e) NULL
        )
        if (!is.null(resp) && !http_error(resp)) {
          success <- TRUE
          cache_file <- cache_xlsx
          break
        }
      }
      if (!success) {
        warning("Failed to download RBA table ", toupper(table_id))
        return(NULL)
      }
    }
  } else {
    cat("  Using cached RBA table", toupper(table_id), "\n")
  }

  cache_file
}

# --- CSV output helper --------------------------------------------------------

write_pipeline_csv <- function(df, filename) {
  path <- file.path(DATA_DIR, filename)
  tryCatch({
    write_csv(df, path)
    cat("  Wrote", nrow(df), "rows to", filename, "\n")
  }, error = function(e) {
    if (str_detect(conditionMessage(e), "open|permission|access|lock")) {
      warning("Cannot write ", filename, " — file may be open in another program. ",
              "Close it and re-run the pipeline.")
    } else {
      stop(e)
    }
  })
  invisible(path)
}

cat("Config loaded successfully.\n")

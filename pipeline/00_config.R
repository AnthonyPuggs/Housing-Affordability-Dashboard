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

.load_pipeline_config_project_paths <- function(envir = parent.frame()) {
  source_file <- NULL
  frames <- sys.frames()
  for (i in rev(seq_along(frames))) {
    frame <- frames[[i]]
    if (exists("ofile", envir = frame, inherits = FALSE)) {
      source_file <- get("ofile", envir = frame, inherits = FALSE)
      break
    }
  }

  starts <- unique(c(
    if (!is.null(source_file)) dirname(normalizePath(source_file, winslash = "/", mustWork = TRUE)),
    getwd()
  ))
  candidates <- unique(c(
    file.path(starts, "R", "project_paths.R"),
    file.path(dirname(starts), "R", "project_paths.R")
  ))
  candidates <- candidates[file.exists(candidates)]
  if (length(candidates) == 0) {
    stop("Could not locate R/project_paths.R for pipeline configuration.", call. = FALSE)
  }
  source(candidates[[1]], local = envir)
}

if (!exists("project_path", mode = "function")) {
  .load_pipeline_config_project_paths()
}
rm(.load_pipeline_config_project_paths)

# --- Paths --------------------------------------------------------------------
PROJECT_ROOT <- project_root()
SIH_DIR <- project_path("resources", "ABS_data",
                        "housing_occupancy_and_costs_SIH")
RESOURCES_DIR <- project_path("resources")
DATA_DIR <- project_path("data")

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

safe_read <- function(expr, label, warn = TRUE, required = FALSE) {
  tryCatch(
    expr,
    error = function(e) {
      if (isTRUE(required)) {
        stop("Required source failed for ", label, ": ", conditionMessage(e),
             call. = FALSE)
      }
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
rba_csv_parse_problem_count <- function(path) {
  if (!file.exists(path)) {
    stop("RBA CSV cache does not exist: ", path, call. = FALSE)
  }

  parsed <- suppressWarnings(
    read_csv(
      path,
      col_names = FALSE,
      col_types = cols(.default = "c"),
      show_col_types = FALSE,
      progress = FALSE
    )
  )
  nrow(problems(parsed))
}

normalise_rba_csv_cache <- function(path) {
  if (!file.exists(path)) {
    stop("RBA CSV cache does not exist: ", path, call. = FALSE)
  }

  raw <- utils::read.csv(
    path,
    header = FALSE,
    fill = TRUE,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    colClasses = "character",
    na.strings = character(0),
    blank.lines.skip = TRUE
  )

  if (nrow(raw) == 0 || ncol(raw) == 0) {
    return(invisible(path))
  }

  raw[is.na(raw)] <- ""
  non_empty_cols <- which(colSums(raw != "") > 0)
  if (length(non_empty_cols) > 0) {
    raw <- raw[, seq_len(max(non_empty_cols)), drop = FALSE]
  }

  utils::write.table(
    raw,
    file = path,
    sep = ",",
    row.names = FALSE,
    col.names = FALSE,
    quote = TRUE,
    na = "",
    qmethod = "double"
  )
  invisible(path)
}

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
      normalise_rba_csv_cache(cache_file)
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

  if (str_detect(cache_file, "\\.csv$") &&
      rba_csv_parse_problem_count(cache_file) > 0) {
    normalise_rba_csv_cache(cache_file)
  }

  cache_file
}

# --- Strict mode ---------------------------------------------------------------
# Under the driver and in CI, problems that used to be downgraded to warnings
# (parser failures, write locks) become hard errors so a failed stage cannot
# ship stale or partial outputs behind a green gate. Standalone interactive
# stage runs keep warnings unless CI is set.
if (!exists("PIPELINE_STRICT")) {
  PIPELINE_STRICT <- nzchar(Sys.getenv("CI"))
}

pipeline_problem <- function(...) {
  message_text <- paste0(...)
  if (isTRUE(PIPELINE_STRICT)) {
    stop(message_text, call. = FALSE)
  }
  warning(message_text, call. = FALSE)
}

# --- Fail-loud series selection helpers ----------------------------------------

# Keep one seasonal-adjustment variant per series name, by fixed preference,
# so the published variant is a deliberate choice rather than bind_rows order.
prefer_series_type <- function(df, prefer = c("Seasonally Adjusted", "Trend",
                                              "Original")) {
  if (is.null(df) || nrow(df) == 0 || !"series_type" %in% names(df)) {
    return(df)
  }
  df %>%
    group_by(series) %>%
    filter({
      present <- intersect(prefer, unique(series_type))
      if (length(present) == 0) rep(TRUE, dplyr::n())
      else series_type == present[[1]]
    }) %>%
    ungroup()
}

# Loud guard for regex/name-based selections: a renamed ABS series must fail
# the pipeline, not write an empty or partial file that passes the stage gate.
assert_selection_nonempty <- function(df, what) {
  if (is.null(df) || nrow(df) == 0) {
    stop("Series selection for '", what,
         "' matched nothing - the source table layout or series names may have changed.",
         call. = FALSE)
  }
  invisible(df)
}

# Deterministic combine: collapse exact repeats on (date, series, series_id) -
# the same source series may be deliberately republished under different names
# (e.g. the 6432.0 mean price ships as both the national mean price and a
# state index) - then
# fail loudly if distinct source series still share a (date, series) cell.
# Previously distinct(date, series) silently kept whichever variant bind_rows
# ordered first.
combine_series_unique <- function(series_list, dataset) {
  combined <- bind_rows(series_list)
  if (nrow(combined) == 0) {
    return(combined)
  }
  combined <- combined %>% distinct(date, series, series_id, .keep_all = TRUE)
  dup <- combined %>%
    count(date, series, name = "n") %>%
    filter(n > 1)
  if (nrow(dup) > 0) {
    stop(
      dataset, " has ", nrow(dup),
      " duplicate (date, series) observations from different source series",
      " (first: '", dup$series[[1]], "' at ", dup$date[[1]],
      "). Select one variant explicitly instead of relying on row order.",
      call. = FALSE
    )
  }
  combined %>% arrange(category, series, date)
}

# --- ABS SDMX API (pinned dataflows) -------------------------------------------
# Direct SDMX endpoints are pinned to explicit dataflow versions (review
# PIPE-09): an ABS dataflow upgrade changes our results only when the version
# is deliberately bumped here, never silently. abs_sdmx_csv() additionally
# asserts the response echoes the pinned dataflow and carries the expected
# dimension columns, so an ABS recode fails loudly at fetch time instead of
# shipping silently different numbers.
ABS_SDMX_DATA_URL <- "https://data.api.abs.gov.au/rest/data"
ABS_SDMX_CPI_FLOW <- "ABS,CPI,2.0.0"
ABS_SDMX_LF_FLOW <- "ABS,LF,1.0.0"
ABS_SDMX_LF_UNDER_FLOW <- "ABS,LF_UNDER,1.0.1"
# Keys are MEASURE.INDEX.TSEST.REGION.FREQ for the CPI dataflow.
ABS_SDMX_CPI_RENTS_KEY <- "1.115522.10.50.Q"
ABS_SDMX_CPI_ALL_GROUPS_KEY <- "1.10001.10.50.Q"

abs_sdmx_csv <- function(flow, key, what,
                         required_columns = c("TIME_PERIOD", "OBS_VALUE")) {
  url <- paste0(ABS_SDMX_DATA_URL, "/", flow, "/", key)
  resp <- httr::GET(url, httr::add_headers(Accept = "text/csv"))
  if (httr::status_code(resp) != 200) {
    stop("ABS SDMX API returned ", httr::status_code(resp), " for ", what,
         " (", url, ")", call. = FALSE)
  }
  d <- readr::read_csv(I(httr::content(resp, as = "text", encoding = "UTF-8")),
                       show_col_types = FALSE)
  if (nrow(d) == 0) {
    stop("ABS SDMX response for ", what, " contains no observations.",
         call. = FALSE)
  }
  missing_columns <- setdiff(required_columns, names(d))
  if (length(missing_columns) > 0) {
    stop("ABS SDMX response for ", what, " is missing expected columns: ",
         paste(missing_columns, collapse = ", "),
         " - the dataflow structure may have changed.", call. = FALSE)
  }
  # The CSV echoes the dataflow as e.g. "ABS:CPI(2.0.0)".
  expected_dataflow <- sub("^([^,]+),([^,]+),(.+)$", "\\1:\\2(\\3)", flow)
  if ("DATAFLOW" %in% names(d) &&
      !all(d$DATAFLOW == expected_dataflow, na.rm = TRUE)) {
    stop("ABS SDMX response for ", what, " reports dataflow '",
         d$DATAFLOW[[1]], "' instead of pinned '", expected_dataflow, "'.",
         call. = FALSE)
  }
  d
}

# Fixed base quarter for the 6432.0 mean dwelling price state indexes (review
# PIPE-11): indexing to first(value) re-bases silently whenever ABS revises or
# back-extends history. 2011-09 is the first published quarter of the 6432.0
# mean price series, so pinning it keeps existing index values unchanged.
DWELLING_PRICE_INDEX_BASE_QUARTER <- as.Date("2011-09-01")

# --- CSV output helper --------------------------------------------------------

write_pipeline_csv <- function(df, filename) {
  path <- file.path(DATA_DIR, filename)
  tryCatch({
    write_csv(df, path)
    cat("  Wrote", nrow(df), "rows to", filename, "\n")
  }, error = function(e) {
    if (str_detect(conditionMessage(e), "open|permission|access|lock")) {
      pipeline_problem(
        "Cannot write ", filename,
        " - file may be open in another program. Close it and re-run the pipeline."
      )
    } else {
      stop(e)
    }
  })
  invisible(path)
}

cat("Config loaded successfully.\n")

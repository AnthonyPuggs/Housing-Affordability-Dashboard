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

# --- RBA acquisition and parsing ----------------------------------------------

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

rba_required_series_contract <- function(table_id) {
  contracts <- list(
    F1 = list(series = "Cash Rate Target", series_id = "FIRMMCRTD"),
    F5 = list(
      series = paste0(
        "Lending rates; Housing loans; Banks; Variable; Discounted; ",
        "Owner-occupier"
      ),
      series_id = "FILRHLBVD"
    ),
    F6 = list(
      series = paste0(
        "Lending rates; Housing credit; New loans funded in the month; ",
        "Owner-occupied; All loans; All institutions"
      ),
      series_id = "FLRHOFTA"
    ),
    E2 = list(series = "Household debt to income", series_id = "BHFDDIT")
  )
  contract <- contracts[[toupper(table_id)]]
  if (is.null(contract)) {
    stop("Unsupported RBA table contract: ", table_id, call. = FALSE)
  }
  contract
}

rba_source_problem <- function(table_id, operation, detail,
                               strict = PIPELINE_STRICT) {
  message_text <- paste0("RBA ", toupper(table_id), " ", operation,
                         " failed: ", detail)
  if (isTRUE(strict)) {
    stop(message_text, call. = FALSE)
  }
  warning(message_text, call. = FALSE)
  invisible(NULL)
}

classify_rba_category <- function(series_name, table_id) {
  tid <- toupper(table_id)
  case_when(
    tid == "F1" ~ "Interest Rates",
    tid == "F5" ~ "Mortgage Rates",
    tid == "F6" ~ "Housing Finance",
    tid == "E2" ~ "Household Finances",
    str_detect(series_name, regex("cash rate", ignore_case = TRUE)) ~
      "Interest Rates",
    str_detect(series_name,
               regex("mortgage|housing|lending", ignore_case = TRUE)) ~
      "Mortgage Rates",
    TRUE ~ "RBA"
  )
}

infer_frequency_from_dates <- function(dates) {
  d <- sort(unique(dates))
  if (length(d) < 3) return("Unknown")
  mg <- median(as.numeric(diff(d)), na.rm = TRUE)
  if (mg <= 5) return("Day")
  if (mg <= 40) return("Month")
  if (mg <= 120) return("Quarter")
  "Year"
}

parse_rba_file <- function(file, table_id, strict = PIPELINE_STRICT) {
  fail <- function(detail) {
    rba_source_problem(table_id, "parsing", detail, strict = strict)
    tibble()
  }

  if (is.null(file) || length(file) != 1 || is.na(file) || !nzchar(file)) {
    return(fail("no source file was supplied"))
  }
  if (!file.exists(file)) {
    return(fail("source file does not exist"))
  }
  if (is.na(file.info(file)$size) || file.info(file)$size == 0) {
    return(fail("table is empty"))
  }

  raw <- tryCatch({
    if (str_detect(file, regex("\\.csv$", ignore_case = TRUE))) {
      lines <- readLines(file, warn = FALSE)
      if (length(lines) == 0) stop("table is empty")
      lines[1] <- sub("^\uFEFF", "", lines[1])
      title_line <- which(str_detect(lines, '^"?Title"?,'))[1]
      if (is.na(title_line)) stop("Title metadata row is missing")
      lines <- lines[title_line:length(lines)]
      lines <- lines[nchar(trimws(lines)) > 0]
      tmp <- tempfile(fileext = ".csv")
      on.exit(unlink(tmp), add = TRUE)
      writeLines(lines, tmp)
      read_csv(tmp, col_names = FALSE, show_col_types = FALSE,
               col_types = cols(.default = "c"), progress = FALSE)
    } else if (str_detect(file, regex("\\.xlsx?$", ignore_case = TRUE))) {
      sheets <- excel_sheets(file)
      if (length(sheets) == 0) stop("workbook has no sheets")
      data_sheet <- sheets[str_detect(sheets,
                                      regex("data", ignore_case = TRUE))]
      if (length(data_sheet) == 0) data_sheet <- sheets[1]
      read_excel(file, sheet = data_sheet[1], col_names = FALSE,
                 col_types = "text")
    } else {
      stop("unsupported file type")
    }
  }, error = identity)
  if (inherits(raw, "error")) {
    return(fail(conditionMessage(raw)))
  }
  if (is.null(raw) || nrow(raw) == 0 || ncol(raw) == 0) {
    return(fail("table is empty"))
  }
  if (nrow(raw) < 5 || ncol(raw) < 2) {
    return(fail("table has too few rows or columns"))
  }

  find_row <- function(label) {
    idx <- which(str_detect(as.character(raw[[1]]),
                            regex(paste0("^", label), ignore_case = TRUE)))
    if (length(idx) > 0) idx[1] else NA_integer_
  }
  title_row <- find_row("title")
  desc_row <- find_row("description")
  series_id_row <- find_row("series.?id")
  unit_row <- find_row("unit")
  if (is.na(title_row) || is.na(series_id_row) || is.na(unit_row)) {
    return(fail("required Title, Series ID or Units metadata is missing"))
  }

  series_names <- as.character(raw[title_row, -1])
  series_ids <- as.character(raw[series_id_row, -1])
  units <- as.character(raw[unit_row, -1])
  last_meta <- max(c(title_row, desc_row, series_id_row, unit_row), na.rm = TRUE)

  data_start <- NA_integer_
  search_rows <- seq.int(last_meta + 1L,
                         min(last_meta + 10L, nrow(raw)))
  for (i in search_rows) {
    date_str <- as.character(raw[[1]][i])
    if (is.na(date_str)) next
    test_date <- as.Date(NA)
    for (fmt in c("%d-%b-%Y", "%d/%m/%Y", "%Y-%m-%d")) {
      test_date <- suppressWarnings(as.Date(date_str, format = fmt))
      if (!is.na(test_date)) break
    }
    if (is.na(test_date)) {
      test_date <- suppressWarnings(
        as.Date(as.numeric(date_str), origin = "1899-12-30")
      )
    }
    if (!is.na(test_date) && test_date > as.Date("1950-01-01")) {
      data_start <- i
      break
    }
  }
  if (is.na(data_start)) {
    return(fail("no parseable observation dates were found"))
  }

  data_raw <- raw[data_start:nrow(raw), , drop = FALSE]
  date_strings <- as.character(data_raw[[1]])
  dates <- rep(as.Date(NA), length(date_strings))
  for (fmt in c("%d-%b-%Y", "%d/%m/%Y", "%Y-%m-%d")) {
    na_idx <- is.na(dates)
    if (!any(na_idx)) break
    dates[na_idx] <- suppressWarnings(
      as.Date(date_strings[na_idx], format = fmt)
    )
  }
  na_dates <- is.na(dates)
  if (any(na_dates)) {
    dates[na_dates] <- suppressWarnings(
      as.Date(as.numeric(date_strings[na_dates]), origin = "1899-12-30")
    )
  }

  results <- list()
  for (j in seq.int(2L, ncol(data_raw))) {
    sname <- series_names[j - 1L]
    if (is.na(sname) || !nzchar(sname) || sname == "NA") next
    sid <- series_ids[j - 1L]
    unit <- units[j - 1L]
    values <- as_numeric_clean(as.character(data_raw[[j]]))
    valid <- !is.na(dates) & !is.na(values)
    if (!any(valid)) next
    results[[length(results) + 1L]] <- tibble(
      date = dates[valid],
      value = values[valid],
      series = str_trim(sname),
      series_id = ifelse(is.na(sid) || sid == "NA", NA_character_,
                         str_trim(sid)),
      category = classify_rba_category(sname, table_id),
      unit = ifelse(is.na(unit) || unit == "NA", NA_character_,
                    str_trim(unit)),
      frequency = infer_frequency_from_dates(dates[valid])
    )
  }
  parsed <- bind_rows(results)
  if (nrow(parsed) == 0) {
    return(fail("no parseable observations were found"))
  }

  contract <- rba_required_series_contract(table_id)
  required <- parsed %>%
    filter(series == contract$series, series_id == contract$series_id)
  if (nrow(required) == 0) {
    return(fail(paste0(
      "required series '", contract$series, "' (", contract$series_id,
      ") was not found"
    )))
  }
  if (any(!is.finite(required$value))) {
    return(fail(paste0(
      "required series '", contract$series, "' (", contract$series_id,
      ") contains non-finite observations"
    )))
  }
  if (any(!is.finite(parsed$value))) {
    return(fail("parsed output contains non-finite observations"))
  }
  parsed
}

prepare_rba_csv_candidate <- function(path) {
  lines <- readLines(path, warn = FALSE)
  if (length(lines) == 0) stop("RBA CSV candidate is empty.", call. = FALSE)
  lines[1] <- sub("^\uFEFF", "", lines[1])
  title_line <- which(str_detect(lines, '^"?Title"?,'))[1]
  if (is.na(title_line)) {
    stop("RBA CSV candidate has no Title metadata row.", call. = FALSE)
  }
  lines <- lines[title_line:length(lines)]
  lines <- lines[nchar(trimws(lines)) > 0]
  writeLines(lines, path)
  normalise_rba_csv_cache(path)
  invisible(path)
}

validate_rba_candidate <- function(path, table_id) {
  parsed <- parse_rba_file(path, table_id, strict = TRUE)
  if (nrow(parsed) == 0) {
    stop("RBA candidate contains no observations.", call. = FALSE)
  }
  invisible(TRUE)
}

promote_rba_candidate <- function(candidate, destination,
                                  rename_fun = file.rename) {
  if (!file.exists(candidate)) {
    stop("RBA cache promotion failed: candidate does not exist.",
         call. = FALSE)
  }
  if (!file.exists(destination)) {
    if (!isTRUE(rename_fun(candidate, destination))) {
      stop("RBA cache promotion failed: could not install candidate.",
           call. = FALSE)
    }
    return(invisible(destination))
  }

  backup <- tempfile(paste0(basename(destination), "-backup-"),
                     tmpdir = dirname(destination),
                     fileext = paste0(".", tools::file_ext(destination)))
  if (!isTRUE(rename_fun(destination, backup))) {
    stop("RBA cache promotion failed: could not preserve known-good cache.",
         call. = FALSE)
  }
  installed <- isTRUE(rename_fun(candidate, destination))
  if (!installed) {
    restored <- isTRUE(rename_fun(backup, destination))
    if (!restored && file.exists(backup)) {
      restored <- isTRUE(file.copy(backup, destination, overwrite = FALSE,
                                   copy.date = TRUE))
    }
    if (!restored) {
      stop("RBA cache promotion failed and known-good cache restoration failed; ",
           "backup remains at ", backup, call. = FALSE)
    }
    stop("RBA cache promotion failed; known-good cache was restored.",
         call. = FALSE)
  }
  unlink(backup)
  invisible(destination)
}

rba_fetch_dependencies <- function() {
  list(
    get = httr::GET,
    write_disk = httr::write_disk,
    http_error = httr::http_error,
    validator = validate_rba_candidate,
    promoter = promote_rba_candidate
  )
}

fetch_rba_table <- function(table_id, cache_dir = DATA_DIR,
                            dependencies = rba_fetch_dependencies(),
                            strict = PIPELINE_STRICT) {
  table_upper <- toupper(table_id)
  table_lower <- tolower(table_id)
  rba_required_series_contract(table_upper)
  ensure_dir(cache_dir)

  required_dependencies <- c("get", "write_disk", "http_error", "validator",
                             "promoter")
  if (!all(required_dependencies %in% names(dependencies))) {
    stop("RBA fetch dependencies are incomplete.", call. = FALSE)
  }

  cache_files <- c(
    csv = file.path(cache_dir, paste0("rba_", table_lower, "_raw.csv")),
    xlsx = file.path(cache_dir, paste0("rba_", table_lower, "_raw.xlsx"))
  )
  valid_cache <- vapply(cache_files, function(path) {
    if (!file.exists(path)) return(FALSE)
    isTRUE(tryCatch({
      dependencies$validator(path, table_upper)
      TRUE
    }, error = function(e) FALSE, warning = function(w) FALSE))
  }, logical(1))
  cache_age_hours <- rep(Inf, length(cache_files))
  present <- file.exists(cache_files)
  cache_age_hours[present] <- as.numeric(
    difftime(Sys.time(), file.info(cache_files[present])$mtime,
             units = "hours")
  )
  fresh_valid <- valid_cache & cache_age_hours <= 24
  if (any(fresh_valid)) {
    selected <- cache_files[which(fresh_valid)[1]]
    cat("  Using cached RBA table", table_upper, "\n")
    return(unname(selected))
  }
  stale_valid <- cache_files[valid_cache]

  csv_url <- paste0("https://www.rba.gov.au/statistics/tables/csv/",
                    table_lower, "-data.csv")
  xlsx_stems <- unique(c(
    paste0(table_lower, "hist"),
    paste0(str_replace(table_lower, "f(\\d)", "f0\\1"), "hist"),
    paste0(table_lower, "d"),
    paste0(str_replace(table_lower, "f(\\d)", "f0\\1"), "d")
  ))
  attempts <- c(csv_url,
                paste0("https://www.rba.gov.au/statistics/tables/xls/",
                       xlsx_stems, ".xlsx"))
  attempt_errors <- character()

  for (url in attempts) {
    extension <- if (grepl("/csv/", url, fixed = TRUE)) ".csv" else ".xlsx"
    candidate <- tempfile(paste0("rba_", table_lower, "_"),
                          tmpdir = cache_dir, fileext = extension)
    response <- tryCatch(
      dependencies$get(
        url,
        dependencies$write_disk(candidate, overwrite = TRUE)
      ),
      error = identity
    )
    if (inherits(response, "error")) {
      attempt_errors <- c(attempt_errors, conditionMessage(response))
      unlink(candidate)
      next
    }
    if (isTRUE(dependencies$http_error(response))) {
      status <- tryCatch(httr::status_code(response), error = function(e) {
        if (!is.null(response$status)) response$status else NA_integer_
      })
      attempt_errors <- c(
        attempt_errors,
        paste0("HTTP request failed",
               if (!is.na(status)) paste0(" (status ", status, ")") else "")
      )
      unlink(candidate)
      next
    }

    validated <- tryCatch({
      if (identical(extension, ".csv")) prepare_rba_csv_candidate(candidate)
      dependencies$validator(candidate, table_upper)
      TRUE
    }, error = function(e) {
      attempt_errors <<- c(attempt_errors, conditionMessage(e))
      FALSE
    })
    if (!validated) {
      unlink(candidate)
      next
    }

    destination <- cache_files[[if (identical(extension, ".csv")) "csv"
                                else "xlsx"]]
    promoted <- tryCatch({
      dependencies$promoter(candidate, destination)
      TRUE
    }, error = function(e) {
      attempt_errors <<- c(attempt_errors, conditionMessage(e))
      FALSE
    })
    unlink(candidate)
    if (promoted) return(unname(destination))
  }

  details <- paste(unique(attempt_errors), collapse = "; ")
  if (length(stale_valid) > 0) {
    selected <- unname(stale_valid[[1]])
    if (isTRUE(strict)) {
      rba_source_problem(
        table_upper,
        "acquisition",
        paste0(details, "; validated stale cache remains at ", selected),
        strict = TRUE
      )
    }
    warning("RBA ", table_upper, " refresh failed (", details,
            "); using validated stale cache.", call. = FALSE)
    return(selected)
  }
  rba_source_problem(table_upper, "acquisition",
                     if (nzchar(details)) details else "all candidates failed",
                     strict = strict)
  NULL
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

# Data vintage helpers for pipeline metadata and dashboard display.

if (!exists("project_root", mode = "function") ||
    !exists("project_path", mode = "function")) {
  source(file.path("R", "project_paths.R"))
}

data_vintage_filename <- function() {
  "data_vintage.csv"
}

data_vintage_format_utc <- function(x) {
  if (length(x) == 0 || is.na(x)) {
    return("")
  }
  format(as.POSIXct(x, tz = "UTC"), "%Y-%m-%d %H:%M:%S UTC", tz = "UTC")
}

data_vintage_parse_utc <- function(x) {
  x <- as.character(x)
  x <- sub(" UTC$", "", x)
  as.POSIXct(x, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
}

data_vintage_source_group <- function(filename) {
  if (filename %in% c("abs_timeseries.csv", "abs_supply_demand.csv")) {
    return("ABS live time series")
  }
  if (filename == "rba_rates.csv" || grepl("^rba_.*_raw[.]csv$", filename)) {
    return("RBA live tables")
  }
  if (filename == "affordability_indices.csv") {
    return("Derived dashboard indicators")
  }
  if (grepl("^sih_", filename)) {
    return("Static ABS SIH workbook outputs")
  }
  "Dashboard data"
}

data_vintage_survey_order <- function(x) {
  suppressWarnings(as.integer(sub("^([0-9]{4}).*$", "\\1", x)))
}

data_vintage_period_bounds <- function(data) {
  if ("date" %in% names(data)) {
    dates <- suppressWarnings(as.Date(data$date))
    dates <- dates[!is.na(dates)]
    if (length(dates) > 0) {
      return(c(
        period_min = format(min(dates), "%Y-%m-%d"),
        period_max = format(max(dates), "%Y-%m-%d")
      ))
    }
  }

  if ("survey_year" %in% names(data)) {
    survey_years <- unique(as.character(data$survey_year))
    survey_years <- survey_years[!is.na(survey_years) & nzchar(survey_years)]
    if (length(survey_years) > 0) {
      order_key <- data_vintage_survey_order(survey_years)
      survey_years <- survey_years[order(order_key, survey_years, na.last = TRUE)]
      return(c(
        period_min = survey_years[[1]],
        period_max = survey_years[[length(survey_years)]]
      ))
    }
  }

  c(period_min = "", period_max = "")
}

data_vintage_csv_files <- function(data_dir = project_path("data")) {
  files <- sort(list.files(data_dir, pattern = "[.]csv$", full.names = TRUE))
  files[basename(files) != data_vintage_filename()]
}

data_vintage_row <- function(path, refreshed_at) {
  filename <- basename(path)
  file_info <- file.info(path)
  data <- tryCatch(
    readr::read_csv(path, show_col_types = FALSE, progress = FALSE),
    error = function(e) NULL
  )

  if (is.null(data)) {
    periods <- c(period_min = "", period_max = "")
    rows <- NA_integer_
  } else {
    periods <- data_vintage_period_bounds(data)
    rows <- nrow(data)
  }

  data.frame(
    dataset = tools::file_path_sans_ext(filename),
    file = file.path("data", filename),
    rows = rows,
    period_min = unname(periods[["period_min"]]),
    period_max = unname(periods[["period_max"]]),
    modified_utc = data_vintage_format_utc(file_info$mtime),
    refreshed_at_utc = data_vintage_format_utc(refreshed_at),
    source_group = data_vintage_source_group(filename),
    stringsAsFactors = FALSE
  )
}

build_data_vintage <- function(data_dir = project_path("data"),
                               refreshed_at = Sys.time()) {
  files <- data_vintage_csv_files(data_dir)
  if (length(files) == 0) {
    return(data.frame(
      dataset = character(),
      file = character(),
      rows = integer(),
      period_min = character(),
      period_max = character(),
      modified_utc = character(),
      refreshed_at_utc = character(),
      source_group = character(),
      stringsAsFactors = FALSE
    ))
  }

  do.call(rbind, lapply(files, data_vintage_row, refreshed_at = refreshed_at))
}

write_data_vintage <- function(data_dir = project_path("data"),
                               refreshed_at = Sys.time()) {
  vintage <- build_data_vintage(data_dir = data_dir, refreshed_at = refreshed_at)
  path <- file.path(data_dir, data_vintage_filename())
  readr::write_csv(vintage, path)
  cat("  Wrote", nrow(vintage), "rows to", data_vintage_filename(), "\n")
  invisible(vintage)
}

read_data_vintage <- function(data_dir = project_path("data"),
                              fallback = TRUE) {
  path <- if (basename(data_dir) == data_vintage_filename()) {
    data_dir
  } else {
    file.path(data_dir, data_vintage_filename())
  }
  source_data_dir <- if (basename(data_dir) == data_vintage_filename()) {
    dirname(data_dir)
  } else {
    data_dir
  }
  if (file.exists(path)) {
    vintage <- readr::read_csv(path, show_col_types = FALSE, progress = FALSE)
    attr(vintage, "fallback") <- FALSE
    return(vintage)
  }

  if (!isTRUE(fallback)) {
    stop("data/data_vintage.csv is missing.", call. = FALSE)
  }

  files <- data_vintage_csv_files(source_data_dir)
  file_times <- file.info(files)$mtime
  refreshed_at <- if (length(file_times) > 0 && any(!is.na(file_times))) {
    max(file_times, na.rm = TRUE)
  } else {
    Sys.time()
  }
  vintage <- build_data_vintage(
    data_dir = source_data_dir,
    refreshed_at = refreshed_at
  )
  attr(vintage, "fallback") <- TRUE
  vintage
}

data_vintage_latest_period <- function(vintage, groups) {
  if (nrow(vintage) == 0 || !"source_group" %in% names(vintage)) {
    return("")
  }
  candidates <- vintage[
    vintage$source_group %in% groups &
      !is.na(vintage$period_max) &
      nzchar(vintage$period_max),
    "period_max",
    drop = TRUE
  ]
  if (length(candidates) == 0) {
    return("")
  }
  max(candidates)
}

data_vintage_summary <- function(vintage = read_data_vintage(),
                                 tz = "Australia/Brisbane",
                                 fallback = attr(vintage, "fallback")) {
  if (nrow(vintage) == 0) {
    return("Data vintage unavailable")
  }

  refreshed <- data_vintage_parse_utc(vintage$refreshed_at_utc[[1]])
  refreshed_label <- format(refreshed, "%d %b %Y %H:%M %Z", tz = tz)
  latest_live <- data_vintage_latest_period(
    vintage,
    c("ABS live time series", "RBA live tables")
  )
  latest_sih <- data_vintage_latest_period(
    vintage,
    "Static ABS SIH workbook outputs"
  )

  if (isTRUE(fallback)) {
    paste0(
      "Data vintage derived from bundled CSVs | Latest ABS/RBA observation: ",
      latest_live,
      " | SIH: ",
      latest_sih
    )
  } else {
    paste0(
      "Data refreshed ",
      refreshed_label,
      " | Latest ABS/RBA observation: ",
      latest_live,
      " | SIH: ",
      latest_sih
    )
  }
}

data_vintage_detail <- function(vintage = read_data_vintage()) {
  if (nrow(vintage) == 0) {
    return("No dashboard CSV inventory available.")
  }

  paste(
    sprintf(
      "%s: %s to %s; %s rows; %s",
      basename(vintage$file),
      ifelse(nzchar(vintage$period_min), vintage$period_min, "not dated"),
      ifelse(nzchar(vintage$period_max), vintage$period_max, "not dated"),
      vintage$rows,
      vintage$source_group
    ),
    collapse = " | "
  )
}

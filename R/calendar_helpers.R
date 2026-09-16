# Calendar comparisons for a single series. No nearest-observation fallback.
calendar_prior_values <- function(date, value, months_back = 12L) {
  date <- as.Date(date)
  if (length(date) != length(value) || anyNA(date) || anyDuplicated(date)) {
    stop("A series requires unique, non-missing date keys.", call. = FALSE)
  }
  if (!is.numeric(months_back) || length(months_back) != 1L ||
      !is.finite(months_back) || months_back <= 0 || months_back %% 1 != 0) {
    stop("months_back must be a positive whole number.", call. = FALSE)
  }
  target <- lubridate::add_with_rollback(date, lubridate::period(month = -months_back))
  as.numeric(value[match(target, date)])
}

calendar_growth <- function(date, value, months_back = 12L) {
  prior <- calendar_prior_values(date, value, months_back)
  result <- rep(NA_real_, length(value))
  keep <- is.finite(value) & is.finite(prior) & prior != 0
  result[keep] <- 100 * (value[keep] / prior[keep] - 1)
  result
}

calendar_quarter_total <- function(date, value, end_date = NULL) {
  date <- as.Date(lubridate::floor_date(as.Date(date), "quarter"))
  if (length(date) != length(value) || anyNA(date) || anyDuplicated(date)) {
    stop("A series requires unique, non-missing quarter keys.", call. = FALSE)
  }
  if (!length(date)) return(NA_real_)
  if (is.null(end_date)) end_date <- max(date)
  keys <- seq(as.Date(lubridate::floor_date(end_date, "quarter")),
              by = "-3 months", length.out = 4L)
  values <- value[match(keys, date)]
  if (any(!is.finite(values))) return(NA_real_)
  sum(values)
}

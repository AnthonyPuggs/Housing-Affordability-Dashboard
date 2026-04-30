source("plot_setup.R")

failures <- character()

check <- function(condition, message) {
  if (!isTRUE(condition)) {
    failures <<- c(failures, message)
  }
}

series_df <- data.frame(
  date = as.Date(c("2024-01-01", "2024-04-01", "2024-07-01", "2024-10-01",
                   "2025-01-01", "2025-04-01", "2025-07-01", "2025-10-01",
                   "2026-01-01", "2026-04-01", "2026-07-01", "2026-10-01",
                   "2027-01-01")),
  series = "Test Series",
  value = c(100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 110),
  stringsAsFactors = FALSE
)

relative_change <- latest_change(
  series_df,
  "series",
  "Test Series",
  periods_back = 12,
  period_label = "YoY",
  change_type = "relative_pct"
)
check(
  identical(relative_change$label, "\u2191 +10.0% YoY"),
  paste("relative_pct label was", relative_change$label)
)
check(
  !grepl("QoQ", relative_change$label, fixed = TRUE),
  "periods_back = 12 with period_label = YoY returned a QoQ label"
)

rate_df <- data.frame(
  date = as.Date(c("2025-01-01", "2026-01-01")),
  series = "Rate Series",
  value = c(4.0, 4.4),
  stringsAsFactors = FALSE
)

pp_change <- latest_change(
  rate_df,
  "series",
  "Rate Series",
  periods_back = 1,
  period_label = "YoY",
  change_type = "percentage_points"
)
check(
  identical(pp_change$label, "\u2191 +0.4 pp YoY"),
  paste("percentage_points label was", pp_change$label)
)

short_df <- data.frame(
  date = as.Date("2026-01-01"),
  series = "Short Series",
  value = 1,
  stringsAsFactors = FALSE
)

missing_change <- latest_change(
  short_df,
  "series",
  "Short Series",
  periods_back = 4,
  period_label = "YoY",
  change_type = "relative_pct"
)
check(is.na(missing_change$change), "insufficient history should return NA change")
check(identical(missing_change$label, ""), "insufficient history should return blank label")

invalid_change_type <- tryCatch(
  latest_change(
    rate_df,
    "series",
    "Rate Series",
    periods_back = 1,
    period_label = "YoY",
    change_type = "ratio"
  ),
  error = function(e) e
)
check(inherits(invalid_change_type, "error"), "invalid change_type should error")

if (length(failures) > 0) {
  stop(
    paste(c("KPI change label checks failed:", paste0("- ", failures)),
          collapse = "\n"),
    call. = FALSE
  )
}

cat("KPI change label checks passed.\n")

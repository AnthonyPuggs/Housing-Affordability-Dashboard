# Shared dashboard formatting and labelling helpers.

latest_val <- function(df, series_col, series_name, val_col = "value",
                       date_col = "date") {
  d <- df %>%
    filter(.data[[series_col]] == series_name, !is.na(.data[[val_col]])) %>%
    arrange(desc(.data[[date_col]]))
  if (nrow(d) == 0) return(NA_real_)
  d[[val_col]][1]
}

latest_date <- function(df, series_col, series_name, date_col = "date") {
  d <- df %>%
    filter(.data[[series_col]] == series_name, !is.na(value)) %>%
    arrange(desc(.data[[date_col]]))
  if (nrow(d) == 0) return(NA_character_)
  format(d[[date_col]][1], "%b %Y")
}

latest_change <- function(df, series_col, series_name, val_col = "value",
                          date_col = "date", periods_back = 4,
                          period_label = NULL,
                          change_type = c("relative_pct", "percentage_points")) {
  change_type <- match.arg(change_type)
  if (is.null(period_label)) {
    period_label <- case_when(
      periods_back %in% c(4, 12) ~ "YoY",
      periods_back == 1 ~ "QoQ",
      TRUE ~ paste0(periods_back, "-period")
    )
  }

  d <- df %>%
    filter(.data[[series_col]] == series_name, !is.na(.data[[val_col]])) %>%
    arrange(desc(.data[[date_col]]))
  if (nrow(d) < periods_back + 1) return(list(change = NA_real_, label = ""))
  current <- d[[val_col]][1]
  previous <- d[[val_col]][periods_back + 1]
  if (is.na(previous)) return(list(change = NA_real_, label = ""))

  if (identical(change_type, "relative_pct")) {
    if (previous == 0) return(list(change = NA_real_, label = ""))
    change <- (current / previous - 1) * 100
    suffix <- "%"
  } else {
    change <- current - previous
    suffix <- " pp"
  }

  direction <- if (change >= 0) "\u2191" else "\u2193"
  list(
    change = change,
    label = paste0(direction, " ", sprintf("%+.1f", change), suffix, " ", period_label)
  )
}

fmt_dollar <- function(x) {
  if (is.na(x)) "N/A" else paste0("$", comma(round(x)))
}

fmt_dollar_k <- function(x) {
  if (is.na(x)) return("N/A")
  if (abs(x) >= 1e6) {
    paste0("$", number(x / 1e6, accuracy = 0.01), "M")
  } else if (abs(x) >= 1e3) {
    paste0("$", comma(round(x / 1e3)), "k")
  } else {
    paste0("$", comma(round(x)))
  }
}

fmt_pct <- function(x, acc = 0.01) {
  if (is.na(x)) "N/A" else paste0(number(x, accuracy = acc), "%")
}

fmt_ratio <- function(x) {
  if (is.na(x)) "N/A" else number(x, accuracy = 0.1)
}

fmt_years <- function(x) {
  if (is.na(x)) "N/A" else paste0(number(x, accuracy = 0.1), " yrs")
}

fmt_index <- function(x) {
  if (is.na(x)) "N/A" else number(x, accuracy = 0.1)
}

fmt_number <- function(x) {
  if (is.na(x)) "N/A" else comma(round(x))
}

tenure_labels <- c(
  "owner_outright" = "Owner (no mortgage)",
  "owner_mortgage" = "Owner (with mortgage)",
  "owner_total" = "All Owners",
  "renter_private" = "Private Renter",
  "renter_social" = "Social Renter",
  "renter_total" = "All Renters",
  "all" = "All Households"
)

label_tenure <- function(x) {
  ifelse(x %in% names(tenure_labels), tenure_labels[x], x)
}

repel_labels <- function(y, min_gap) {
  ord <- order(y)
  pos <- y[ord]
  for (i in seq_along(pos)[-1]) {
    if (pos[i] - pos[i - 1] < min_gap) {
      pos[i] <- pos[i - 1] + min_gap
    }
  }

  shift <- mean(y[ord]) - mean(pos)
  pos <- pos + shift

  for (i in seq_along(pos)[-1]) {
    if (pos[i] - pos[i - 1] < min_gap) {
      pos[i] <- pos[i - 1] + min_gap
    }
  }
  out <- numeric(length(y))
  out[ord] <- pos
  out
}

extract_city <- function(s) {
  str_trim(str_extract(s, ";\\s*([^;]+)\\s*;?$") %>%
             str_remove_all(";") %>%
             str_trim())
}

city_colours <- c(
  "Sydney" = "#e41a1c",
  "Melbourne" = "#377eb8",
  "Brisbane" = "#4daf4a",
  "Adelaide" = "#984ea3",
  "Perth" = "#ff7f00",
  "Hobart" = "#a65628",
  "Darwin" = "#f781bf",
  "Canberra" = "#999999",
  "Weighted average of eight capital cities" = "#000000"
)

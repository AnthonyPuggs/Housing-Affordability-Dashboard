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

replace_missing_labels <- function(x, labels) {
  labels <- as.character(labels)
  labels[is.na(x)] <- "N/A"
  labels
}

fmt_dollar <- function(x) {
  replace_missing_labels(x, paste0("$", comma(round(x))))
}

fmt_dollar_k <- function(x) {
  labels <- ifelse(
    abs(x) >= 1e6,
    paste0("$", number(x / 1e6, accuracy = 0.01), "M"),
    ifelse(
      abs(x) >= 1e3,
      paste0("$", comma(round(x / 1e3)), "k"),
      paste0("$", comma(round(x)))
    )
  )
  replace_missing_labels(x, labels)
}

fmt_pct <- function(x, acc = 0.01) {
  replace_missing_labels(x, paste0(number(x, accuracy = acc), "%"))
}

fmt_ratio <- function(x) {
  replace_missing_labels(x, number(x, accuracy = 0.1))
}

fmt_years <- function(x) {
  replace_missing_labels(x, paste0(number(x, accuracy = 0.1), " yrs"))
}

fmt_index <- function(x) {
  replace_missing_labels(x, number(x, accuracy = 0.1))
}

fmt_number <- function(x) {
  replace_missing_labels(x, comma(round(x)))
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

# Colour-blind-safe city palette (UX-02). Replaces the old ColorBrewer Set1
# (Sydney red vs Brisbane green were the classic red-green confusion pair) with
# the Okabe-Ito qualitative palette — the same colour-blind-safe family the
# semantic chart colours use — plus neutral grey for the eighth capital. The
# national reference series is black (light mode) and a light tone in dark mode.
city_colours <- c(
  "Sydney" = "#0072B2",     # Okabe-Ito blue
  "Melbourne" = "#E69F00",  # Okabe-Ito orange
  "Brisbane" = "#009E73",   # Okabe-Ito bluish green
  "Adelaide" = "#CC79A7",   # Okabe-Ito reddish purple
  "Perth" = "#D55E00",      # Okabe-Ito vermillion
  "Hobart" = "#56B4E9",     # Okabe-Ito sky blue
  "Darwin" = "#F0E442",     # Okabe-Ito yellow
  "Canberra" = "#999999",   # neutral grey (8th, achromatic so CB-distinct)
  "Weighted average of eight capital cities" = "#000000",
  "National Avg" = "#000000",
  # State/territory series (all-dwellings mean-price indexes) reuse their
  # capital's hue so the palette stays stable across dwelling-type views.
  "New South Wales" = "#0072B2",
  "Victoria" = "#E69F00",
  "Queensland" = "#009E73",
  "South Australia" = "#CC79A7",
  "Western Australia" = "#D55E00",
  "Tasmania" = "#56B4E9",
  "Northern Territory" = "#F0E442",
  "Australian Capital Territory" = "#999999",
  "Australia" = "#000000"
)

# Theme-aware city palette: the national reference lines are pure black,
# which is invisible against the dark panel background (~1.2:1 contrast), so
# dark mode swaps them for a light tone. Chart builders pass their dark flag.
city_palette <- function(dark = FALSE) {
  colours <- city_colours
  if (isTRUE(dark)) {
    colours[["Weighted average of eight capital cities"]] <- "#E8EEF7"
    colours[["National Avg"]] <- "#E8EEF7"
    colours[["Australia"]] <- "#E8EEF7"
  }
  colours
}

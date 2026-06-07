# Shared helpers for contextual KPI calculations.

latest_capital_price_extreme <- function(data,
                                         direction = c("highest", "lowest"),
                                         city_col = "city",
                                         value_col = "value",
                                         date_col = "date") {
  direction <- match.arg(direction)
  required <- c(city_col, value_col, date_col)
  missing <- setdiff(required, names(data))
  if (length(missing) > 0 || nrow(data) == 0) {
    return(data.frame(
      city = character(), value = numeric(), date = as.Date(character()),
      stringsAsFactors = FALSE
    ))
  }

  excluded <- c("National Avg", "Australia", "Aust.", "National")
  d <- data[!is.na(data[[value_col]]) &
              !is.na(data[[date_col]]) &
              !(data[[city_col]] %in% excluded), , drop = FALSE]
  if (nrow(d) == 0) {
    return(data.frame(
      city = character(), value = numeric(), date = as.Date(character()),
      stringsAsFactors = FALSE
    ))
  }

  latest <- max(as.Date(d[[date_col]]), na.rm = TRUE)
  d <- d[as.Date(d[[date_col]]) == latest, , drop = FALSE]
  idx <- if (identical(direction, "highest")) {
    which.max(d[[value_col]])
  } else {
    which.min(d[[value_col]])
  }

  data.frame(
    city = as.character(d[[city_col]][idx]),
    value = as.numeric(d[[value_col]][idx]),
    date = as.Date(d[[date_col]][idx]),
    stringsAsFactors = FALSE
  )
}

contextual_approval_series_components <- function(series) {
  state_labels <- c(
    "New South Wales" = "NSW",
    "Victoria" = "VIC",
    "Queensland" = "QLD",
    "South Australia" = "SA",
    "Western Australia" = "WA",
    "Tasmania" = "TAS",
    "Northern Territory" = "NT",
    "Australian Capital Territory" = "ACT",
    "Australia" = "AUS"
  )

  parse_one <- function(x) {
    parts <- trimws(strsplit(as.character(x), ";", fixed = TRUE)[[1]])
    parts <- parts[nzchar(parts)]

    approval_state <- if (length(parts) >= 2) parts[[2]] else NA_character_
    approval_building_type_raw <- if (length(parts) >= 3) {
      parts[[3]]
    } else {
      NA_character_
    }
    approval_sector_raw <- if (length(parts) >= 4) {
      parts[[4]]
    } else {
      NA_character_
    }

    approval_building_type <- dplyr::case_when(
      approval_building_type_raw == "Total (Type of Building)" ~
        "Total approvals",
      approval_building_type_raw == "Houses" ~ "Houses",
      approval_building_type_raw == "Dwellings excluding houses" ~
        "Dwellings excluding houses",
      TRUE ~ approval_building_type_raw
    )

    approval_sector <- dplyr::case_when(
      approval_sector_raw == "Total Sectors" ~ "Total sectors",
      approval_sector_raw == "Private Sector" ~ "Private sector",
      TRUE ~ approval_sector_raw
    )

    label <- if (approval_state %in% names(state_labels)) {
      state_labels[[approval_state]]
    } else {
      approval_state
    }

    data.frame(
      series = as.character(x),
      approval_state = approval_state,
      approval_building_type = approval_building_type,
      approval_sector = approval_sector,
      approval_label = label,
      stringsAsFactors = FALSE
    )
  }

  dplyr::bind_rows(lapply(unique(as.character(series)), parse_one))
}

available_supply_states <- function(data = NULL) {
  fallback <- c("New South Wales", "Victoria")
  if (is.null(data) || nrow(data) == 0 || !"series" %in% names(data)) {
    return(fallback)
  }

  components <- contextual_approval_series_components(unique(data$series))
  states <- sort(unique(components$approval_state))
  states <- states[!is.na(states) & nzchar(states) & states != "Australia"]
  if (length(states) == 0) fallback else states
}

approval_filtered_rows <- function(data, states, building_type, sector) {
  empty <- data.frame()
  if (nrow(data) == 0 || !"series" %in% names(data)) {
    return(empty)
  }

  components <- contextual_approval_series_components(unique(data$series))
  d <- dplyr::left_join(data, components, by = "series")
  if ("category" %in% names(d)) {
    d <- d[d$category == "Building Approvals", , drop = FALSE]
  }

  d <- d[!is.na(d$value) &
           d$approval_state %in% states &
           d$approval_building_type == building_type &
           d$approval_sector == sector, , drop = FALSE]
  d
}

selected_approvals_latest <- function(data, states, building_type, sector) {
  d <- approval_filtered_rows(data, states, building_type, sector)
  if (nrow(d) == 0) {
    return(data.frame(
      date = as.Date(character()), value = numeric(), n_jurisdictions = integer(),
      states_label = character(), stringsAsFactors = FALSE
    ))
  }

  latest <- max(as.Date(d$date), na.rm = TRUE)
  current <- d[as.Date(d$date) == latest, , drop = FALSE]
  data.frame(
    date = latest,
    value = sum(current$value, na.rm = TRUE),
    n_jurisdictions = length(unique(current$approval_state)),
    states_label = paste(sort(unique(current$approval_label)), collapse = ", "),
    stringsAsFactors = FALSE
  )
}

largest_selected_approval <- function(data, states, building_type, sector) {
  d <- approval_filtered_rows(data, states, building_type, sector)
  if (nrow(d) == 0) {
    return(data.frame(
      date = as.Date(character()), approval_state = character(),
      approval_label = character(), value = numeric(), stringsAsFactors = FALSE
    ))
  }

  latest <- max(as.Date(d$date), na.rm = TRUE)
  current <- d[as.Date(d$date) == latest, , drop = FALSE]
  by_state <- stats::aggregate(
    value ~ approval_state + approval_label,
    data = current,
    FUN = sum,
    na.rm = TRUE
  )
  by_state$date <- latest
  by_state[order(-by_state$value), c("date", "approval_state",
                                     "approval_label", "value")][1, ,
                                                                  drop = FALSE]
}

selected_approvals_yoy_change <- function(data, states, building_type, sector) {
  d <- approval_filtered_rows(data, states, building_type, sector)
  if (nrow(d) == 0) {
    return(data.frame(change = NA_real_, label = "", stringsAsFactors = FALSE))
  }

  by_date <- stats::aggregate(value ~ date, data = d, FUN = sum, na.rm = TRUE)
  by_date$date <- as.Date(by_date$date)
  by_date <- by_date[order(by_date$date, decreasing = TRUE), , drop = FALSE]
  if (nrow(by_date) < 2) {
    return(data.frame(change = NA_real_, label = "", stringsAsFactors = FALSE))
  }

  latest_date <- by_date$date[1]
  prior_target <- as.Date(sprintf(
    "%d-%s",
    as.integer(format(latest_date, "%Y")) - 1L,
    format(latest_date, "%m-%d")
  ))
  prior <- by_date[by_date$date == prior_target, , drop = FALSE]
  if (nrow(prior) == 0) {
    prior <- by_date[by_date$date < latest_date, , drop = FALSE][1, ,
                                                                  drop = FALSE]
  }
  if (nrow(prior) == 0 || is.na(prior$value[1]) || prior$value[1] == 0) {
    return(data.frame(change = NA_real_, label = "", stringsAsFactors = FALSE))
  }

  change <- (by_date$value[1] / prior$value[1] - 1) * 100
  direction <- if (change >= 0) "\u2191" else "\u2193"
  data.frame(
    change = change,
    label = paste0(direction, " ", sprintf("%+.1f%%", change), " YoY"),
    stringsAsFactors = FALSE
  )
}

# Shared visual semantics for dashboard KPI and chart colours.

semantic_colours <- function() {
  c(
    better = "#0072B2",
    worse = "#D55E00",
    neutral = "#6C757D",
    caution = "#E69F00",
    reference = "#4D4D4D",
    reference_dark = "#F0E442",
    categorical_blue = "#56B4E9",
    categorical_purple = "#CC79A7",
    categorical_teal = "#009E73",
    categorical_navy = "#1F4E79"
  )
}

semantic_colour <- function(name) {
  colours <- semantic_colours()
  if (length(name) != 1 || !name %in% names(colours)) {
    stop("Unknown semantic colour: ", name, call. = FALSE)
  }
  unname(colours[[name]])
}

kpi_change_class <- function(change,
                             favourable = c("increase", "decrease", "neutral")) {
  favourable <- match.arg(favourable)
  if (identical(favourable, "neutral") || length(change) != 1 ||
      !is.numeric(change) || !is.finite(change) || change == 0) {
    return("kpi-change-neutral")
  }

  if (identical(favourable, "increase")) {
    if (change > 0) "kpi-change-better" else "kpi-change-worse"
  } else {
    if (change < 0) "kpi-change-better" else "kpi-change-worse"
  }
}

stress_band_palette <- function() {
  c(
    "<25%" = semantic_colour("better"),
    "25-30%" = semantic_colour("caution"),
    "30-50%" = semantic_colour("worse"),
    ">50%" = semantic_colour("categorical_purple")
  )
}

burden_gradient_colours <- function() {
  c(
    low = semantic_colour("better"),
    mid = semantic_colour("caution"),
    high = semantic_colour("worse")
  )
}

cost_pressure_palette <- function(labels = NULL) {
  palette <- c(
    "Rent Cost Pressure" = semantic_colour("categorical_blue"),
    "Modelled Mortgage Cost Pressure" = semantic_colour("worse"),
    "Price-to-Income Cost Pressure" = semantic_colour("categorical_navy"),
    "Stylised Deposit Gap (Years)" = semantic_colour("categorical_purple")
  )

  if (is.null(labels)) {
    return(palette)
  }
  palette[labels]
}

rental_stress_gradient_colours <- function() {
  c(
    low = semantic_colour("better"),
    mid = semantic_colour("caution"),
    high = semantic_colour("worse")
  )
}

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

# Contrast-aware label colour for diverging-fill heatmap tiles (UX-11): black
# text is unreadable on the dark ends of a blue->orange->vermillion fill, so
# reproduce scale_fill_gradient2()'s diverging interpolation around the midpoint
# and pick each tile's label colour from the fill's WCAG relative luminance —
# light ink on dark tiles, dark ink on light ones. Non-finite values fall back
# to dark ink. Returns a character vector the same length as `values`.
tile_contrast_text_colour <- function(values, low, mid, high, midpoint,
                                      dark_text = "#172033",
                                      light_text = "#FFFFFF",
                                      luminance_threshold = 0.4) {
  out <- rep(dark_text, length(values))
  finite <- is.finite(values)
  if (!any(finite)) {
    return(out)
  }
  rescaled <- scales::rescale_mid(values[finite], mid = midpoint)
  fills <- scales::div_gradient_pal(low = low, mid = mid, high = high)(rescaled)
  fills[is.na(fills)] <- mid
  channels <- grDevices::col2rgb(fills) / 255
  channels <- ifelse(channels <= 0.03928, channels / 12.92,
                     ((channels + 0.055) / 1.055)^2.4)
  luminance <- 0.2126 * channels[1, ] + 0.7152 * channels[2, ] +
    0.0722 * channels[3, ]
  out[finite] <- ifelse(luminance < luminance_threshold, light_text, dark_text)
  out
}

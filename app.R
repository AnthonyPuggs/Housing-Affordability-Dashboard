# =============================================================================
# Australian Macroeconomic Dashboard — Shiny + readabs
# =============================================================================
#
# PURPOSE:
#   Interactive dashboard pulling key Australian macro time series from the
#   Australian Bureau of Statistics (ABS) via the {readabs} package.
#
# ECONOMIC CONTEXT:
#   The dashboard tracks the core variables of a standard open-economy macro
#   model: output (GDP), prices (CPI), the labour market (employment, unemp.
#   rate, participation rate), wages, and the policy rate.
#
#   Time series transforms matter for stationarity. Most macro aggregates
#   (GDP, CPI level, employment) are I(1) — they contain a unit root and
#   are non-stationary in levels. Year-over-year percentage changes or
#   first differences render them (approximately) stationary, which is why
#   the "Transform" controls are provided. Rates (unemployment, cash rate)
#   are typically already I(0) or borderline, so "Levels" is often fine.
#
# PACKAGES:
#   shiny   — reactive web application framework
#   bslib   — Bootstrap 5 theming for Shiny
#   ggplot2 — grammar-of-graphics plotting
#   plotly  — interactive wrapper around ggplot
#   readabs — tidy interface to ABS time series data
#   dplyr, tidyr, purrr, stringr, scales — Tidyverse data wrangling
#
# RUNNING:
#   shiny::runApp("path/to/this/directory")
# =============================================================================

# --- Load required libraries -------------------------------------------------
library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(scales)
library(plotly)
library(readabs)

# Enable live-reload during development (saves re-launching the app manually)
options(shiny.autoreload = TRUE)


# =============================================================================
# SECTION 1 — HELPER FUNCTIONS FOR DATA RETRIEVAL
# =============================================================================

#' Safely evaluate an expression, returning an empty tibble on failure.
#' This wraps readabs calls so one failed ABS download doesn't crash the
#' entire app. The `expr` argument is lazily evaluated inside tryCatch.
#'
#' @param expr  An R expression (typically a readabs call)
#' @param label  Human-readable label for warning messages
#' @param warn  If TRUE, emit a warning on failure (useful for debugging)
#' @return A tibble — either the successful result or an empty tibble
safe_read <- function(expr, label, warn = TRUE) {
  tryCatch(
    expr,
    error = function(e) {
      if (isTRUE(warn)) {
        warning(paste0("Failed to load ", label, ": ", conditionMessage(e)))
      }
      tibble()
    }
  )
}


# =============================================================================
# SECTION 2 — NORMALISATION & SELECTION HELPERS
# =============================================================================
# readabs returns tibbles with columns: date, value, series, series_id,
# unit, frequency, series_type, table_no, etc.
# We normalise everything into a consistent 7-column schema so that all
# series can be stacked together via bind_rows().

#' Normalise a readabs data frame into our standard schema.
#'
#' @param df        A tibble returned by read_abs(), read_cpi(), etc.
#' @param label     If non-NULL, overrides the `series` column with this name
#' @param category  Grouping category (e.g., "Output", "Prices", "Labour Market")
#' @param units     Fallback unit string if `unit` column is absent
#' @param freq_hint Fallback frequency string if `frequency` column is absent
#' @return A tibble with columns: date, value, series, series_id,
#'         category, unit, frequency
normalize_abs <- function(df,
                          label = NULL,
                          category = "Other",
                          units = NA_character_,
                          freq_hint = NA_character_) {
  # Guard clauses: return empty tibble if input is useless

  if (nrow(df) == 0) return(tibble())
  if (!"date"  %in% names(df)) return(tibble())
  if (!"value" %in% names(df)) return(tibble())

  out <- df %>%
    mutate(
      date  = as.Date(date),
      value = as.numeric(value),
      # Use existing columns if present, otherwise fall back to arguments
      series    = if ("series"    %in% names(df)) as.character(series)    else ifelse(is.null(label), "Series", label),
      series_id = if ("series_id" %in% names(df)) as.character(series_id) else NA_character_,
      unit      = if ("unit"      %in% names(df)) as.character(unit)      else units,
      frequency = if ("frequency" %in% names(df)) as.character(frequency) else freq_hint
    ) %>%
    filter(!is.na(date), !is.na(value))

  # Override series name when an explicit label is provided
  if (!is.null(label)) {
    out$series <- label
  }

  out %>%
    mutate(category = category) %>%
    select(date, value, series, series_id, category, unit, frequency)
}


#' Filter a readabs tibble by regex on the `series` column, then normalise.
#' This is handy because a single ABS table often contains dozens of series;
#' we want to pick out the one matching our pattern.
#'
#' @param df       A readabs tibble (wide set of series from one table)
#' @param pattern  Regex pattern to match against the `series` column
#' @param label    Friendly name for the matched series
#' @param category Category grouping for the sidebar filter
#' @param units    Fallback units string
#' @return A normalised tibble (or empty tibble if no match)
select_series <- function(df, pattern, label, category, units = NA_character_) {
  if (nrow(df) == 0 || !"series" %in% names(df)) return(tibble())

  # Case-insensitive regex match on the ABS series description

  matched <- df %>%
    filter(str_detect(series, regex(pattern, ignore_case = TRUE)))

  if (nrow(matched) == 0) return(tibble())

  normalize_abs(matched, label = label, category = category, units = units)
}


# =============================================================================
# SECTION 3 — LAG INFERENCE FOR YEAR-OVER-YEAR CALCULATIONS
# =============================================================================
# To compute YoY % change we need to know how many observations correspond
# to one year:
#   • Monthly data  → lag 12
#   • Quarterly data → lag 4
#   • Annual data   → lag 1
#
# We try two approaches and take the most reliable:
#   (a) Parse the `frequency` column text from readabs ("Quarter", "Month").
#   (b) Compute the median gap between consecutive dates empirically.

#' Infer the number of periods per year from a frequency label string.
#' @param freq_label Character, e.g. "Quarter", "Month"
#' @return Integer: 4 (quarterly), 12 (monthly), 1 (annual), default 12
infer_lag <- function(freq_label) {
  case_when(
    # %||% "" provides a safe default if freq_label is NULL
    str_detect(freq_label %||% "", regex("quarter", ignore_case = TRUE)) ~ 4L,
    str_detect(freq_label %||% "", regex("month",   ignore_case = TRUE)) ~ 12L,
    str_detect(freq_label %||% "", regex("year",    ignore_case = TRUE)) ~ 1L,
    TRUE ~ 12L
  )
}


#' Infer the number of periods per year from the actual date column.
#' Falls back to `fallback` when there aren't enough observations.
#'
#' @param dates    A vector of Date objects
#' @param fallback Default lag if we can't infer (e.g. too few data points)
#' @return Integer: 12, 4, or 1
infer_lag_from_dates <- function(dates, fallback = 12L) {
  d <- sort(unique(as.Date(dates)))
  if (length(d) < 3) return(fallback)

  # Median gap in days between consecutive observations
  median_gap <- median(as.numeric(diff(d)), na.rm = TRUE)

  if (is.na(median_gap)) return(fallback)

  # Classify: ~30 days = monthly, ~90 days = quarterly, ~365 = annual
  if (median_gap <= 40)  return(12L)
  if (median_gap <= 120) return(4L)
  1L
}


# =============================================================================
# SECTION 4 — MAIN DATA RETRIEVAL FUNCTION
# =============================================================================
# Fetches all core macro series from the ABS in one go.
# Catalogue numbers used:
#   5206.0 — Australian National Accounts: National Income, Expenditure and
#            Product (quarterly). Tables 1 & 2 contain chain-volume (real)
#            and current-price (nominal) aggregates.
#   6202.0 — Labour Force, Australia (monthly). Table 1 has headline
#            employment, unemployment rate, and participation rate.
#   6345.0 — Wage Price Index, Australia (quarterly). Table 1 has the
#            aggregate WPI (total hourly rates, private + public).
#   read_cpi() — convenience wrapper returning the CPI All Groups index.
#   read_awe() — convenience wrapper returning Average Weekly Earnings.

get_macro_data <- function(custom_ids = character(), nominal_rate_id = "") {

  # ---------------------------------------------------------------------------
  # 4a. National Accounts (ABS 5206.0)
  # ---------------------------------------------------------------------------
  # Table 1 = chain-volume (real) measures;  Table 2 = current-price (nominal).
  # GDP is the broadest measure of aggregate output. Household consumption is
  # the largest expenditure component (~55–60% of GDP).
  na_real <- safe_read(
    read_abs(cat_no = "5206.0", tables = "1"),
    "National accounts table 1 (chain volume)"
  )
  na_nom <- safe_read(
    read_abs(cat_no = "5206.0", tables = "2"),
    "National accounts table 2 (current prices)"
  )

  gdp_real <- select_series(
    na_real,
    "Gross domestic product.*Chain volume",
    "Real GDP (chain volume, $m)",
    "Output"
  )

  gdp_nominal <- select_series(
    na_nom,
    "Gross domestic product.*Current prices",
    "Nominal GDP (current prices, $m)",
    "Output"
  )

  consumption_real <- select_series(
    na_real,
    "Household final consumption expenditure.*Chain volume",
    "Real Household Consumption",
    "Consumption"
  )

  consumption_nominal <- select_series(
    na_nom,
    "Household final consumption expenditure.*Current prices",
    "Nominal Household Consumption",
    "Consumption"
  )

  # ---------------------------------------------------------------------------
  # 4b. Consumer Price Index (Quarterly)
  # ---------------------------------------------------------------------------
  # NOTE (Feb 2026): The ABS transitioned from the quarterly CPI to a complete
  # Monthly CPI in November 2025 (cat. 6401.0). The readabs convenience
  # function read_cpi() now returns the short monthly series (from April 2024
  # only), which is unsuitable for long-run analysis.
  #
  # Instead we pull the quarterly All Groups CPI directly by its ABS series ID:
  #   A2325846C — "Index Numbers ; All groups CPI ; Australia"
  # This is the quarterly CPI for the weighted average of eight capital cities,
  # with history back to September 1948. The ABS re-referenced the base period
  # from 2011-12 = 100 to Sep 2025 = 100 in December 2025, but percentage
  # changes (and hence YoY inflation) are unaffected by re-referencing.
  cpi <- safe_read(
    read_abs(series_id = "A2325846C"),
    "Quarterly CPI All Groups (A2325846C)"
  ) %>%
    normalize_abs(
      label     = "CPI (All Groups, Australia)",
      category  = "Prices",
      units     = "Index",
      freq_hint = "Quarter"
    )

  # ---------------------------------------------------------------------------
  # 4c. Labour Force (ABS 6202.0)
  # ---------------------------------------------------------------------------
  # Monthly survey-based estimates. The three key headline indicators are:
  #   • Employment — total number of employed persons
  #   • Unemployment rate — U / (U + E) as a percentage
  #   • Participation rate — Labour force / Civilian population 15+ (%)
  labour <- safe_read(
    read_abs(cat_no = "6202.0", tables = "1"),
    "Labour force table 1"
  )

  employment <- select_series(
    labour,
    "Employed.*Total.*Persons",
    "Employment (Persons, '000s)",
    "Labour Market"
  )

  unemployment_rate <- select_series(
    labour,
    "Unemployment rate.*Persons",
    "Unemployment Rate",
    "Labour Market",
    units = "Per cent"
  )

  participation_rate <- select_series(
    labour,
    "Participation rate.*Persons",
    "Participation Rate",
    "Labour Market",
    units = "Per cent"
  )

  # ---------------------------------------------------------------------------
  # 4d. Wage Indicators
  # ---------------------------------------------------------------------------
  # Two measures of wages:
  #   1. WPI (6345.0) — the Wage Price Index, which measures pure price change
  #      in wages holding the composition of the workforce fixed (analogous to
  #      the CPI for wages). This is the RBA's preferred wage growth indicator.
  #   2. AWE — Average Weekly Ordinary Time Earnings, which measures the level
  #      of actual earnings (affected by compositional shifts in employment).
  #
  # Both are useful: the WPI for "clean" wage inflation; AWE for living
  # standards and real purchasing power of workers.

  # WPI — Wage Price Index (quarterly index number, all sectors)
  wpi_raw <- safe_read(
    read_abs(cat_no = "6345.0", tables = "1"),
    "Wage Price Index table 1"
  )

  wpi <- select_series(
    wpi_raw,
    "Total hourly rates of pay excluding bonuses.*Australia.*Private and Public.*All industries",
    "WPI (All Sectors, Excl. Bonuses)",
    "Wages",
    units = "Index"
  )

  # AWE — Average Weekly Ordinary Time Earnings (levels, AUD)
  wages <- safe_read(
    read_awe(wage_measure = "awote", sex = "persons", sector = "total"),
    "Average Weekly Ordinary Time Earnings"
  ) %>%
    transmute(date, value = as.numeric(value)) %>%
    normalize_abs(
      label     = "AWE (AWOTE, Persons)",
      category  = "Wages",
      units     = "AUD",
      freq_hint = "Quarter"
    )

  # ---------------------------------------------------------------------------
  # 4e. Interest Rates
  # ---------------------------------------------------------------------------
  # The RBA cash rate target is not published by the ABS in a reliably
  # scrapeable time series catalogue. Users can optionally supply a specific
  # ABS series_id (e.g. from ABS.Stat) that contains the nominal policy rate.
  #
  # Fallback: try the (now discontinued) ABS key economic indicators (1350.0)
  # which historically included the cash rate.

  nominal_rate <- tibble()

  # Attempt 1: user-supplied series ID (most reliable if provided)
  if (nzchar(nominal_rate_id)) {
    nominal_rate <- safe_read(
      read_abs(series_id = nominal_rate_id),
      paste0("Nominal rate series ", nominal_rate_id)
    ) %>%
      normalize_abs(
        label    = paste0("Nominal Interest Rate (", nominal_rate_id, ")"),
        category = "Interest Rates",
        units    = "Per cent"
      )
  }

  # Attempt 2: look in the macro indicators catalogue (may be discontinued)
  if (nrow(nominal_rate) == 0) {
    indicators <- safe_read(
      read_abs(cat_no = "1350.0", tables = "1"),
      "Key macro indicators (1350.0)",
      warn = FALSE
    )

    nominal_rate <- select_series(
      indicators,
      "Cash rate target|Cash rate|Interbank overnight cash rate",
      "Nominal Cash Rate",
      "Interest Rates",
      units = "Per cent"
    )
  }

  # ---------------------------------------------------------------------------
  # 4f. Derived series: CPI Inflation (YoY) and Real Cash Rate
  # ---------------------------------------------------------------------------
  # CPI Inflation (YoY %):
  #   π_t = 100 × (CPI_t / CPI_{t-4} − 1)
  # where the lag of 4 corresponds to quarterly data (one year back).
  #
  # Real Cash Rate (ex-post Fisher approximation):
  #   r_t ≈ i_t − π_t
  # where i_t is the nominal cash rate and π_t is the YoY CPI inflation.
  # This is the ex-post real rate; the ex-ante rate would use expected
  # inflation, which we don't observe directly.

  # infer_lag_from_dates() automatically detects whether the CPI is quarterly
  # (lag 4) or monthly (lag 12), so the YoY calculation is correct either way.
  cpi_lag <- infer_lag_from_dates(cpi$date, fallback = 4L)

  inflation_yoy <- cpi %>%
    arrange(date) %>%
    mutate(
      # YoY percentage change: compare current period to same period last year
      value = 100 * (value / lag(value, cpi_lag) - 1)
    ) %>%
    filter(!is.na(value)) %>%
    mutate(
      series   = "CPI Inflation (YoY %)",
      category = "Prices",
      unit     = "Per cent"
    )

  # Compute WPI growth as YoY % change (useful for seeing wage-price dynamics)
  wpi_lag <- infer_lag_from_dates(wpi$date, fallback = 4L)

  wpi_growth <- wpi %>%
    arrange(date) %>%
    mutate(
      value = 100 * (value / lag(value, wpi_lag) - 1)
    ) %>%
    filter(!is.na(value)) %>%
    mutate(
      series   = "WPI Growth (YoY %)",
      category = "Wages",
      unit     = "Per cent"
    )

  # Real cash rate = nominal rate − CPI inflation (ex-post Fisher equation)
  real_rate <- tibble()
  if (nrow(nominal_rate) > 0 && nrow(inflation_yoy) > 0) {
    inflation_for_join <- inflation_yoy %>%
      select(date, inflation_yoy = value)

    real_rate <- nominal_rate %>%
      arrange(date) %>%
      # Join inflation to the rate series by date
      left_join(inflation_for_join, by = "date") %>%
      # Fill forward: if dates don't align exactly, carry the last known
      # inflation figure forward (common when mixing monthly + quarterly)
      tidyr::fill(inflation_yoy, .direction = "down") %>%
      mutate(
        value    = value - inflation_yoy,
        series   = "Real Cash Rate (Nominal − CPI YoY)",
        category = "Interest Rates",
        unit     = "Per cent"
      ) %>%
      filter(!is.na(value)) %>%
      select(date, value, series, series_id, category, unit, frequency)
  }

  # ---------------------------------------------------------------------------
  # 4g. Custom user-supplied ABS series IDs
  # ---------------------------------------------------------------------------
  # Users can enter comma-separated ABS series IDs in the sidebar to pull
  # any additional ABS time series they wish to explore.
  custom_series <- map_dfr(custom_ids, function(id) {
    id <- str_trim(id)
    if (!nzchar(id)) return(tibble())

    safe_read(
      read_abs(series_id = id),
      paste0("Custom series ", id)
    ) %>%
      normalize_abs(label = paste0("Custom: ", id), category = "Custom")
  })

  # ---------------------------------------------------------------------------
  # 4h. Combine all series into one tidy tibble
  # ---------------------------------------------------------------------------
  bind_rows(
    gdp_nominal,
    gdp_real,
    consumption_nominal,
    consumption_real,
    cpi,
    inflation_yoy,
    employment,
    unemployment_rate,
    participation_rate,
    wpi,
    wpi_growth,
    wages,
    nominal_rate,
    real_rate,
    custom_series
  ) %>%
    # De-duplicate: some tables may overlap in series x date
    distinct(date, series, .keep_all = TRUE) %>%
    arrange(category, series, date)
}


# =============================================================================
# SECTION 5 — TIME SERIES TRANSFORMATIONS
# =============================================================================
# Transforms applied to the plot values (the raw `value` column is preserved).
# These are standard time-series transforms used in empirical macro:
#
#   Levels              — raw data as published
#   Year-over-Year %    — 100 × (x_t / x_{t-k} − 1) where k = periods/year
#                         Makes non-stationary I(1) series approximately
#                         stationary; useful for comparing growth rates.
#   Period-over-Period % — one-period percentage change (month-on-month or
#                          quarter-on-quarter). Noisier but more timely.
#   Rebased Index       — 100 × x_t / x_1, so all series start at 100.
#                          Useful for comparing relative cumulative growth
#                          across series with different scales.

transform_series <- function(df, transform_mode) {
  if (nrow(df) == 0) return(df)

  out <- df %>%
    group_by(series) %>%
    arrange(date, .by_group = TRUE) %>%
    mutate(
      # Determine the appropriate lag for YoY calculations
      lag_meta = infer_lag(first(frequency)),
      lag_data = infer_lag_from_dates(date, fallback = lag_meta),
      lag_n    = coalesce(lag_data, lag_meta)
    )

  if (transform_mode == "levels") {
    out <- out %>% mutate(plot_value = value, plot_label = "Level")
  } else if (transform_mode == "yoy") {
    out <- out %>%
      mutate(plot_value = 100 * (value / lag(value, lag_n) - 1),
             plot_label = "YoY %")
  } else if (transform_mode == "period") {
    out <- out %>%
      mutate(plot_value = 100 * (value / lag(value, 1) - 1),
             plot_label = "Period-over-period %")
  } else if (transform_mode == "index") {
    out <- out %>%
      # first(value) is the earliest value within each series group
      mutate(plot_value = 100 * value / first(value),
             plot_label = "Index (start = 100)")
  }

  out %>%
    filter(!is.na(plot_value)) %>%
    ungroup()
}


# =============================================================================
# SECTION 6 — PLOTTING FUNCTION
# =============================================================================
# Builds a ggplot object that plotly later wraps for interactivity (hover,
# zoom, pan). Colour palettes from ColorBrewer are colour-blind friendly.

plot_macro <- function(df, chart_type, show_points = FALSE, dark_mode = FALSE) {
  # Adaptive theme colours for light/dark mode
  axis_col  <- if (isTRUE(dark_mode)) "#D7E0EA" else "#3B3F4A"
  strip_col <- if (isTRUE(dark_mode)) "#E8EEF6" else "#1E2C4A"
  panel_bg  <- if (isTRUE(dark_mode)) "#0F172A" else "#FFFFFF"
  grid_col  <- if (isTRUE(dark_mode)) "#334155" else "#D9DDE3"

  # Base aesthetic mapping: x = date, y = transformed value, colour = series

  p <- ggplot(df, aes(x = date, y = plot_value, colour = series, fill = series))

  # ---- Chart type geometry --------------------------------------------------
  if (chart_type == "line") {
    p <- p + geom_line(linewidth = 1.05, alpha = 0.9)
    if (show_points) {
      p <- p + geom_point(size = 1.6, alpha = 0.9)
    }
  }

  if (chart_type == "area") {
    p <- p +
      geom_area(alpha = 0.25, position = "identity") +
      geom_line(linewidth = 0.8)
  }

  if (chart_type == "bar") {
    p <- p + geom_col(position = "dodge", alpha = 0.85)
  }

  if (chart_type == "facet") {
    # Small multiples — one panel per series, free y-axes for different scales
    p <- p +
      geom_line(linewidth = 0.95, show.legend = FALSE) +
      facet_wrap(~series, scales = "free_y", ncol = 2)
  }

  if (chart_type == "seasonal") {
    # Seasonal overlay: each year as a separate coloured line plotted against
    # the month, so you can visually compare intra-year patterns across years.
    df <- df %>%
      mutate(
        year  = format(date, "%Y"),
        month = factor(format(date, "%b"), levels = month.abb)
      )

    p <- ggplot(df, aes(x = month, y = plot_value, group = year, colour = year)) +
      geom_line(linewidth = 0.9, alpha = 0.85) +
      geom_point(size = 1.2, alpha = 0.85) +
      facet_wrap(~series, scales = "free_y")

    return(
      p +
        scale_y_continuous(labels = label_number(big.mark = ",", accuracy = 0.1)) +
        labs(x = NULL, y = NULL, colour = NULL) +
        theme_minimal(base_size = 13) +
        theme(
          panel.background = element_rect(fill = panel_bg, colour = NA),
          plot.background  = element_rect(fill = panel_bg, colour = NA),
          panel.grid.major = element_line(colour = grid_col),
          panel.grid.minor = element_blank(),
          legend.position  = "bottom",
          legend.box       = "vertical",
          axis.text.x      = element_text(colour = axis_col),
          axis.text.y      = element_text(colour = axis_col),
          legend.text      = element_text(colour = axis_col),
          strip.text       = element_text(face = "bold", colour = strip_col)
        )
    )
  }

  # ---- Shared scales and theme for non-seasonal charts ----------------------
  p +
    scale_x_date(date_labels = "%Y", date_breaks = "4 years") +
    scale_y_continuous(labels = label_number(big.mark = ",", accuracy = 0.1)) +
    scale_colour_brewer(palette = "Dark2") +
    scale_fill_brewer(palette = "Set2") +
    labs(x = NULL, y = NULL, colour = NULL, fill = NULL) +
    theme_minimal(base_size = 13) +
    theme(
      panel.background = element_rect(fill = panel_bg, colour = NA),
      plot.background  = element_rect(fill = panel_bg, colour = NA),
      panel.grid.major = element_line(colour = grid_col),
      panel.grid.minor = element_blank(),
      legend.position  = "bottom",
      legend.box       = "vertical",
      axis.text.x      = element_text(colour = axis_col),
      axis.text.y      = element_text(colour = axis_col),
      legend.text      = element_text(colour = axis_col),
      strip.text       = element_text(face = "bold", colour = strip_col)
    )
}


#' Choose sensible default series to display on first load.
#' Prefer rate/growth series since they share a common scale (per cent)
#' and are meaningful in levels. Showing GDP levels alongside the
#' unemployment rate on one axis would be meaningless.
default_rate_series <- function(series_names) {
  preferred <- series_names[str_detect(
    series_names,
    regex("rate|inflation|yoy|growth|wpi growth", ignore_case = TRUE)
  )]

  if (length(preferred) > 0) return(preferred)

  # Fallback: just show up to 8 series
  series_names[seq_len(min(8, length(series_names)))]
}


# =============================================================================
# SECTION 7 — USER INTERFACE (UI)
# =============================================================================

ui <- page_navbar(
  title = "Australia Macro Dashboard",
  theme = bs_theme(
    version   = 5,
    bootswatch = "flatly",
    base_font    = font_google("Source Sans 3"),
    heading_font = font_google("Poppins"),
    primary   = "#0E5A8A",
    secondary = "#1F9D8C"
  ),

  # --- Inline CSS for dark/light mode styling --------------------------------
  # The key issue: bslib/Bootstrap does not natively toggle ALL element
  # colours when we flip a custom `data-theme` attribute. We must manually
  # target every element class that contains text or backgrounds.
  header = tags$head(
    tags$style(HTML("
      /* ================================================================
         LIGHT MODE (default) — CSS custom properties
         ================================================================ */
      :root {
        --app-bg:     #f4f7fb;
        --app-text:   #1f2d3d;
        --app-panel:  #ffffff;
        --app-border: #d9e0e8;
        --app-muted:  #5a6d82;
      }

      /* ================================================================
         DARK MODE — override custom properties
         ================================================================ */
      html[data-theme='dark'] {
        --app-bg:     #0b1220;
        --app-text:   #e3ebf4;
        --app-panel:  #111b2e;
        --app-border: #2a3a54;
        --app-muted:  #8fa3be;
      }

      /* ================================================================
         GLOBAL ELEMENT STYLES (reference the CSS variables above)
         ================================================================ */
      body {
        background-color: var(--app-bg);
        color: var(--app-text);
      }
      .bslib-page-fill {
        background-color: var(--app-bg);
      }

      /* Gradient banner in the sidebar */
      .top-note {
        background: linear-gradient(135deg, #0E5A8A 0%, #1F9D8C 100%);
        color: #ffffff;
        border-radius: 14px;
        padding: 14px 18px;
        margin-bottom: 14px;
        font-size: 0.9rem;
      }

      /* ---------- Cards, sidebar, form controls ---------- */
      .card,
      .sidebar,
      .form-control,
      .selectize-input,
      .selectize-dropdown {
        background-color: var(--app-panel) !important;
        color: var(--app-text) !important;
        border-color: var(--app-border) !important;
      }

      /* Card headers (e.g. 'Australian Macroeconomic Time Series') */
      .card-header {
        background-color: var(--app-panel) !important;
        color: var(--app-text) !important;
        border-bottom-color: var(--app-border) !important;
        font-weight: 600;
      }

      /* ---------- Sidebar label and control text ---------- */
      /* This is the main fix for the dark-mode readability issue:
         all label text, radio/checkbox labels, and helper text must
         inherit the theme text colour. */
      .sidebar label,
      .sidebar .control-label,
      .sidebar .radio label,
      .sidebar .checkbox label,
      .sidebar .form-check-label,
      .sidebar .shiny-input-container label,
      .sidebar .selectize-input,
      .sidebar .selectize-dropdown,
      .sidebar .selectize-dropdown-content .option,
      .sidebar h1, .sidebar h2, .sidebar h3,
      .sidebar h4, .sidebar h5, .sidebar h6,
      .sidebar p, .sidebar span {
        color: var(--app-text) !important;
      }

      /* Selectize dropdown items when highlighted */
      .selectize-dropdown-content .option.active {
        background-color: var(--app-border) !important;
        color: var(--app-text) !important;
      }

      /* Selectize selected items (tags in the input) */
      .selectize-input .item {
        background-color: var(--app-border) !important;
        color: var(--app-text) !important;
        border-color: var(--app-muted) !important;
      }

      /* Date range input fields */
      .input-daterange .form-control {
        background-color: var(--app-panel) !important;
        color: var(--app-text) !important;
        border-color: var(--app-border) !important;
      }
      .input-daterange .input-group-addon {
        background-color: var(--app-panel) !important;
        color: var(--app-muted) !important;
        border-color: var(--app-border) !important;
      }

      /* ---------- Navbar ---------- */
      .navbar {
        border-bottom: 1px solid var(--app-border);
      }
      .navbar .nav-link {
        color: var(--app-text) !important;
      }
      .navbar .nav-link.active {
        font-weight: 700;
      }

      /* ---------- Value boxes on the Snapshot tab ---------- */
      .bslib-value-box .value-box-title {
        opacity: 0.9;
      }

      /* ---------- Validation error styling ---------- */
      .shiny-output-error-validation {
        color: #e06c75;
        font-weight: 600;
        padding: 1rem;
      }

      /* ---------- Data table on the Table tab ---------- */
      .dataTable,
      .dataTables_wrapper,
      .dataTables_wrapper .dataTables_length,
      .dataTables_wrapper .dataTables_filter,
      .dataTables_wrapper .dataTables_info,
      .dataTables_wrapper .dataTables_paginate {
        color: var(--app-text) !important;
      }
      table.dataTable thead th {
        color: var(--app-text) !important;
        border-bottom-color: var(--app-border) !important;
      }
      table.dataTable tbody td {
        color: var(--app-text) !important;
        border-bottom-color: var(--app-border) !important;
      }
      table.dataTable tbody tr {
        background-color: var(--app-panel) !important;
      }
      table.dataTable tbody tr:hover {
        background-color: var(--app-border) !important;
      }
    ")),

    # --- JavaScript for dark/light mode toggle --------------------------------
    # On load: check localStorage (or system preference) and set the theme.
    # On click: toggle theme attribute and notify Shiny via setInputValue().
    tags$script(HTML("
      (function() {
        function setTheme(theme) {
          document.documentElement.setAttribute('data-theme', theme);
          // Send the theme to the Shiny server so plot backgrounds can adapt
          if (typeof Shiny !== 'undefined' && Shiny.setInputValue) {
            Shiny.setInputValue('theme_mode', theme, { priority: 'event' });
          }
          var btn = document.getElementById('theme_toggle');
          if (btn) {
            btn.textContent = theme === 'dark' ? '☀ Light mode' : '☾ Dark mode';
          }
        }

        function initTheme() {
          var saved = window.localStorage.getItem('macro_theme');
          var prefersDark = window.matchMedia &&
                            window.matchMedia('(prefers-color-scheme: dark)').matches;
          var initial = saved || (prefersDark ? 'dark' : 'light');
          setTheme(initial);
        }

        // Wait for DOM + Shiny to be ready
        document.addEventListener('DOMContentLoaded', function() {
          // Retry after a short delay to ensure Shiny is initialised
          setTimeout(initTheme, 100);
          document.addEventListener('click', function(evt) {
            var toggle = evt.target && evt.target.closest
                         ? evt.target.closest('#theme_toggle')
                         : null;
            if (toggle) {
              var current = document.documentElement.getAttribute('data-theme') || 'light';
              var next = current === 'dark' ? 'light' : 'dark';
              window.localStorage.setItem('macro_theme', next);
              setTheme(next);
            }
          });
        });
      })();
    "))
  ),

  # Spacer pushes the toggle button to the right of the navbar
  nav_spacer(),
  nav_item(
    actionButton("theme_toggle", "☾ Dark mode", class = "btn-outline-light btn-sm")
  ),

  # ---------------------------------------------------------------------------
  # TAB 1: Dashboard (interactive chart)
  # ---------------------------------------------------------------------------
  nav_panel(
    "Dashboard",
    layout_sidebar(
      sidebar = sidebar(
        width = 360,
        tags$div(
          class = "top-note",
          tags$strong("Data source:"),
          tags$span(" ABS via {readabs}. Press Refresh to re-download.")
        ),

        # Refresh button triggers a fresh pull from the ABS website
        actionButton("refresh", "Refresh Data", class = "btn-primary w-100"),
        tags$hr(),

        # Optional: user can supply a specific ABS series ID for the
        # nominal interest rate (e.g. from ABS.Stat or a known series_id)
        textInput(
          "nominal_rate_id",
          "Nominal Interest Rate Series ID (optional)",
          placeholder = "e.g., A2314867V"
        ),

        # Custom ABS series IDs — any valid ABS time series identifier
        textAreaInput(
          "custom_ids",
          "Custom ABS Series IDs (comma-separated)",
          placeholder = "A84423086A, A2302476C",
          rows = 2
        ),
        tags$hr(),

        # Filter by macro category
        checkboxGroupInput("categories", "Categories", choices = NULL),

        # Select individual series to display
        selectizeInput("series", "Series", choices = NULL, multiple = TRUE),

        # Date range slider
        dateRangeInput(
          "date_range",
          "Date Range",
          start     = as.Date("1990-01-01"),
          end       = Sys.Date(),
          min       = as.Date("1970-01-01"),
          separator = "to"
        ),
        tags$hr(),

        # Transform: choose how to represent the raw levels
        radioButtons(
          "transform_mode",
          "Transform",
          choices = c(
            "Levels"               = "levels",
            "Year-over-Year %"     = "yoy",
            "Period-over-Period %"  = "period",
            "Rebased Index"        = "index"
          ),
          selected = "levels"
        ),

        # Chart geometry
        radioButtons(
          "chart_type",
          "Chart Type",
          choices = c(
            "Line"             = "line",
            "Area"             = "area",
            "Bar"              = "bar",
            "Small Multiples"  = "facet",
            "Seasonality"      = "seasonal"
          ),
          selected = "line"
        ),

        checkboxInput("show_points", "Show data points", value = FALSE)
      ),

      # Main panel: the interactive Plotly chart
      card(
        card_header("Australian Macroeconomic Time Series"),
        card_body(
          plotlyOutput("macro_plot", height = "640px")
        )
      )
    )
  ),

  # ---------------------------------------------------------------------------
  # TAB 2: Latest Snapshot (value boxes showing most recent readings)
  # ---------------------------------------------------------------------------
  nav_panel(
    "Latest Snapshot",
    tags$div(
      class = "container-fluid mt-3",
      tags$p(
        class = "text-muted mb-3",
        "Most recent observation for each headline indicator. Values reflect",
        "ABS published levels (GDP in $millions, rates in per cent)."
      ),
      layout_column_wrap(
        width = 1 / 3,
        value_box(
          title = "Nominal GDP ($m)",
          value = textOutput("vb_gdp_nom"),
          showcase = bsicons::bs_icon("graph-up-arrow"),
          theme = value_box_theme(bg = "#0E5A8A", fg = "#fff")
        ),
        value_box(
          title = "Real GDP ($m)",
          value = textOutput("vb_gdp_real"),
          showcase = bsicons::bs_icon("bar-chart-line"),
          theme = value_box_theme(bg = "#1F9D8C", fg = "#fff")
        ),
        value_box(
          title = "CPI Inflation (YoY)",
          value = textOutput("vb_inflation"),
          showcase = bsicons::bs_icon("percent"),
          theme = value_box_theme(bg = "#3B4C7A", fg = "#fff")
        ),
        value_box(
          title = "Unemployment Rate",
          value = textOutput("vb_ur"),
          showcase = bsicons::bs_icon("people"),
          theme = value_box_theme(bg = "#326273", fg = "#fff")
        ),
        value_box(
          title = "WPI Growth (YoY)",
          value = textOutput("vb_wpi_growth"),
          showcase = bsicons::bs_icon("cash-stack"),
          theme = value_box_theme(bg = "#5B4A7A", fg = "#fff")
        ),
        value_box(
          title = "Participation Rate",
          value = textOutput("vb_participation"),
          showcase = bsicons::bs_icon("person-check"),
          theme = value_box_theme(bg = "#2A6F97", fg = "#fff")
        )
      )
    )
  ),

  # ---------------------------------------------------------------------------
  # TAB 3: Data Table (underlying data for inspection / export)
  # ---------------------------------------------------------------------------
  nav_panel(
    "Data Table",
    card(
      card_header("Filtered Data"),
      card_body(
        DT::dataTableOutput("data_table")
      )
    )
  )
)


# =============================================================================
# SECTION 8 — SERVER LOGIC
# =============================================================================

server <- function(input, output, session) {

  # Reactive value storing the master dataset (all series stacked)
  macro_data <- reactiveVal(tibble())

  # Reactive: current theme (used to adapt plot backgrounds)
  theme_mode <- reactive({
    mode <- input$theme_mode %||% "light"
    if (identical(mode, "dark")) "dark" else "light"
  })

  # --- Data loading function -------------------------------------------------
  load_data <- function() {
    # Parse the comma-separated custom IDs from the text input
    custom_ids <- str_split(input$custom_ids %||% "", pattern = ",")[[1]]
    custom_ids <- custom_ids[nzchar(str_trim(custom_ids))]

    # Show a progress bar while downloading from the ABS
    withProgress(message = "Loading ABS macro data…", value = 0, {
      incProgress(0.2, detail = "Fetching time series from ABS")
      dat <- get_macro_data(
        custom_ids      = custom_ids,
        nominal_rate_id = str_trim(input$nominal_rate_id %||% "")
      )
      incProgress(0.8, detail = "Finalising")
      macro_data(dat)
    })
  }

  # Load data once on app startup
  observeEvent(TRUE, { load_data() }, once = TRUE)

  # Re-load data when the user presses Refresh
  observeEvent(input$refresh, { load_data() })

  # --- Update sidebar controls when data changes ----------------------------
  observeEvent(macro_data(), {
    dat <- macro_data()
    req(nrow(dat) > 0)

    # Populate the category checkboxes
    cats <- sort(unique(dat$category))
    updateCheckboxGroupInput(session, "categories",
                             choices = cats, selected = cats)

    # Populate the series selectize input
    srs <- dat %>% distinct(series) %>% arrange(series) %>% pull(series)
    default_selected <- default_rate_series(srs)
    updateSelectizeInput(session, "series",
                         choices = srs, selected = default_selected,
                         server = TRUE)

    # Set sensible date range (default start from 2000 to reduce clutter)
    updateDateRangeInput(
      session, "date_range",
      min   = min(dat$date, na.rm = TRUE),
      max   = max(dat$date, na.rm = TRUE),
      start = max(min(dat$date, na.rm = TRUE), as.Date("2000-01-01")),
      end   = max(dat$date, na.rm = TRUE)
    )
  })

  # --- Reactive: filtered data (category + series + date range) ---------------
  filtered <- reactive({
    dat <- macro_data()
    req(nrow(dat) > 0)
    req(length(input$categories) > 0)
    req(length(input$series) > 0)

    dat %>%
      filter(
        category %in% input$categories,
        series   %in% input$series,
        date >= input$date_range[1],
        date <= input$date_range[2]
      )
  })

  # --- Reactive: transformed data (apply the selected transform) -------------
  transformed <- reactive({
    transform_series(filtered(), input$transform_mode)
  })

  # --- Main chart output (ggplot → plotly) -----------------------------------
  output$macro_plot <- renderPlotly({
    dat <- transformed()
    validate(need(nrow(dat) > 0,
                  "No data available for your current filters. Try adjusting the date range or selecting different series."))

    is_dark <- identical(theme_mode(), "dark")

    p <- plot_macro(
      dat,
      chart_type  = input$chart_type,
      show_points = isTRUE(input$show_points),
      dark_mode   = is_dark
    )

    # Convert ggplot to plotly and set layout for dark/light mode
    ggplotly(p, tooltip = c("x", "y", "colour")) %>%
      layout(
        paper_bgcolor = if (is_dark) "#0F172A" else "#FFFFFF",
        plot_bgcolor  = if (is_dark) "#0F172A" else "#FFFFFF",
        font = list(color = if (is_dark) "#D7E0EA" else "#3B3F4A"),
        legend = list(
          orientation = "h",
          y = -0.2,
          font = list(color = if (is_dark) "#D7E0EA" else "#3B3F4A")
        ),
        xaxis = list(
          title      = "",
          tickformat = "%Y",
          nticks     = 7,
          tickangle  = 0,
          gridcolor  = if (is_dark) "#334155" else "#D9DDE3",
          tickfont   = list(color = if (is_dark) "#D7E0EA" else "#3B3F4A"),
          automargin = TRUE
        ),
        yaxis = list(
          gridcolor = if (is_dark) "#334155" else "#D9DDE3",
          tickfont  = list(color = if (is_dark) "#D7E0EA" else "#3B3F4A")
        )
      )
  })

  # --- Data table output (Tab 3) ---------------------------------------------
  output$data_table <- DT::renderDataTable({
    dat <- transformed()
    req(nrow(dat) > 0)

    dat %>%
      select(Date = date, Series = series, Value = value,
             Transformed = plot_value, Category = category,
             Unit = unit, Frequency = frequency) %>%
      DT::datatable(
        options = list(pageLength = 20, scrollX = TRUE),
        rownames = FALSE
      )
  })

  # --- Helper: extract the latest value for a given series name ---------------
  latest_value <- function(series_name, percent = FALSE, prefix = "", suffix = "") {
    dat <- macro_data() %>%
      filter(series == series_name) %>%
      arrange(date)

    if (nrow(dat) == 0) return("N/A")

    val <- tail(dat$value, 1)

    if (isTRUE(percent)) {
      return(paste0(number(val, accuracy = 0.01), "%"))
    }

    paste0(prefix, comma(round(val, 2)), suffix)
  }

  # --- Value box text outputs (Tab 2) ----------------------------------------
  output$vb_gdp_nom     <- renderText({ latest_value("Nominal GDP (current prices, $m)", prefix = "$") })
  output$vb_gdp_real    <- renderText({ latest_value("Real GDP (chain volume, $m)", prefix = "$") })
  output$vb_inflation   <- renderText({ latest_value("CPI Inflation (YoY %)", percent = TRUE) })
  output$vb_ur          <- renderText({ latest_value("Unemployment Rate", percent = TRUE) })
  output$vb_wpi_growth  <- renderText({ latest_value("WPI Growth (YoY %)", percent = TRUE) })
  output$vb_participation <- renderText({ latest_value("Participation Rate", percent = TRUE) })
}


# =============================================================================
# LAUNCH THE APP
# =============================================================================
shinyApp(ui, server)

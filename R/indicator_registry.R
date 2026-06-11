# ==============================================================================
# Derived affordability indicator registry
# ==============================================================================
# Source of truth for derived indicator metadata used by the pipeline, app labels
# and tests. These rows document existing formulas; they do not convert stylised
# proxy measures into official ABS or lender assessment measures.
# ==============================================================================

INDICATOR_SOURCE_RPPI <- "RPPI"
INDICATOR_SOURCE_WPI <- "WPI"
INDICATOR_SOURCE_CPI_ALL_GROUPS <- "CPI All Groups"
INDICATOR_SOURCE_CPI_RENTS_NATIONAL <- "CPI Rents ; Weighted average of eight capital cities ;"
INDICATOR_SOURCE_CPI_INFLATION_YOY <- "CPI Inflation YoY"
INDICATOR_SOURCE_AWE <- "AWE (AWOTE, Persons)"
INDICATOR_SOURCE_RBA_MORTGAGE_RATE <- "Lending rates; Housing loans; Banks; Variable; Discounted; Owner-occupier"
INDICATOR_SOURCE_RBA_NEW_LOAN_RATE <- "Lending rates; Housing credit; New loans funded in the month; Owner-occupied; All loans; All institutions"

# Fixed estimation window for the F5-to-F6 splice wedge. Fixed (not growing)
# so the spliced history does not mutate as new observations arrive; changing
# the window requires a methodology version bump.
RBA_NEW_LOAN_SPLICE_WINDOW_START <- as.Date("2019-07-01")
RBA_NEW_LOAN_SPLICE_WINDOW_END <- as.Date("2021-06-30")

# Effective new-loan owner-occupier mortgage rate.
# RBA F6 (rates on new loans actually funded) starts 2019-07; the advertised
# F5 discounted variable rate has sat well above rates actually paid since the
# mid-2010s. For history before F6 exists, the F5 discounted series is level-
# adjusted down by the mean F5-F6 wedge over the fixed overlap window above,
# then spliced onto F6. Returns a monthly date/value frame ordered by date,
# with the estimated wedge attached as attribute "splice_wedge".
rba_new_loan_rate_spliced <- function(rba_rates) {
  required_columns <- c("date", "value", "series")
  missing_columns <- setdiff(required_columns, names(rba_rates))
  if (length(missing_columns) > 0) {
    stop("rba_rates is missing columns: ",
         paste(missing_columns, collapse = ", "), call. = FALSE)
  }

  extract_series <- function(series_name) {
    rows <- rba_rates[rba_rates$series == series_name,
                      c("date", "value"), drop = FALSE]
    rows$date <- as.Date(rows$date)
    rows <- rows[!is.na(rows$date) & !is.na(rows$value), , drop = FALSE]
    if (nrow(rows) == 0) {
      stop("rba_rates is missing required series '", series_name,
           "' for the spliced new-loan rate.", call. = FALSE)
    }
    rows[order(rows$date), , drop = FALSE]
  }

  f6 <- extract_series(INDICATOR_SOURCE_RBA_NEW_LOAN_RATE)
  f5 <- extract_series(INDICATOR_SOURCE_RBA_MORTGAGE_RATE)

  in_window <- function(d) {
    d >= RBA_NEW_LOAN_SPLICE_WINDOW_START & d <= RBA_NEW_LOAN_SPLICE_WINDOW_END
  }
  f5_window <- f5[in_window(f5$date), , drop = FALSE]
  f6_window <- f6[in_window(f6$date), , drop = FALSE]
  f5_window$month <- format(f5_window$date, "%Y-%m")
  f6_window$month <- format(f6_window$date, "%Y-%m")
  overlap <- merge(f5_window, f6_window, by = "month",
                   suffixes = c("_f5", "_f6"))
  if (nrow(overlap) < 12) {
    stop("Insufficient F5/F6 overlap (", nrow(overlap),
         " months) in the fixed splice window to estimate the wedge.",
         call. = FALSE)
  }
  wedge <- mean(overlap$value_f5 - overlap$value_f6)

  f6_start <- min(f6$date)
  pre <- f5[f5$date < f6_start, , drop = FALSE]
  pre$value <- pre$value - wedge

  spliced <- rbind(pre[, c("date", "value")], f6[, c("date", "value")])
  spliced <- spliced[order(spliced$date), , drop = FALSE]
  rownames(spliced) <- NULL
  attr(spliced, "splice_wedge") <- wedge
  spliced
}

indicator_source_series_constants <- function() {
  c(
    rppi = INDICATOR_SOURCE_RPPI,
    wpi = INDICATOR_SOURCE_WPI,
    cpi_all_groups = INDICATOR_SOURCE_CPI_ALL_GROUPS,
    cpi_rents_national = INDICATOR_SOURCE_CPI_RENTS_NATIONAL,
    cpi_inflation_yoy = INDICATOR_SOURCE_CPI_INFLATION_YOY,
    awe = INDICATOR_SOURCE_AWE,
    rba_mortgage_rate = INDICATOR_SOURCE_RBA_MORTGAGE_RATE,
    rba_new_loan_rate = INDICATOR_SOURCE_RBA_NEW_LOAN_RATE
  )
}

join_sources <- function(...) {
  paste(c(...), collapse = " | ")
}

indicator_registry <- function() {
  data.frame(
    indicator = c(
      "Price-to-Income Ratio",
      "Mortgage Serviceability Index",
      "Rental Affordability Index",
      "Deposit Gap (Years)",
      "Real House Price Growth YoY",
      "Real Wage Growth YoY",
      "Real Mortgage Rate",
      "National Housing Affordability Score",
      "Mortgage Serviceability Component Score",
      "Rental Entry Component Score",
      "Deposit Barrier Component Score"
    ),
    chart_label = c(
      "Price-to-Income Cost Pressure",
      "Modelled Mortgage Cost Pressure",
      "Rent Cost Pressure",
      "Stylised Deposit Gap (Years)",
      "Real House Price Growth YoY",
      "Real Wage Growth YoY",
      "Real Mortgage Rate",
      "National Housing Affordability Score",
      "Mortgage Serviceability Component",
      "Rental Entry Component",
      "Deposit Barrier Component"
    ),
    unit = c(
      "Index (base=100)",
      "Index (base=100)",
      "Index (base=100)",
      "Years",
      "Per cent",
      "Per cent",
      "Per cent",
      rep("Score (0-100)", 4)
    ),
    geography = rep("National", 11),
    frequency = rep("Quarter", 11),
    concept_group = c(
      "cost_pressure",
      "cost_pressure",
      "cost_pressure",
      "market_entry_scenario",
      "real_growth",
      "real_growth",
      "interest_rate_context",
      rep("market_entry_composite", 4)
    ),
    interpretation_direction = c(
      "higher_less_affordable",
      "higher_less_affordable",
      "higher_less_affordable",
      "higher_less_affordable",
      "higher_less_affordable",
      "higher_more_affordable",
      "higher_less_affordable",
      rep("higher_more_affordable", 4)
    ),
    formula = c(
      "Indexed RPPI divided by indexed WPI, multiplied by 100.",
      "Indexed 30-year annuity principal-and-interest repayments on an 80 per cent LVR loan against the ABS 6432.0 national mean dwelling price, at the RBA F6 new-loan owner-occupier rate (spliced onto level-adjusted F5 history before July 2019), divided by WPI as the income-growth proxy, then indexed to 100.",
      "Indexed eight-capital-city CPI rents (ABS weighted average of the eight capital cities; rest-of-state areas are not covered) divided by indexed WPI, multiplied by 100.",
      "Twenty per cent of the ABS 6432.0 national mean dwelling price divided by annual savings assumed at 15 per cent of gross income, with income proxied by AWE individual earnings.",
      "Year-ended percentage change in RPPI deflated by CPI All Groups.",
      "Year-ended percentage change in WPI deflated by CPI All Groups.",
      "RBA owner-occupier discounted variable mortgage rate minus CPI Inflation YoY.",
      "Weighted market-entry composite score on a 0-100 historical percentile scale: 40 per cent mortgage serviceability component, 35 per cent rental entry component and 25 per cent deposit barrier component. Higher = more affordable. Not an official ABS/NHHA statistic or lender assessment.",
      "Winsorised historical percentile score for the Mortgage Serviceability Index burden input. Higher = more affordable. Used at 40 per cent weight in the National Housing Affordability Score. Not an official ABS/NHHA statistic or lender assessment.",
      "Winsorised historical percentile score for the Rental Affordability Index burden input. Higher = more affordable. Used at 35 per cent weight in the National Housing Affordability Score. Not an official ABS/NHHA statistic or lender assessment.",
      "Winsorised historical percentile score for the Deposit Gap (Years) burden input. Higher = more affordable. Used at 25 per cent weight in the National Housing Affordability Score. Not an official ABS/NHHA statistic or lender assessment."
    ),
    source_files = c(
      "abs_timeseries.csv",
      "abs_timeseries.csv | rba_rates.csv",
      "abs_timeseries.csv",
      "abs_timeseries.csv",
      "abs_timeseries.csv",
      "abs_timeseries.csv",
      "abs_timeseries.csv | rba_rates.csv",
      rep("affordability_indices.csv", 4)
    ),
    source_series = c(
      join_sources(INDICATOR_SOURCE_RPPI, INDICATOR_SOURCE_WPI),
      join_sources(INDICATOR_SOURCE_RPPI, INDICATOR_SOURCE_WPI,
                   INDICATOR_SOURCE_RBA_NEW_LOAN_RATE,
                   INDICATOR_SOURCE_RBA_MORTGAGE_RATE),
      join_sources(INDICATOR_SOURCE_CPI_RENTS_NATIONAL,
                   INDICATOR_SOURCE_WPI),
      join_sources(INDICATOR_SOURCE_RPPI, INDICATOR_SOURCE_AWE),
      join_sources(INDICATOR_SOURCE_RPPI,
                   INDICATOR_SOURCE_CPI_ALL_GROUPS),
      join_sources(INDICATOR_SOURCE_WPI,
                   INDICATOR_SOURCE_CPI_ALL_GROUPS),
      join_sources(INDICATOR_SOURCE_RBA_MORTGAGE_RATE,
                   INDICATOR_SOURCE_CPI_INFLATION_YOY),
      join_sources("Mortgage Serviceability Index",
                   "Rental Affordability Index",
                   "Deposit Gap (Years)"),
      "Mortgage Serviceability Index",
      "Rental Affordability Index",
      "Deposit Gap (Years)"
    ),
    official_measure = rep(FALSE, 11),
    stylised_scenario = c(FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE,
                          TRUE, TRUE, TRUE, TRUE),
    measure_class = c(
      "derived_index",
      "derived_index",
      "derived_index",
      "stylised_scenario",
      "context_series",
      "context_series",
      "context_series",
      rep("stylised_scenario", 4)
    ),
    methodology_version = c(
      "affordability_indices_v1",
      "affordability_indices_v2",
      rep("affordability_indices_v1", 5),
      rep("national_affordability_score_v2", 4)
    ),
    primary_source = c(
      "ABS 6432.0 mean dwelling prices and ABS WPI",
      "ABS 6432.0 mean dwelling prices, ABS WPI and RBA F6 new-loan owner-occupier rates (F5-spliced history)",
      "ABS CPI rents and ABS WPI",
      "ABS mean dwelling prices and AWE with fixed deposit and savings assumptions",
      "ABS mean dwelling prices and CPI All Groups",
      "ABS WPI and CPI All Groups",
      "RBA owner-occupier mortgage rates and ABS CPI inflation",
      "Dashboard affordability_indices.csv component scores",
      "Dashboard Mortgage Serviceability Index component",
      "Dashboard Rental Affordability Index component",
      "Dashboard Deposit Gap component"
    ),
    quality_note = c(
      "Derived dashboard index from public ABS time series; no SIH sampling-error interval applies.",
      "Derived dashboard index from public ABS/RBA time series using stylised 80 per cent LVR and 30-year-term loan assumptions; no SIH sampling-error interval applies.",
      "Derived dashboard index from public ABS CPI and wage series; no SIH sampling-error interval applies.",
      "Stylised scenario using fixed deposit and savings assumptions; not an official ABS or lender measure.",
      "Context series derived from public ABS price and CPI inputs.",
      "Context series derived from public ABS wage and CPI inputs.",
      "Context series derived from public RBA rates and ABS inflation inputs.",
      "Stylised composite score with sensitivity diagnostics; not an official ABS/NHHA statistic or lender assessment.",
      "Stylised component score; no SIH sampling-error interval applies.",
      "Stylised component score; CPI rents may understate new-lease stress.",
      "Stylised component score using fixed deposit assumptions."
    ),
    vintage_dataset = rep("affordability_indices", 11),
    public_caveat = c(
      "Cost-pressure index, higher = less affordable.",
      "Modelled principal-and-interest repayment-burden index (80 per cent LVR, 30-year term, actual new-loan rates; pre-2019 rates are level-adjusted F5 history), higher = less affordable.",
      "Rent cost-pressure index from eight-capital-city CPI rents (rest-of-state areas not covered), higher = less affordable.",
      "Stylised years-to-save estimate: 20 per cent deposit on the ABS 6432.0 national mean dwelling price, saving 15 per cent of gross income proxied by AWE individual earnings (not household income); not an official ABS measure or lender assessment.",
      "Context series, not a household burden measure.",
      "Context series, not a household distribution measure.",
      "Context series, not a lender assessment.",
      "Historical-relative market-entry score, not an official ABS/NHHA statistic or lender assessment.",
      "Historical-relative component score, not a standalone affordability threshold.",
      "Historical-relative component score; CPI rents may lag advertised rents.",
      "Historical-relative component score using fixed upfront deposit assumptions."
    ),
    minimum_rows = c(40L, 50L, 80L, 30L, 50L, 80L, 50L,
                     20L, 20L, 20L, 20L),
    stringsAsFactors = FALSE
  )
}

indicator_registry_required_abs_sources <- function() {
  unname(c(
    INDICATOR_SOURCE_RPPI,
    INDICATOR_SOURCE_WPI,
    INDICATOR_SOURCE_CPI_ALL_GROUPS,
    INDICATOR_SOURCE_CPI_INFLATION_YOY,
    INDICATOR_SOURCE_AWE,
    INDICATOR_SOURCE_CPI_RENTS_NATIONAL
  ))
}

indicator_registry_required_rba_sources <- function() {
  unname(c(
    INDICATOR_SOURCE_RBA_MORTGAGE_RATE,
    INDICATOR_SOURCE_RBA_NEW_LOAN_RATE
  ))
}

indicator_metadata <- function(indicator) {
  registry <- indicator_registry()
  matched <- registry[registry$indicator %in% indicator, , drop = FALSE]
  missing <- setdiff(indicator, matched$indicator)
  if (length(missing) > 0) {
    stop("Unknown indicator metadata: ", paste(missing, collapse = ", "),
         call. = FALSE)
  }
  matched[match(indicator, matched$indicator), , drop = FALSE]
}

indicator_chart_label <- function(indicator) {
  indicator_metadata(indicator)$chart_label
}

indicator_registry_minimum_rows <- function() {
  registry <- indicator_registry()
  stats::setNames(registry$minimum_rows, registry$indicator)
}

indicator_registry_output_metadata <- function(indicator) {
  metadata <- indicator_metadata(indicator)
  list(
    indicator = metadata$indicator[[1]],
    geography = metadata$geography[[1]],
    unit = metadata$unit[[1]],
    frequency = metadata$frequency[[1]]
  )
}

indicator_interpretation_label <- function(direction) {
  labels <- c(
    higher_less_affordable = "Higher = less affordable",
    higher_more_affordable = "Higher = more affordable"
  )
  missing <- setdiff(direction, names(labels))
  if (length(missing) > 0) {
    stop("Unknown interpretation direction: ",
         paste(missing, collapse = ", "),
         call. = FALSE)
  }
  unname(labels[direction])
}

indicator_registry_methodology_table <- function() {
  registry <- indicator_registry()
  data.frame(
    "Indicator" = registry$indicator,
    "Chart Label" = registry$chart_label,
    "Concept Group" = registry$concept_group,
    "Unit" = registry$unit,
    "Frequency" = registry$frequency,
    "Interpretation" = indicator_interpretation_label(
      registry$interpretation_direction
    ),
    "Formula" = registry$formula,
    "Source Files" = registry$source_files,
    "Source Series" = registry$source_series,
    "Official Measure" = ifelse(registry$official_measure, "Yes", "No"),
    "Stylised Scenario" = ifelse(registry$stylised_scenario, "Yes", "No"),
    "Measure Class" = registry$measure_class,
    "Methodology Version" = registry$methodology_version,
    "Primary Source" = registry$primary_source,
    "Quality Note" = registry$quality_note,
    "Vintage Dataset" = registry$vintage_dataset,
    "Public Caveat" = registry$public_caveat,
    "Minimum Rows" = registry$minimum_rows,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

market_entry_scenario_methodology_note <- function() {
  c(
    "Saved affordability_indices.csv rows remain proxy cost-pressure indexes documented by R/indicator_registry.R.",
    "R/market_entry_scenarios.R defines app-only market-entry scenarios for the Affordability calculator and serviceability sensitivity chart.",
    "Assessment buffer and expense inputs are sensitivity assumptions, not a lender assessment."
  )
}

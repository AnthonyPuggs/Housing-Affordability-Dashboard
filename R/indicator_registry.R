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

indicator_source_series_constants <- function() {
  c(
    rppi = INDICATOR_SOURCE_RPPI,
    wpi = INDICATOR_SOURCE_WPI,
    cpi_all_groups = INDICATOR_SOURCE_CPI_ALL_GROUPS,
    cpi_rents_national = INDICATOR_SOURCE_CPI_RENTS_NATIONAL,
    cpi_inflation_yoy = INDICATOR_SOURCE_CPI_INFLATION_YOY,
    awe = INDICATOR_SOURCE_AWE,
    rba_mortgage_rate = INDICATOR_SOURCE_RBA_MORTGAGE_RATE
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
      "Real Mortgage Rate"
    ),
    chart_label = c(
      "Price-to-Income Cost Pressure",
      "Modelled Mortgage Cost Pressure",
      "Rent Cost Pressure",
      "Stylised Deposit Gap (Years)",
      "Real House Price Growth YoY",
      "Real Wage Growth YoY",
      "Real Mortgage Rate"
    ),
    unit = c(
      "Index (base=100)",
      "Index (base=100)",
      "Index (base=100)",
      "Years",
      "Per cent",
      "Per cent",
      "Per cent"
    ),
    geography = rep("National", 7),
    frequency = rep("Quarter", 7),
    concept_group = c(
      "cost_pressure",
      "cost_pressure",
      "cost_pressure",
      "market_entry_scenario",
      "real_growth",
      "real_growth",
      "interest_rate_context"
    ),
    interpretation_direction = c(
      "higher_less_affordable",
      "higher_less_affordable",
      "higher_less_affordable",
      "higher_less_affordable",
      "higher_less_affordable",
      "higher_more_affordable",
      "higher_less_affordable"
    ),
    formula = c(
      "Indexed RPPI divided by indexed WPI, multiplied by 100.",
      "Indexed RPPI multiplied by the RBA owner-occupier discounted variable mortgage rate, divided by indexed WPI, then indexed to 100.",
      "Indexed national CPI rents divided by indexed WPI, multiplied by 100.",
      "Twenty per cent of an RPPI-scaled SIH-base dwelling price divided by annual AWE savings at a 15 per cent savings rate.",
      "Year-ended percentage change in RPPI deflated by CPI All Groups.",
      "Year-ended percentage change in WPI deflated by CPI All Groups.",
      "RBA owner-occupier discounted variable mortgage rate minus CPI Inflation YoY."
    ),
    source_files = c(
      "abs_timeseries.csv",
      "abs_timeseries.csv | rba_rates.csv",
      "abs_timeseries.csv",
      "abs_timeseries.csv",
      "abs_timeseries.csv",
      "abs_timeseries.csv",
      "abs_timeseries.csv | rba_rates.csv"
    ),
    source_series = c(
      join_sources(INDICATOR_SOURCE_RPPI, INDICATOR_SOURCE_WPI),
      join_sources(INDICATOR_SOURCE_RPPI, INDICATOR_SOURCE_WPI,
                   INDICATOR_SOURCE_RBA_MORTGAGE_RATE),
      join_sources(INDICATOR_SOURCE_CPI_RENTS_NATIONAL,
                   INDICATOR_SOURCE_WPI),
      join_sources(INDICATOR_SOURCE_RPPI, INDICATOR_SOURCE_AWE),
      join_sources(INDICATOR_SOURCE_RPPI,
                   INDICATOR_SOURCE_CPI_ALL_GROUPS),
      join_sources(INDICATOR_SOURCE_WPI,
                   INDICATOR_SOURCE_CPI_ALL_GROUPS),
      join_sources(INDICATOR_SOURCE_RBA_MORTGAGE_RATE,
                   INDICATOR_SOURCE_CPI_INFLATION_YOY)
    ),
    official_measure = rep(FALSE, 7),
    stylised_scenario = c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE),
    minimum_rows = c(40L, 50L, 80L, 30L, 50L, 80L, 50L),
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
  unname(INDICATOR_SOURCE_RBA_MORTGAGE_RATE)
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

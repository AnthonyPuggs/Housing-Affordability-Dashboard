# Feature-level source and interpretation metadata for user-facing surfaces.

feature_metadata_registry <- function() {
  data.frame(
    feature_id = c(
      "overview_national_mean_price",
      "overview_highest_capital_price",
      "overview_market_entry_score",
      "market_context_underutilisation",
      "housing_supply_selected_approvals",
      "housing_supply_largest_selected_jurisdiction",
      "housing_supply_construction_cost",
      "housing_supply_houses_share",
      "rental_market_weekly_rent",
      "rental_market_rent_to_income",
      "price_trends_rent_cpi",
      "methodology_source_audit"
    ),
    title = c(
      "National Mean Dwelling Price",
      "Highest Capital Median Price",
      "National Market-Entry Affordability Score",
      "Labour Underutilisation Rate",
      "Selected Approvals",
      "Largest Selected Jurisdiction",
      "Construction Costs",
      "Houses Share",
      "Weekly Rent",
      "Rent-to-Income Ratio",
      "Rent CPI",
      "Data Source Audit"
    ),
    measure_class = c(
      "context",
      "context",
      "stylised_scenario",
      "context",
      "context",
      "context",
      "context",
      "context",
      "official_survey",
      "official_survey",
      "context",
      "context"
    ),
    source_label = c(
      "ABS Total Value of Dwellings",
      "ABS residential property transfer medians",
      "Derived dashboard indicators from saved ABS/RBA/WPI inputs",
      "ABS Labour Force",
      "ABS Building Approvals",
      "ABS Building Approvals",
      "ABS CPI new dwelling purchase",
      "ABS Building Approvals",
      "ABS Survey of Income and Housing",
      "ABS Survey of Income and Housing",
      "ABS Consumer Price Index rents",
      "Official-source audit registry"
    ),
    economic_role = c(
      "Market price context for national dwelling-value levels.",
      "Capital-city price context, excluding the national average.",
      "Stylised market-entry pressure summary across mortgage, rent and deposit channels.",
      "Income-security and spare-capacity context for household affordability.",
      "Supply pipeline context for user-selected jurisdictions and approval filters.",
      "Cross-jurisdiction context within the selected approval filters.",
      "Construction-cost pressure context.",
      "Dwelling mix context within approvals.",
      "Observed renter housing-cost level by household characteristic.",
      "Observed renter housing-cost burden as a share of gross income.",
      "Rent price inflation context across CPI capital cities.",
      "Documents unresolved high-value source gaps before any new ingestion."
    ),
    caveat = c(
      "Mean dwelling price is not a median transaction price or affordability burden measure.",
      "A high capital median is a price signal, not an affordability outcome without an income or burden denominator.",
      "Not an official ABS/NHHA statistic, lender assessment or share of households who can afford housing.",
      "Context series only; it does not directly measure housing costs or household stress.",
      "Approval counts are pipeline indicators, not completed supply or adequacy measures.",
      "The largest selected jurisdiction depends on the user's state, building-type and sector filters.",
      "This is a price index for new dwelling purchase by owner occupiers, not household burden.",
      "The houses share is a composition indicator, not a measure of aggregate supply sufficiency.",
      "Nominal weekly dollars should be interpreted alongside the cost-to-income ratio option.",
      "Gross-income denominator and SIH sampling uncertainty should remain visible.",
      "CPI rents include new and existing tenancies and are not an advertised-rent series.",
      "Candidate status means the source needs acceptance before it becomes a dashboard data feed."
    ),
    stringsAsFactors = FALSE
  )
}

feature_metadata <- function(feature_id = NULL) {
  registry <- feature_metadata_registry()
  if (is.null(feature_id)) {
    return(registry)
  }

  out <- registry[registry$feature_id %in% feature_id, , drop = FALSE]
  missing <- setdiff(feature_id, out$feature_id)
  if (length(missing) > 0) {
    stop("Unknown feature metadata ID(s): ",
         paste(missing, collapse = ", "), call. = FALSE)
  }
  out
}

feature_source_note_text <- function(feature_id) {
  row <- feature_metadata(feature_id)
  paste0(
    row$source_label[1], ". ",
    row$economic_role[1], " ",
    row$caveat[1]
  )
}

feature_source_note <- function(feature_id) {
  text <- feature_source_note_text(feature_id)
  if (exists("policy_source_note", mode = "function", inherits = TRUE)) {
    return(policy_source_note(text))
  }
  if (exists("source_note", mode = "function", inherits = TRUE)) {
    return(source_note(text))
  }
  shiny::tags$p(text, class = "source-note policy-source-note")
}

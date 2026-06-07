# Candidate-source registry for unresolved affordability data gaps.

source_audit_status_levels <- function() {
  c("ready", "candidate", "not suitable yet")
}

source_audit_registry <- function() {
  data.frame(
    source_id = c(
      "abs_household_income_wealth",
      "abs_housing_occupancy_costs",
      "abs_cpi_slci_housing_faq",
      "abs_selected_living_cost_indexes",
      "nsw_rental_bond_lodgements",
      "qld_rta_median_rents"
    ),
    gap = c(
      "household_disposable_income",
      "household_disposable_income",
      "new_tenancy_rent",
      "residual_income_living_cost",
      "new_tenancy_rent",
      "new_tenancy_rent"
    ),
    source_name = c(
      "ABS Household income and wealth",
      "ABS Housing Occupancy and Costs / Survey of Income and Housing",
      "ABS CPI/SLCI housing measurement FAQ",
      "ABS Selected Living Cost Indexes",
      "NSW rental bond lodgement data",
      "Queensland RTA median rents"
    ),
    provider = c(
      "Australian Bureau of Statistics",
      "Australian Bureau of Statistics",
      "Australian Bureau of Statistics",
      "Australian Bureau of Statistics",
      "NSW Government",
      "Residential Tenancies Authority Queensland"
    ),
    status = c(
      "candidate",
      "candidate",
      "not suitable yet",
      "candidate",
      "candidate",
      "candidate"
    ),
    reason = c(
      "Provides a direct disposable-income concept, including median equivalised weekly disposable household income, but needs denominator alignment before use in price or rent ratios.",
      "Official SIH source for observed household income, tenure, housing costs and burden ratios; periodic survey coverage means it should anchor official burden measures rather than live market-entry updates.",
      "CPI rents include new and existing tenants and are index-style price measures, so they are not a direct advertised-rent or new-tenancy dollar series.",
      "Outlays-based living-cost indexes capture household-group price pressure including rents and mortgage interest, but are national indexes rather than household residual-income microdata.",
      "Quarterly bond lodgement files can proxy new-tenancy rents in NSW, but coverage is jurisdiction-specific and would need cleaning, dwelling matching and caveats.",
      "Quarterly median rents are based on new Queensland rental bonds and are useful for new-tenancy context, but coverage is jurisdiction-specific and not a national feed."
    ),
    coverage = c(
      "National income and wealth indicators; SIH and HILDA-based income series.",
      "Australia, states, tenure and household characteristics in SIH 2019-20 public data cubes.",
      "Capital-city CPI/SLCI housing measurement concepts; rents cover new and existing tenants.",
      "Quarterly national household-group living-cost indexes.",
      "NSW postcode-level rental bond lodgements and refunds, quarterly and annual files.",
      "Queensland postcode and dwelling-type median weekly rents, quarterly from new rental bonds."
    ),
    update_frequency = c(
      "Annual or periodic, depending on underlying series.",
      "Periodic SIH publication; 2023-24 outputs are not being released.",
      "Monthly and quarterly CPI/SLCI methodology context.",
      "Quarterly.",
      "Quarterly for lodgement/refund files; annual holdings files.",
      "Quarterly."
    ),
    dashboard_implication = c(
      "Audit as the preferred household disposable-income denominator before adding a true disposable-income affordability ratio.",
      "Keep as the official burden anchor and use for observed SIH ratios, not as a live market-entry series.",
      "Keep CPI rents as context; do not use it alone as advertised-rent or new-lease evidence.",
      "Candidate for a later living-cost or residual-income context layer, not a household residual-income calculator by itself.",
      "Candidate for a later NSW new-tenancy rent module after source cleaning and coverage warnings.",
      "Candidate for a later Queensland new-tenancy rent module after source cleaning and national comparability warnings."
    ),
    url = c(
      "https://www.abs.gov.au/statistics/measuring-what-matters/measuring-what-matters-themes-and-indicators/prosperous/household-income-and-wealth",
      "https://www.abs.gov.au/statistics/people/housing/housing-occupancy-and-costs/latest-release",
      "https://www.abs.gov.au/articles/frequently-asked-questions-faqs-about-measurement-housing-consumer-price-index-cpi-and-selected-living-cost-indexes-slcis",
      "https://www.abs.gov.au/methodologies/selected-living-cost-indexes-australia-methodology/mar-2026",
      "https://www.nsw.gov.au/housing-and-construction/rental-forms-surveys-and-data/rental-bond-data",
      "https://www.rta.qld.gov.au/forms-resources/rta-quarterly-data/median-rents-quick-finder"
    ),
    stringsAsFactors = FALSE
  )
}

source_audit_gap_label <- function(x) {
  labels <- c(
    household_disposable_income = "Household disposable income",
    new_tenancy_rent = "New-tenancy or advertised rents",
    residual_income_living_cost = "Residual income / living costs"
  )
  ifelse(x %in% names(labels), labels[x], x)
}

source_audit_methodology_table <- function(audit = source_audit_registry()) {
  data.frame(
    Gap = source_audit_gap_label(audit$gap),
    `Candidate Source` = audit$source_name,
    Status = audit$status,
    Coverage = audit$coverage,
    `Update Frequency` = audit$update_frequency,
    `Dashboard Implication` = audit$dashboard_implication,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

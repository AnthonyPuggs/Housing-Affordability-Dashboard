# Runs standalone via `Rscript tests/test_official_burden_summary.R` (repo root) and under
# testthat::test_dir("tests"); see tests/helper-contracts.R.
if (!exists("contracts_harness_loaded", mode = "function")) {
  source(file.path(if (basename(getwd()) == "tests") "." else "tests",
                   "helper-contracts.R"))
}

test_that("official_burden_summary contracts", {
  repo_root <- repo_root_path()


  helper_path <- file.path(repo_root, "R", "official_burden_summary.R")
  check(file.exists(helper_path), "R/official_burden_summary.R does not exist")

  if (file.exists(helper_path)) {
    suppressPackageStartupMessages({
      library(dplyr)
      library(scales)
    })
    source(file.path(repo_root, "R", "dashboard_formatting.R"), local = TRUE)
    source(helper_path, local = TRUE)

    check(exists("official_burden_summary", mode = "function"),
          "official_burden_summary() must be defined")

    nhha <- data.frame(
      survey_year = c("2017-18", "2019-20", "2019-20"),
      value = c(39.5, 42.0, 45.1),
      metric = "pct_rental_stress_over_30",
      tenure = "renter_lower_income",
      breakdown_var = "nhha_location",
      breakdown_val = c("Total", "Total", "Greater capital city area"),
      geography = "Aust.",
      stat_type = "proportion",
      stringsAsFactors = FALSE
    )

    stress <- data.frame(
      survey_year = "2019-20",
      value = c(25.1, 37.4, 58.0, 47.9),
      metric = "pct_over_30",
      tenure = c("all", "owner_mortgage", "renter_private",
                 "owner_mortgage"),
      breakdown_var = c("renter", "owner", "renter",
                        "equiv_income_quintile"),
      breakdown_val = c("Total", "Owner with a mortgage",
                        "Private landlord", "Lowest quintile"),
      geography = "National",
      stat_type = c("lower_income", "lower_income", "lower_income", "mean"),
      stringsAsFactors = FALSE
    )

    summary <- official_burden_summary(
      sih_nhha = nhha,
      sih_stress = stress,
      sih_cost_ratios = data.frame()
    )

    check(is.data.frame(summary), "official_burden_summary() must return a data frame")
    expected_ids <- c(
      "nhha_lower_income_renter_stress",
      "lower_income_over_30",
      "mortgage_owner_over_30",
      "highest_stress_group"
    )
    check(identical(summary$metric_id, expected_ids),
          "official_burden_summary() must return the four Overview burden metrics in display order")
    check(all(c("title", "value", "formatted_value", "subtitle",
                "source", "measure_class") %in% names(summary)),
          "official_burden_summary() missing expected display columns")
    check(summary$value[summary$metric_id == "nhha_lower_income_renter_stress"] == 42.0,
          "NHHA KPI must use latest Australia Total lower-income renter stress")
    check(summary$value[summary$metric_id == "lower_income_over_30"] == 25.1,
          "Lower-income KPI must use National lower-income all-households >30%")
    check(summary$value[summary$metric_id == "mortgage_owner_over_30"] == 37.4,
          "Mortgage owner KPI must use National lower-income owner-with-mortgage >30%")
    check(summary$value[summary$metric_id == "highest_stress_group"] == 47.9,
          "Highest-stress KPI must use the largest eligible household-group burden")
    check(all(summary$measure_class == "official_survey"),
          "Overview burden strip must be labelled as official_survey")
    check(any(grepl("official", summary$subtitle, ignore.case = TRUE)),
          "Overview burden strip subtitles must identify official survey burden evidence")

    empty_summary <- official_burden_summary(
      sih_nhha = data.frame(),
      sih_stress = data.frame(),
      sih_cost_ratios = data.frame()
    )
    check(identical(empty_summary$metric_id, expected_ids),
          "official_burden_summary() must return display rows even when saved SIH inputs are unavailable")
    check(all(empty_summary$formatted_value == "N/A"),
          "official_burden_summary() must format unavailable values as N/A")
  }
})
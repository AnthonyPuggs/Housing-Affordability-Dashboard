# Runs standalone via `Rscript tests/test_national_affordability_score.R` (repo root) and under
# testthat::test_dir("tests"); see tests/helper-contracts.R.
if (!exists("contracts_harness_loaded", mode = "function")) {
  source(file.path(if (basename(getwd()) == "tests") "." else "tests",
                   "helper-contracts.R"))
}

test_that("national_affordability_score contracts", {
  repo_root <- repo_root_path()


  helper_path <- file.path(repo_root, "R", "national_affordability_score.R")
  check(file.exists(helper_path), "R/national_affordability_score.R does not exist")

  if (file.exists(helper_path)) {
    parsed <- tryCatch({
      parse(helper_path)
      TRUE
    }, error = function(e) conditionMessage(e))
    check(identical(parsed, TRUE),
          paste(helper_path, "does not parse:", parsed))
    source(helper_path, local = TRUE)

    required_functions <- c(
      "national_affordability_score_weights",
      "national_affordability_score_inputs",
      "national_affordability_score_indicators",
      "calculate_national_affordability_score",
      "national_affordability_score_diagnostics"
    )
    missing_functions <- required_functions[
      !exists_in(required_functions, environment())
    ]
    check(length(missing_functions) == 0,
          paste("Missing national affordability score helpers:",
                paste(missing_functions, collapse = ", ")))

    if (all(exists_in(required_functions, environment()))) {
      weights <- national_affordability_score_weights()
      check(abs(sum(weights$weight) - 1) < 1e-10,
            "National affordability score weights must sum to 1")
      check(identical(weights$component, c(
        "mortgage_serviceability",
        "rental_entry",
        "deposit_barrier"
      )), "National affordability score components must use fixed v1 ordering")

      inputs <- national_affordability_score_inputs()
      forbidden_inputs <- c(
        "Price-to-Income Ratio",
        "Real Wage Growth YoY",
        "Real Mortgage Rate",
        "Unemployment Rate",
        "Underemployment Rate",
        "Building Approvals",
        "Net Overseas Migration"
      )
      check(length(intersect(inputs$input_indicator, forbidden_inputs)) == 0,
            "National affordability score must not include debated/context variables")

      indicators <- national_affordability_score_indicators()
      expected_indicators <- c(
        "National Housing Affordability Score",
        "Mortgage Serviceability Component Score",
        "Rental Entry Component Score",
        "Deposit Barrier Component Score"
      )
      check(identical(indicators$indicator, expected_indicators),
            "National affordability score output indicators changed unexpectedly")

      dates <- as.Date(c(
        "2012-07-01", "2012-10-01", "2013-01-01",
        "2013-04-01", "2013-07-01", "2013-10-01"
      ))
      complete_input <- rbind(
        data.frame(date = dates, value = c(80, 85, 90, 95, 100, 105),
                   indicator = "Mortgage Serviceability Index"),
        data.frame(date = dates, value = c(100, 98, 102, 104, 106, 108),
                   indicator = "Rental Affordability Index"),
        data.frame(date = dates, value = c(4, 4.2, 4.5, 4.8, 5.1, 5.4),
                   indicator = "Deposit Gap (Years)")
      )
      complete_input$geography <- "National"
      complete_input$unit <- "Input"
      complete_input$frequency <- "Quarter"

      score <- calculate_national_affordability_score(complete_input)
      check(all(c("date", "value", "indicator", "geography", "unit",
                  "frequency") %in% names(score)),
            "Score output must use the affordability_indices.csv schema")
      check(all(score$value >= 0 & score$value <= 100),
            "All score and component values must stay in the 0-100 range")
      check(all(expected_indicators %in% unique(score$indicator)),
            "Score output missing expected headline or component indicators")

      latest_date <- max(score$date)
      latest <- score[score$date == latest_date, ]
      latest_headline <- latest[
        latest$indicator == "National Housing Affordability Score", "value"
      ]
      latest_components <- merge(
        latest[latest$indicator != "National Housing Affordability Score",
               c("indicator", "value")],
        indicators[, c("indicator", "component")],
        by = "indicator"
      )
      latest_components <- merge(
        latest_components,
        weights[, c("component", "weight")],
        by = "component"
      )
      expected_headline <- sum(latest_components$value *
                                 latest_components$weight)
      check(abs(latest_headline - expected_headline) < 1e-8,
            "Headline score must equal the weighted component score")

      mortgage_scores <- score[
        score$indicator == "Mortgage Serviceability Component Score",
        c("date", "value")
      ]
      check(mortgage_scores$value[1] > mortgage_scores$value[nrow(mortgage_scores)],
            "Higher mortgage burden must lower the mortgage component score")

      incomplete_input <- complete_input[
        !(complete_input$date == as.Date("2013-04-01") &
            complete_input$indicator == "Deposit Gap (Years)"),
      ]
      incomplete_score <- calculate_national_affordability_score(incomplete_input)
      check(!as.Date("2013-04-01") %in% incomplete_score$date,
            "Dates missing a core component must be dropped")

      diagnostics <- national_affordability_score_diagnostics(complete_input)
      check(is.list(diagnostics),
            "national_affordability_score_diagnostics() must return a list")
      expected_diagnostic_names <- c(
        "sample_window",
        "component_correlations",
        "missingness",
        "latest_contributions",
        "sensitivity_scores",
        "interpretation_warning"
      )
      check(all(expected_diagnostic_names %in% names(diagnostics)),
            "Diagnostics must include sample, correlation, missingness, contribution and sensitivity outputs")

      check(all(c("start_date", "latest_date", "score_rows", "latest_score") %in%
                  names(diagnostics$sample_window)),
            "Diagnostics sample window must expose dates, row count and latest score")
      check(diagnostics$sample_window$score_rows == length(unique(score$date)),
            "Diagnostics score row count must match complete score dates")

      check(all(c("component_x", "component_y", "correlation") %in%
                  names(diagnostics$component_correlations)),
            "Diagnostics correlations must expose component pairs and correlation")
      check(nrow(diagnostics$component_correlations) == 3,
            "Diagnostics must report all three pairwise component correlations")

      check(all(c("component", "input_indicator", "available_rows",
                  "complete_score_rows") %in% names(diagnostics$missingness)),
            "Diagnostics missingness must expose input row counts and common rows")
      check(all(diagnostics$missingness$complete_score_rows ==
                  diagnostics$sample_window$score_rows),
            "Diagnostics missingness common-row count must match score rows")

      check(all(c("component", "component_label", "score", "weight",
                  "contribution_points") %in%
                  names(diagnostics$latest_contributions)),
            "Diagnostics latest contributions must expose score, weight and weighted points")
      check(abs(sum(diagnostics$latest_contributions$contribution_points) -
                  diagnostics$sample_window$latest_score) < 1e-8,
            "Latest contribution points must sum to the latest headline score")

      required_sensitivity <- c(
        "default_40_35_25",
        "equal_weights",
        "ownership_heavy",
        "rental_heavy",
        "leave_out_mortgage_serviceability",
        "leave_out_rental_entry",
        "leave_out_deposit_barrier",
        "geometric_default"
      )
      check(all(required_sensitivity %in%
                  diagnostics$sensitivity_scores$scenario),
            "Diagnostics must include default, alternative-weight, leave-one-out and geometric sensitivity scores")
      check(grepl("historical-relative", diagnostics$interpretation_warning,
                  fixed = TRUE),
            "Diagnostics warning must describe the score as historical-relative")

      # --- v2 frozen reference window ------------------------------------------
      # Appending observations dated after NATIONAL_AFFORDABILITY_SCORE_REFERENCE_END
      # must not change any previously published score (the v1 growing-sample
      # normalisation silently re-ranked all history on every refresh), and the
      # new observation must be scored against the frozen distribution.
      check(exists("NATIONAL_AFFORDABILITY_SCORE_REFERENCE_END"),
            "v2 must define a frozen reference window end constant")

      future_date <- as.Date("2026-04-01")
      check(future_date > NATIONAL_AFFORDABILITY_SCORE_REFERENCE_END,
            "Test future date must fall outside the frozen reference window")
      extreme_future <- rbind(
        data.frame(date = future_date, value = 500,
                   indicator = "Mortgage Serviceability Index"),
        data.frame(date = future_date, value = 500,
                   indicator = "Rental Affordability Index"),
        data.frame(date = future_date, value = 50,
                   indicator = "Deposit Gap (Years)")
      )
      extreme_future$geography <- "National"
      extreme_future$unit <- "Input"
      extreme_future$frequency <- "Quarter"
      extended_input <- rbind(complete_input, extreme_future)

      baseline_score <- calculate_national_affordability_score(complete_input)
      extended_score <- calculate_national_affordability_score(extended_input)
      common <- merge(
        baseline_score[, c("date", "indicator", "value")],
        extended_score[, c("date", "indicator", "value")],
        by = c("date", "indicator"),
        suffixes = c("_baseline", "_extended")
      )
      check(nrow(common) == nrow(baseline_score),
            "Extended sample must retain all baseline score rows")
      check(isTRUE(all.equal(common$value_baseline, common$value_extended)),
            "Scores inside the frozen reference window must not change when later observations arrive")

      future_headline <- extended_score[
        extended_score$date == future_date &
          extended_score$indicator == "National Housing Affordability Score",
        "value"
      ]
      check(length(future_headline) == 1 && future_headline == 0,
            "An extreme post-reference burden must score 0 against the frozen distribution")
    }
  }
})
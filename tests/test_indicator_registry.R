repo_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
failures <- character()

check <- function(condition, message) {
  if (!isTRUE(condition)) {
    failures <<- c(failures, message)
  }
}

registry_path <- file.path(repo_root, "R", "indicator_registry.R")
check(file.exists(registry_path), "R/indicator_registry.R does not exist")

if (file.exists(registry_path)) {
  source(registry_path)

  required_functions <- c(
    "indicator_registry",
    "indicator_registry_required_abs_sources",
    "indicator_registry_required_rba_sources",
    "indicator_chart_label",
    "indicator_metadata"
  )
  for (fn in required_functions) {
    check(exists(fn, mode = "function"), paste("Missing registry helper:", fn))
  }

  registry <- indicator_registry()
  required_columns <- c(
    "indicator",
    "chart_label",
    "unit",
    "geography",
    "frequency",
    "concept_group",
    "interpretation_direction",
    "formula",
    "source_files",
    "source_series",
    "official_measure",
    "stylised_scenario",
    "minimum_rows"
  )
  missing_columns <- setdiff(required_columns, names(registry))
  check(length(missing_columns) == 0,
        paste("Registry missing columns:", paste(missing_columns, collapse = ", ")))
  check(anyDuplicated(registry$indicator) == 0,
        "Registry indicator values must be unique")

  afford_path <- file.path(repo_root, "data", "affordability_indices.csv")
  check(file.exists(afford_path), "data/affordability_indices.csv does not exist")
  if (file.exists(afford_path)) {
    afford_idx <- read.csv(afford_path, stringsAsFactors = FALSE)
    missing_registry <- setdiff(unique(afford_idx$indicator), registry$indicator)
    extra_registry <- setdiff(registry$indicator, unique(afford_idx$indicator))
    check(length(missing_registry) == 0,
          paste("Saved indicators missing from registry:",
                paste(missing_registry, collapse = ", ")))
    check(length(extra_registry) == 0,
          paste("Registry contains indicators absent from saved CSV:",
                paste(extra_registry, collapse = ", ")))
  }

  required_abs_sources <- c(
    "RPPI",
    "WPI",
    "CPI All Groups",
    "CPI Inflation YoY",
    "AWE (AWOTE, Persons)",
    "CPI Rents ; Weighted average of eight capital cities ;"
  )
  required_rba_sources <- "Lending rates; Housing loans; Banks; Variable; Discounted; Owner-occupier"
  check(identical(sort(indicator_registry_required_abs_sources()), sort(required_abs_sources)),
        "ABS source constants do not match required source labels")
  check(identical(indicator_registry_required_rba_sources(), required_rba_sources),
        "RBA source constants do not match required source label")

  required_labels <- c(
    "Price-to-Income Cost Pressure",
    "Modelled Mortgage Cost Pressure",
    "Rent Cost Pressure",
    "Stylised Deposit Gap (Years)",
    "National Housing Affordability Score",
    "Mortgage Serviceability Component",
    "Rental Entry Component",
    "Deposit Barrier Component"
  )
  missing_labels <- setdiff(required_labels, registry$chart_label)
  check(length(missing_labels) == 0,
        paste("Registry missing chart labels:", paste(missing_labels, collapse = ", ")))

  check(all(!is.na(registry$interpretation_direction) &
              nzchar(registry$interpretation_direction)),
        "Every registry row must declare interpretation_direction")
  cost_pressure <- registry[registry$concept_group == "cost_pressure", ]
  check(nrow(cost_pressure) > 0, "Registry has no cost_pressure indicators")
  check(all(cost_pressure$interpretation_direction == "higher_less_affordable"),
        "Cost-pressure indicators must use higher_less_affordable interpretation")

  score_registry <- registry[
    registry$indicator %in% c(
      "National Housing Affordability Score",
      "Mortgage Serviceability Component Score",
      "Rental Entry Component Score",
      "Deposit Barrier Component Score"
    ),
  ]
  check(nrow(score_registry) == 4,
        "Registry must contain headline and component score rows")
  if (nrow(score_registry) == 4) {
    check(all(score_registry$unit == "Score (0-100)"),
          "Score registry rows must use Score (0-100) units")
    check(all(score_registry$concept_group == "market_entry_composite"),
          "Score registry rows must use market_entry_composite concept_group")
    check(all(score_registry$interpretation_direction == "higher_more_affordable"),
          "Score registry rows must use higher_more_affordable interpretation")
    check(all(!score_registry$official_measure),
          "Score registry rows must not be official ABS measures")
    check(all(score_registry$stylised_scenario),
          "Score registry rows must be marked as stylised scenarios")
    score_formula_text <- paste(score_registry$formula, collapse = "\n")
    required_score_formula_text <- c(
      "40 per cent",
      "35 per cent",
      "25 per cent",
      "Not an official ABS/NHHA statistic or lender assessment"
    )
    missing_score_formula_text <- required_score_formula_text[
      !vapply(required_score_formula_text, grepl, logical(1),
              score_formula_text, fixed = TRUE)
    ]
    check(length(missing_score_formula_text) == 0,
          paste("Score registry formulas missing text:",
                paste(missing_score_formula_text, collapse = "; ")))
  }

  minimum_expectations <- c(
    "Real House Price Growth YoY" = 50L,
    "Real Wage Growth YoY" = 80L,
    "Real Mortgage Rate" = 50L
  )
  for (indicator in names(minimum_expectations)) {
    row <- registry[registry$indicator == indicator, ]
    check(nrow(row) == 1, paste("Missing minimum-row registry row:", indicator))
    if (nrow(row) == 1) {
      check(row$minimum_rows >= minimum_expectations[[indicator]],
            paste(indicator, "minimum_rows is below expected threshold"))
    }
  }

  check(identical(indicator_chart_label("Price-to-Income Ratio"),
                  "Price-to-Income Cost Pressure"),
        "indicator_chart_label() returned an unexpected label")
  check(nrow(indicator_metadata("Real Mortgage Rate")) == 1,
        "indicator_metadata() should return one row for a known indicator")

  check(exists("market_entry_scenario_methodology_note", mode = "function"),
        "market_entry_scenario_methodology_note() must be defined")
  if (exists("market_entry_scenario_methodology_note", mode = "function")) {
    scenario_note <- paste(market_entry_scenario_methodology_note(),
                           collapse = "\n")
    required_scenario_note_text <- c(
      "R/market_entry_scenarios.R",
      "app-only market-entry scenarios",
      "Assessment buffer and expense inputs are sensitivity assumptions, not a lender assessment"
    )
    missing_scenario_note_text <- required_scenario_note_text[
      !vapply(required_scenario_note_text, grepl, logical(1),
              scenario_note, fixed = TRUE)
    ]
    check(length(missing_scenario_note_text) == 0,
          paste("market_entry_scenario_methodology_note() missing text:",
                paste(missing_scenario_note_text, collapse = "; ")))
  }
}

if (length(failures) > 0) {
  stop(
    paste(c("Indicator registry checks failed:", paste0("- ", failures)),
          collapse = "\n"),
    call. = FALSE
  )
}

cat("Indicator registry checks passed.\n")

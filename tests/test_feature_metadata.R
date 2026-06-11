# Runs standalone via `Rscript tests/test_feature_metadata.R` (repo root) and under
# testthat::test_dir("tests"); see tests/helper-contracts.R.
if (!exists("contracts_harness_loaded", mode = "function")) {
  source(file.path(if (basename(getwd()) == "tests") "." else "tests",
                   "helper-contracts.R"))
}

test_that("feature_metadata contracts", {
  repo_root <- repo_root_path()


  metadata_path <- file.path(repo_root, "R", "feature_metadata.R")
  check(file.exists(metadata_path), "R/feature_metadata.R does not exist")

  if (file.exists(metadata_path)) {
    parsed <- tryCatch({
      parse(metadata_path)
      TRUE
    }, error = function(e) conditionMessage(e))
    check(identical(parsed, TRUE),
          paste(metadata_path, "does not parse:", parsed))

    suppressPackageStartupMessages(library(shiny))
    source(metadata_path, local = TRUE)

    check(exists("feature_metadata_registry", mode = "function"),
          "feature_metadata_registry() must be defined")
    check(exists("feature_metadata", mode = "function"),
          "feature_metadata() must be defined")
    check(exists("feature_source_note", mode = "function"),
          "feature_source_note() must be defined")

    if (exists("feature_metadata_registry", mode = "function")) {
      registry <- feature_metadata_registry()
      required_columns <- c(
        "feature_id", "title", "measure_class", "source_label",
        "economic_role", "caveat"
      )
      missing_columns <- setdiff(required_columns, names(registry))
      check(length(missing_columns) == 0,
            paste("Feature metadata registry missing columns:",
                  paste(missing_columns, collapse = ", ")))

      allowed_classes <- c(
        "official_survey", "derived_index", "stylised_scenario", "context"
      )
      check(all(registry$measure_class %in% allowed_classes),
            "Feature metadata measure_class values must use the established classes")
      check(!any(duplicated(registry$feature_id)),
            "Feature metadata feature_id values must be unique")

      required_features <- c(
        "overview_highest_capital_price",
        "market_context_underutilisation",
        "housing_supply_selected_approvals",
        "housing_supply_largest_selected_jurisdiction",
        "rental_market_weekly_rent",
        "rental_market_rent_to_income",
        "methodology_source_audit"
      )
      missing_features <- setdiff(required_features, registry$feature_id)
      check(length(missing_features) == 0,
            paste("Feature metadata missing required feature IDs:",
                  paste(missing_features, collapse = ", ")))
    }

    if (exists("feature_metadata", mode = "function")) {
      one <- feature_metadata("rental_market_rent_to_income")
      check(nrow(one) == 1,
            "feature_metadata(feature_id) must return one row for an existing feature")
      check(identical(one$measure_class[1], "official_survey"),
            "Rental cost ratio metadata must be classed as official_survey")
      missing <- tryCatch({
        feature_metadata("missing_feature")
        FALSE
      }, error = function(e) TRUE)
      check(missing,
            "feature_metadata() must fail clearly for unknown feature IDs")
    }

    if (exists("feature_source_note", mode = "function")) {
      note <- paste(as.character(feature_source_note("methodology_source_audit")),
                    collapse = "\n")
      check(grepl("source-note", note, fixed = TRUE),
            "feature_source_note() must render source-note UI markup")
      check(grepl("candidate", note, ignore.case = TRUE),
            "Methodology source audit note must explain candidate-source status")
    }
  }
})
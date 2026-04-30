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
    "Stylised Deposit Gap (Years)"
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
}

if (length(failures) > 0) {
  stop(
    paste(c("Indicator registry checks failed:", paste0("- ", failures)),
          collapse = "\n"),
    call. = FALSE
  )
}

cat("Indicator registry checks passed.\n")

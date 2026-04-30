repo_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
failures <- character()

check <- function(condition, message) {
  if (!isTRUE(condition)) {
    failures <<- c(failures, message)
  }
}

registry_path <- file.path(repo_root, "R", "indicator_registry.R")
app_path <- file.path(repo_root, "app.R")
description_path <- file.path(repo_root, "DESCRIPTION")

check(file.exists(registry_path), "R/indicator_registry.R does not exist")
check(file.exists(app_path), "app.R does not exist")
check(file.exists(description_path), "DESCRIPTION does not exist")

if (file.exists(registry_path)) {
  source(registry_path)

  expected_direction_labels <- c(
    higher_less_affordable = "Higher = less affordable",
    higher_more_affordable = "Higher = more affordable"
  )
  for (direction in names(expected_direction_labels)) {
    actual <- indicator_interpretation_label(direction)
    check(identical(actual, expected_direction_labels[[direction]]),
          paste("Unexpected interpretation label for", direction))
  }

  registry <- indicator_registry()
  methodology_table <- indicator_registry_methodology_table()
  check(nrow(methodology_table) == nrow(registry),
        "Methodology table must have one row per registry indicator")

  required_methodology_columns <- c(
    "Indicator",
    "Chart Label",
    "Concept Group",
    "Unit",
    "Frequency",
    "Interpretation",
    "Formula",
    "Source Files",
    "Source Series",
    "Official Measure",
    "Stylised Scenario",
    "Minimum Rows"
  )
  missing_methodology_columns <- setdiff(required_methodology_columns,
                                         names(methodology_table))
  check(length(missing_methodology_columns) == 0,
        paste("Methodology table missing columns:",
              paste(missing_methodology_columns, collapse = ", ")))

  non_empty_columns <- c("Indicator", "Chart Label", "Interpretation",
                         "Formula", "Source Files", "Source Series")
  for (column in non_empty_columns) {
    if (column %in% names(methodology_table)) {
      check(all(!is.na(methodology_table[[column]]) &
                  nzchar(methodology_table[[column]])),
            paste(column, "must be non-empty for every methodology row"))
    }
  }
}

if (file.exists(app_path)) {
  app_text <- paste(readLines(app_path, warn = FALSE), collapse = "\n")
  required_app_text <- c(
    'nav_panel("Methodology"',
    'tableOutput("methodology_indicator_table")',
    "renderTable",
    "R/indicator_registry.R",
    "pipeline/05_driver.R",
    "pipeline/06_validate_outputs.R",
    "data/*.csv",
    "AWE is individual earnings",
    "WPI is a wage price index",
    "CPI rents and CPI new dwelling indexes are price indexes",
    "stylised scenarios are not official ABS measures or lender assessments",
    "Official SIH/NHHA burden measures",
    "Market-entry cost-pressure indexes",
    "Stylised scenario calculators"
  )
  missing_app_text <- required_app_text[
    !vapply(required_app_text, grepl, logical(1), app_text, fixed = TRUE)
  ]
  check(length(missing_app_text) == 0,
        paste("app.R missing methodology page text:",
              paste(missing_app_text, collapse = "; ")))
}

tracked_text <- paste(
  vapply(c(app_path, registry_path, description_path), function(path) {
    if (file.exists(path)) paste(readLines(path, warn = FALSE), collapse = "\n") else ""
  }, character(1)),
  collapse = "\n"
)
check(!grepl("DT::", tracked_text, fixed = TRUE),
      "Methodology page must not introduce DT dependency usage")
if (file.exists(description_path)) {
  desc_lines <- readLines(description_path, warn = FALSE)
  check(!any(grepl("^\\s*DT\\s*,?\\s*$", desc_lines)),
        "DESCRIPTION must not add DT as a dependency")
}

if (length(failures) > 0) {
  stop(
    paste(c("Methodology page checks failed:", paste0("- ", failures)),
          collapse = "\n"),
    call. = FALSE
  )
}

cat("Methodology page checks passed.\n")

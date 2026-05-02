repo_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
failures <- character()

check <- function(condition, message) {
  if (!isTRUE(condition)) {
    failures <<- c(failures, message)
  }
}

module_path <- file.path(repo_root, "R", "housing_supply_module.R")
helper_path <- file.path(repo_root, "R", "app_ui_helpers.R")
check(file.exists(module_path), "R/housing_supply_module.R does not exist")
check(file.exists(helper_path), "R/app_ui_helpers.R does not exist")

if (file.exists(module_path)) {
  parsed <- tryCatch({
    parse(module_path)
    TRUE
  }, error = function(e) conditionMessage(e))
  check(identical(parsed, TRUE),
        paste("R/housing_supply_module.R does not parse:", parsed))
}

if (file.exists(module_path)) {
  suppressPackageStartupMessages({
    library(shiny)
    library(bslib)
    library(plotly)
  })
  source(helper_path)
  source(module_path)

  check(exists("supply_approval_series_components", mode = "function"),
        "supply_approval_series_components() must be defined")

  if (exists("supply_approval_series_components", mode = "function")) {
    parsed_series <- supply_approval_series_components(c(
      "Total number of dwelling units ;  New South Wales ;  Total (Type of Building) ;  Total Sectors ;",
      "Total number of dwelling units ;  Victoria ;  Houses ;  Private Sector ;"
    ))
    required_columns <- c(
      "series", "approval_state", "approval_building_type",
      "approval_sector", "approval_label"
    )
    missing_columns <- setdiff(required_columns, names(parsed_series))
    check(length(missing_columns) == 0,
          paste("Parsed approval series missing columns:",
                paste(missing_columns, collapse = ", ")))

    if (length(missing_columns) == 0) {
      check(identical(parsed_series$approval_state,
                      c("New South Wales", "Victoria")),
            "Approval parser must extract state")
      check(identical(parsed_series$approval_building_type,
                      c("Total approvals", "Houses")),
            "Approval parser must map building type to short labels")
      check(identical(parsed_series$approval_sector,
                      c("Total sectors", "Private sector")),
            "Approval parser must map sector to short labels")
      check(identical(parsed_series$approval_label,
                      c("NSW", "VIC")),
            "Approval parser must use short state legend labels")
    }
  }

  module_text <- paste(readLines(module_path, warn = FALSE), collapse = "\n")
  required_text <- c(
    'selectizeInput(ns("supply_states")',
    'selectInput(ns("supply_building_type")',
    'selectInput(ns("supply_sector")',
    'selected = supply_state_choices',
    'selected = "Total approvals"',
    'selected = "Total sectors"',
    "approval_state %in% input$supply_states",
    "approval_building_type == input$supply_building_type",
    "approval_sector == input$supply_sector",
    "color = approval_label",
    "bindCache(input$supply_dates, input$supply_states, input$supply_building_type, input$supply_sector, is_dark())",
    "dashboard_ggplotly"
  )
  missing_text <- required_text[
    !vapply(required_text, grepl, logical(1), module_text, fixed = TRUE)
  ]
  check(length(missing_text) == 0,
        paste("Housing Supply legend contract text missing:",
              paste(missing_text, collapse = "; ")))

  forbidden_text <- c(
    "series_short",
    'str_remove("Total number of dwelling units',
    "str_replace(\";\\\\s*\", \" - \")"
  )
  found_forbidden <- forbidden_text[
    vapply(forbidden_text, grepl, logical(1), module_text, fixed = TRUE)
  ]
  check(length(found_forbidden) == 0,
        paste("Housing Supply chart must not build legend labels from full ABS strings:",
              paste(found_forbidden, collapse = "; ")))

  if (exists("housingSupplyPageUI", mode = "function")) {
    module_ui <- paste(as.character(housingSupplyPageUI("housing_supply")),
                       collapse = "\n")
    required_ui <- c(
      "States/Territories",
      "Building type",
      "Sector",
      "housing_supply-supply_states",
      "housing_supply-supply_building_type",
      "housing_supply-supply_sector",
      "New South Wales",
      "Victoria",
      "Total approvals",
      "Total sectors"
    )
    missing_ui <- required_ui[
      !vapply(required_ui, grepl, logical(1), module_ui, fixed = TRUE)
    ]
    check(length(missing_ui) == 0,
          paste("Housing Supply UI missing legend controls/defaults:",
                paste(missing_ui, collapse = "; ")))
  }
}

if (length(failures) > 0) {
  stop(
    paste(c("Housing Supply legend contract checks failed:",
            paste0("- ", failures)),
          collapse = "\n"),
    call. = FALSE
  )
}

cat("Housing Supply legend contract checks passed.\n")

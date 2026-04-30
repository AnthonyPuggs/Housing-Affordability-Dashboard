repo_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
failures <- character()

check <- function(condition, message) {
  if (!isTRUE(condition)) {
    failures <<- c(failures, message)
  }
}

helper_path <- file.path(repo_root, "R", "app_ui_helpers.R")
module_path <- file.path(repo_root, "R", "methodology_module.R")
app_path <- file.path(repo_root, "app.R")
readme_path <- file.path(repo_root, "README.md")

check(file.exists(helper_path), "R/app_ui_helpers.R does not exist")
check(file.exists(module_path), "R/methodology_module.R does not exist")
check(file.exists(app_path), "app.R does not exist")
check(file.exists(readme_path), "README.md does not exist")

for (path in c(helper_path, module_path)) {
  if (file.exists(path)) {
    parsed <- tryCatch({
      parse(path)
      TRUE
    }, error = function(e) conditionMessage(e))
    check(identical(parsed, TRUE), paste(path, "does not parse:", parsed))
  }
}

if (all(file.exists(c(helper_path, module_path)))) {
  suppressPackageStartupMessages({
    library(shiny)
    library(bslib)
  })
  source(file.path(repo_root, "R", "indicator_registry.R"))
  source(helper_path)
  source(module_path)

  check(exists("source_note", mode = "function"),
        "source_note() must be defined by R/app_ui_helpers.R")
  check(exists("stylised_scenario_note"),
        "stylised_scenario_note must be defined by R/app_ui_helpers.R")
  check(exists("methodologyPageUI", mode = "function"),
        "methodologyPageUI() must be defined by R/methodology_module.R")
  check(exists("methodologyPageServer", mode = "function"),
        "methodologyPageServer() must be defined by R/methodology_module.R")

  module_ui <- paste(as.character(methodologyPageUI("methodology")),
                     collapse = "\n")
  required_ui_text <- c(
    "Methodology",
    "Methodology &amp; Provenance",
    "Derived Indicator Registry",
    "AWE is individual earnings",
    "pipeline/05_driver.R",
    "R/indicator_registry.R"
  )
  missing_ui_text <- required_ui_text[
    !vapply(required_ui_text, grepl, logical(1), module_ui, fixed = TRUE)
  ]
  check(length(missing_ui_text) == 0,
        paste("methodologyPageUI() missing text:",
              paste(missing_ui_text, collapse = "; ")))
}

if (file.exists(module_path)) {
  module_text <- paste(readLines(module_path, warn = FALSE), collapse = "\n")
  required_module_text <- c(
    "NS(id)",
    "moduleServer",
    'tableOutput(ns("indicator_table"))',
    "output$indicator_table <- renderTable",
    'downloadButton(ns("provenance_download")',
    "downloadHandler",
    "methodology_provenance_report"
  )
  missing_module_text <- required_module_text[
    !vapply(required_module_text, grepl, logical(1), module_text, fixed = TRUE)
  ]
  check(length(missing_module_text) == 0,
        paste("R/methodology_module.R missing module constructs:",
              paste(missing_module_text, collapse = "; ")))
}

if (file.exists(app_path)) {
  app_text <- paste(readLines(app_path, warn = FALSE), collapse = "\n")
  required_app_text <- c(
    'source(project_path("R", "app_ui_helpers.R"), local = TRUE)',
    'source(project_path("R", "methodology_module.R"), local = TRUE)',
    'methodologyPageUI("methodology")',
    'methodologyPageServer("methodology")'
  )
  missing_app_text <- required_app_text[
    !vapply(required_app_text, grepl, logical(1), app_text, fixed = TRUE)
  ]
  check(length(missing_app_text) == 0,
        paste("app.R missing methodology module wiring:",
              paste(missing_app_text, collapse = "; ")))
  check(!grepl("output$methodology_indicator_table", app_text, fixed = TRUE),
        "app.R must not keep the old inline methodology table output")
}

if (file.exists(readme_path)) {
  readme_text <- paste(readLines(readme_path, warn = FALSE), collapse = "\n")
  check(grepl("R/methodology_module.R", readme_text, fixed = TRUE),
        "README.md must document the Methodology module pilot")
}

if (length(failures) > 0) {
  stop(
    paste(c("Methodology module checks failed:", paste0("- ", failures)),
          collapse = "\n"),
    call. = FALSE
  )
}

cat("Methodology module checks passed.\n")

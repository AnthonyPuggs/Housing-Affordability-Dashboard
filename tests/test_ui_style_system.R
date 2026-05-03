repo_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
failures <- character()

check <- function(condition, message) {
  if (!isTRUE(condition)) {
    failures <<- c(failures, message)
  }
}

read_text <- function(path) {
  paste(readLines(path, warn = FALSE), collapse = "\n")
}

style_path <- file.path(repo_root, "R", "ui_style_system.R")
app_path <- file.path(repo_root, "app.R")
readme_path <- file.path(repo_root, "README.md")
description_path <- file.path(repo_root, "DESCRIPTION")
module_paths <- list.files(file.path(repo_root, "R"),
                           pattern = "_module[.]R$",
                           full.names = TRUE)

check(file.exists(style_path), "R/ui_style_system.R does not exist")
check(file.exists(app_path), "app.R does not exist")
check(file.exists(readme_path), "README.md does not exist")
check(file.exists(description_path), "DESCRIPTION does not exist")

if (file.exists(style_path)) {
  parsed <- tryCatch({
    parse(style_path)
    TRUE
  }, error = function(e) conditionMessage(e))
  check(identical(parsed, TRUE),
        paste("R/ui_style_system.R does not parse:", parsed))

  source(style_path)
  required_functions <- c(
    "policy_page_header",
    "policy_kpi_box",
    "policy_chart_card",
    "policy_source_note",
    "policy_card"
  )
  missing_functions <- required_functions[
    !vapply(required_functions, exists, logical(1), mode = "function")
  ]
  check(length(missing_functions) == 0,
        paste("R/ui_style_system.R missing functions:",
              paste(missing_functions, collapse = ", ")))

  style_text <- read_text(style_path)
  required_style_classes <- c(
    "policy-page-header",
    "policy-kpi",
    "policy-chart-card",
    "policy-source-note"
  )
  missing_style_classes <- required_style_classes[
    !vapply(required_style_classes, grepl, logical(1),
            style_text, fixed = TRUE)
  ]
  check(length(missing_style_classes) == 0,
        paste("UI helper missing public-policy classes:",
              paste(missing_style_classes, collapse = ", ")))

  suppressPackageStartupMessages({
    library(shiny)
    library(bslib)
  })
  chart_card_html <- paste(as.character(
    policy_chart_card(
      "Chart title",
      note = "Source note",
      div(class = "chart-wide", "Chart body")
    )
  ), collapse = "\n")
  card_body_matches <- gregexpr("class=\"[^\"]*card-body",
                                chart_card_html, perl = TRUE)[[1]]
  card_body_count <- if (identical(card_body_matches, -1L)) {
    0L
  } else {
    length(card_body_matches)
  }
  check(card_body_count == 1L,
        "policy_chart_card() must keep note and chart content in one card body")
  check(grepl("policy-source-note", chart_card_html, fixed = TRUE),
        "policy_chart_card() must retain policy source-note markup")
  check(grepl("chart-wide", chart_card_html, fixed = TRUE),
        "policy_chart_card() must retain chart body markup")
}

app_text <- if (file.exists(app_path)) read_text(app_path) else ""
module_text <- paste(vapply(module_paths, read_text, character(1)),
                     collapse = "\n")

if (nzchar(app_text)) {
  style_source <- 'source(project_path("R", "ui_style_system.R"), local = TRUE)'
  app_ui_source <- 'source(project_path("R", "app_ui_helpers.R"), local = TRUE)'
  check(grepl(style_source, app_text, fixed = TRUE),
        "app.R must source R/ui_style_system.R")
  check(grepl(app_ui_source, app_text, fixed = TRUE),
        "app.R must source R/app_ui_helpers.R")
  check(regexpr(style_source, app_text, fixed = TRUE)[1] <
          regexpr('source(project_path("R", "overview_module.R"), local = TRUE)',
                  app_text, fixed = TRUE)[1],
        "app.R must source the UI style system before page modules")

  required_css <- c(
    "--policy-accent",
    "--policy-surface",
    ".policy-page-header",
    ".policy-kpi",
    ".policy-chart-card",
    ".policy-card-body-with-note",
    ".policy-source-note",
    ".bslib-sidebar-layout > .sidebar"
  )
  missing_css <- required_css[
    !vapply(required_css, grepl, logical(1), app_text, fixed = TRUE)
  ]
  check(length(missing_css) == 0,
        paste("app.R missing public-policy CSS:",
              paste(missing_css, collapse = ", ")))

  check(!grepl("font_google\\s*\\(", app_text, perl = TRUE),
        "app.R must not use Google-hosted fonts")
}

required_module_calls <- c(
  "policy_page_header(",
  "policy_kpi_box(",
  "policy_chart_card(",
  "policy_card("
)
missing_module_calls <- required_module_calls[
  !vapply(required_module_calls, grepl, logical(1), module_text, fixed = TRUE)
]
check(length(missing_module_calls) == 0,
      paste("Page modules missing UI style helper calls:",
            paste(missing_module_calls, collapse = ", ")))

forbidden_kpi_theme_literals <- c(
  'value_box_theme(bg = "#0E5A8A"',
  'value_box_theme(bg = "#3B4C7A"',
  'value_box_theme(bg = "#17415F"',
  'value_box_theme(bg = "#984ea3"'
)
present_kpi_literals <- forbidden_kpi_theme_literals[
  vapply(forbidden_kpi_theme_literals, grepl, logical(1),
         module_text, fixed = TRUE)
]
check(length(present_kpi_literals) == 0,
      paste("Modules still use saturated KPI value_box_theme literals:",
            paste(present_kpi_literals, collapse = ", ")))

if (file.exists(description_path)) {
  desc_text <- read_text(description_path)
  forbidden_direct_dependencies <- c(
    "shinyjs",
    "htmltools",
    "waiter",
    "shinycssloaders",
    "DT",
    "showtext",
    "sysfonts"
  )
  unexpected <- forbidden_direct_dependencies[
    vapply(forbidden_direct_dependencies, function(pkg) {
      grepl(paste0("(^|\\n)\\s*", pkg, "\\s*,?\\s*($|\\n)"),
            desc_text, perl = TRUE)
    }, logical(1))
  ]
  check(length(unexpected) == 0,
        paste("DESCRIPTION must not add new direct dependencies:",
              paste(unexpected, collapse = ", ")))
}

if (file.exists(readme_path)) {
  readme_text <- read_text(readme_path)
  required_readme <- c(
    "public-policy report UI system",
    "R/ui_style_system.R",
    "Rscript tests/test_ui_style_system.R"
  )
  missing_readme <- required_readme[
    !vapply(required_readme, grepl, logical(1),
            readme_text, fixed = TRUE)
  ]
  check(length(missing_readme) == 0,
        paste("README.md missing UI style documentation:",
              paste(missing_readme, collapse = ", ")))
}

if (length(failures) > 0) {
  stop(
    paste(c("UI style system checks failed:", paste0("- ", failures)),
          collapse = "\n"),
    call. = FALSE
  )
}

cat("UI style system checks passed.\n")

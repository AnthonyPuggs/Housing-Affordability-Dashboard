repo_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
helper_path <- file.path(repo_root, "R", "visual_semantics.R")
app_path <- file.path(repo_root, "app.R")
readme_path <- file.path(repo_root, "README.md")
failures <- character()

check <- function(condition, message) {
  if (!isTRUE(condition)) {
    failures <<- c(failures, message)
  }
}

check(file.exists(helper_path), "R/visual_semantics.R does not exist")

if (file.exists(helper_path)) {
  parsed <- tryCatch({
    parse(helper_path)
    TRUE
  }, error = function(e) conditionMessage(e))
  check(identical(parsed, TRUE),
        paste("R/visual_semantics.R does not parse:", parsed))

  source(helper_path)

  required_functions <- c(
    "semantic_colour",
    "semantic_colours",
    "kpi_change_class",
    "stress_band_palette",
    "burden_gradient_colours",
    "cost_pressure_palette",
    "rental_stress_gradient_colours"
  )
  missing_functions <- required_functions[
    !vapply(required_functions, exists, logical(1), mode = "function")
  ]
  check(length(missing_functions) == 0,
        paste("R/visual_semantics.R missing functions:",
              paste(missing_functions, collapse = ", ")))

  if (exists("semantic_colours", mode = "function")) {
    colours <- semantic_colours()
    required_names <- c("better", "worse", "neutral", "caution", "reference")
    missing_names <- setdiff(required_names, names(colours))
    check(length(missing_names) == 0,
          paste("semantic_colours() missing names:",
                paste(missing_names, collapse = ", ")))
    check(all(grepl("^#[0-9A-Fa-f]{6}$", colours[required_names])),
          "Required semantic colours must be six-digit hex colours")
  }

  if (exists("kpi_change_class", mode = "function")) {
    check(identical(kpi_change_class(1, "decrease"), "kpi-change-worse"),
          "Increase in a decrease-favourable metric must be worse")
    check(identical(kpi_change_class(-1, "decrease"), "kpi-change-better"),
          "Decrease in a decrease-favourable metric must be better")
    check(identical(kpi_change_class(1, "increase"), "kpi-change-better"),
          "Increase in an increase-favourable metric must be better")
    check(identical(kpi_change_class(-1, "increase"), "kpi-change-worse"),
          "Decrease in an increase-favourable metric must be worse")
    check(identical(kpi_change_class(1, "neutral"), "kpi-change-neutral"),
          "Neutral metrics must use neutral KPI class")
    check(identical(kpi_change_class(NA_real_, "increase"),
                    "kpi-change-neutral"),
          "Missing KPI changes must use neutral KPI class")
  }
}

if (file.exists(app_path)) {
  app_text <- paste(readLines(app_path, warn = FALSE), collapse = "\n")
  required_app_text <- c(
    'source(project_path("R", "visual_semantics.R"), local = TRUE)',
    ".kpi-change-better",
    ".kpi-change-worse",
    ".kpi-change-neutral",
    ".kpi-change-up",
    ".kpi-change-down"
  )
  missing_app_text <- required_app_text[
    !vapply(required_app_text, grepl, logical(1), app_text, fixed = TRUE)
  ]
  check(length(missing_app_text) == 0,
        paste("app.R missing semantic colour wiring:",
              paste(missing_app_text, collapse = "; ")))
}

module_text <- paste(vapply(
  list.files(file.path(repo_root, "R"), pattern = "_module[.]R$",
             full.names = TRUE),
  function(path) paste(readLines(path, warn = FALSE), collapse = "\n"),
  character(1)
), collapse = "\n")

required_module_text <- c(
  'kpi_change_class(ch$change, favourable = "decrease")',
  'kpi_change_class(diff_val, favourable = "decrease")',
  'kpi_change_class(diff_val, favourable = "increase")',
  'kpi_change_class(pct, favourable = "increase")',
  'kpi_change_class(pct, favourable = "neutral")',
  'kpi_change_class(diff_pp, favourable = "neutral")',
  "stress_band_palette()",
  "burden_gradient_colours()",
  "cost_pressure_palette(",
  "rental_stress_gradient_colours()",
  'semantic_colour("worse")',
  'semantic_colour("caution")'
)
missing_module_text <- required_module_text[
  !vapply(required_module_text, grepl, logical(1), module_text, fixed = TRUE)
]
check(length(missing_module_text) == 0,
      paste("Modules missing semantic colour contracts:",
            paste(missing_module_text, collapse = "; ")))

if (file.exists(readme_path)) {
  readme_text <- paste(readLines(readme_path, warn = FALSE), collapse = "\n")
  required_readme <- c(
    "KPI colours encode economic interpretation",
    "better, worse or neutral/contextual",
    "R/visual_semantics.R"
  )
  missing_readme <- required_readme[
    !vapply(required_readme, grepl, logical(1), readme_text, fixed = TRUE)
  ]
  check(length(missing_readme) == 0,
        paste("README.md missing visual semantics documentation:",
              paste(missing_readme, collapse = "; ")))
}

if (length(failures) > 0) {
  stop(
    paste(c("Visual semantics checks failed:", paste0("- ", failures)),
          collapse = "\n"),
    call. = FALSE
  )
}

cat("Visual semantics checks passed.\n")

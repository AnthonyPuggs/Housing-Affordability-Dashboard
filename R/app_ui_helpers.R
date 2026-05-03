# Shared UI helpers for the dashboard app.

if (!exists("semantic_colour", mode = "function", inherits = TRUE)) {
  visual_semantics_path <- if (exists("project_path", mode = "function", inherits = TRUE)) {
    project_path("R", "visual_semantics.R")
  } else {
    file.path("R", "visual_semantics.R")
  }
  if (!file.exists(visual_semantics_path)) {
    stop("Could not locate R/visual_semantics.R for app UI helpers.",
         call. = FALSE)
  }
  source(visual_semantics_path, local = environment())
}

if (!exists("policy_page_header", mode = "function", inherits = TRUE)) {
  ui_style_path <- if (exists("project_path", mode = "function", inherits = TRUE)) {
    project_path("R", "ui_style_system.R")
  } else {
    file.path("R", "ui_style_system.R")
  }
  if (!file.exists(ui_style_path)) {
    stop("Could not locate R/ui_style_system.R for app UI helpers.",
         call. = FALSE)
  }
  source(ui_style_path, local = environment())
}

source_note <- function(...) {
  if (exists("policy_source_note", mode = "function", inherits = TRUE)) {
    return(policy_source_note(...))
  }
  tags$p(..., class = "source-note px-3",
         style = "color: var(--app-muted); font-size: 0.85rem; margin-bottom: 0;")
}

stylised_scenario_note <- "Stylised scenario, not an official ABS measure or lender assessment."

sih_sampling_error_note <- paste(
  "SIH estimates are survey estimates;",
  "relative standard error and 95% margin of error metadata are saved in data/sih_estimate_quality.csv,",
  "and users should interpret with caution when estimates have high RSE values."
)

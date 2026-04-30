# Shared UI helpers for the dashboard app.

source_note <- function(...) {
  tags$p(...,
         class = "source-note px-3",
         style = "color: var(--app-muted); font-size: 0.85rem; margin-bottom: 0;")
}

stylised_scenario_note <- "Stylised scenario, not an official ABS measure or lender assessment."

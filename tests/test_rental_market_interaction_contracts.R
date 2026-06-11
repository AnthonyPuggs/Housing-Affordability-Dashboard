# Runs standalone via `Rscript tests/test_rental_market_interaction_contracts.R` (repo root) and under
# testthat::test_dir("tests"); see tests/helper-contracts.R.
if (!exists("contracts_harness_loaded", mode = "function")) {
  source(file.path(if (basename(getwd()) == "tests") "." else "tests",
                   "helper-contracts.R"))
}

test_that("rental_market_interaction_contracts contracts", {
  repo_root <- repo_root_path()


  module_path <- file.path(repo_root, "R", "rental_market_module.R")
  chart_builders_path <- file.path(repo_root, "R", "chart_builders.R")
  readme_path <- file.path(repo_root, "README.md")

  check(file.exists(module_path), "R/rental_market_module.R does not exist")
  check(file.exists(chart_builders_path), "R/chart_builders.R does not exist")
  check(file.exists(readme_path), "README.md does not exist")

  if (file.exists(module_path)) {
    parsed <- tryCatch({
      parse(module_path)
      TRUE
    }, error = function(e) conditionMessage(e))
    check(identical(parsed, TRUE),
          paste("R/rental_market_module.R does not parse:", parsed))

    suppressPackageStartupMessages({
      library(shiny)
      library(bslib)
      library(plotly)
    })
    source(module_path, local = TRUE)

    check(exists("normalise_rental_states", mode = "function"),
          "normalise_rental_states() helper is missing")

    if (exists("normalise_rental_states", mode = "function")) {
      geographies <- c("Aust.", "NSW", "Qld", "Vic.", "WA")

      check(identical(normalise_rental_states("all", geographies),
                      c("NSW", "Qld", "Vic.", "WA")),
            "'all' should resolve to all non-national states")
      check(identical(normalise_rental_states(c("all", "NSW"), geographies),
                      "NSW"),
            "Explicit state selections should override the default 'all' token")
      check(identical(normalise_rental_states(c("Vic.", "NSW"), geographies),
                      c("Vic.", "NSW")),
            "Explicit state selection order should be preserved")
    }

    module_text <- paste(readLines(module_path, warn = FALSE), collapse = "\n")
    check(grepl("build_rental_stress_trend_plot(", module_text, fixed = TRUE),
          "Rental Market module must delegate NHHA heatmap construction to chart builder")
    total_filters <- gregexpr('breakdown_val == "Total"', module_text,
                              fixed = TRUE)[[1]]
    total_filter_count <- if (identical(total_filters, -1L)) {
      0L
    } else {
      length(total_filters)
    }
    check(total_filter_count >= 3,
          "Rental Market NHHA state/trend charts must filter to Total location rows")
  }

  if (file.exists(chart_builders_path)) {
    chart_builders_text <- paste(readLines(chart_builders_path, warn = FALSE),
                                 collapse = "\n")
    required_heatmap_text <- c(
      "build_rental_stress_trend_plot <- function(",
      "geom_text(",
      "tile_label",
      "scale_color_identity()",
      "show.legend = FALSE"
    )
    missing_heatmap_text <- required_heatmap_text[
      !vapply(required_heatmap_text, grepl, logical(1),
              chart_builders_text, fixed = TRUE)
    ]
    check(length(missing_heatmap_text) == 0,
          paste("NHHA heatmap must retain compact tile labels:",
                paste(missing_heatmap_text, collapse = "; ")))
  }

  if (file.exists(module_path)) {
    module_text <- paste(readLines(module_path, warn = FALSE), collapse = "\n")
    check(grepl('width = 280, open = "desktop"', module_text, fixed = TRUE),
          "Rental Market sidebar must be open on desktop and collapsed on mobile")
  }

  if (file.exists(readme_path)) {
    readme_text <- paste(readLines(readme_path, warn = FALSE), collapse = "\n")
    check(grepl("Rscript tests/test_rental_market_interaction_contracts.R",
                readme_text, fixed = TRUE),
          "README.md must document the Rental Market interaction contract test")
  }
})
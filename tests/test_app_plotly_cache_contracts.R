repo_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
app_path <- file.path(repo_root, "app.R")
module_paths <- list.files(file.path(repo_root, "R"),
                           pattern = "_module[.]R$",
                           full.names = TRUE)
failures <- character()

check <- function(condition, message) {
  if (!isTRUE(condition)) {
    failures <<- c(failures, message)
  }
}

check(file.exists(app_path), "app.R does not exist")

if (file.exists(app_path)) {
  app_text <- paste(readLines(app_path, warn = FALSE), collapse = "\n")
  plot_source_text <- paste(c(
    app_text,
    vapply(module_paths, function(path) {
      paste(readLines(path, warn = FALSE), collapse = "\n")
    }, character(1))
  ), collapse = "\n")

  check(grepl('source(project_path("R", "plotly_helpers.R"), local = TRUE)',
              app_text, fixed = TRUE),
        "app.R must source R/plotly_helpers.R")
  direct_ggplotly_pattern <- "(^|[^A-Za-z0-9_])ggplotly\\s*\\(|plotly::ggplotly\\s*\\("
  check(!grepl(direct_ggplotly_pattern, plot_source_text, perl = TRUE),
        "app.R and modules must not call ggplotly() directly")

  render_pattern <- "output\\$([A-Za-z0-9_]+)\\s*<-\\s*renderPlotly\\s*\\("
  render_matches <- gregexpr(render_pattern, plot_source_text, perl = TRUE)[[1]]
  if (identical(render_matches, -1L)) {
    render_ids <- character()
  } else {
    render_text <- regmatches(plot_source_text, list(render_matches))[[1]]
    render_ids <- sub(render_pattern, "\\1", render_text, perl = TRUE)
  }

  expected_plotly_outputs <- c(
    "overview_median_prices",
    "overview_afford_change",
    "price_chart",
    "supply_cpi_construction",
    "rent_cpi_chart",
    "afford_indices_chart",
    "afford_serviceability",
    "stress_chart",
    "burden_heatmap",
    "context_rates",
    "context_labour",
    "context_pop",
    "supply_approvals",
    "rental_stress_state",
    "rental_stress_trend",
    "rental_afford_index",
    "rental_costs_demo",
    "geo_state_trend",
    "geo_state_latest",
    "geo_lower_income",
    "geo_gcc_comparison"
  )

  missing_outputs <- setdiff(expected_plotly_outputs, render_ids)
  extra_outputs <- setdiff(render_ids, expected_plotly_outputs)
  check(length(missing_outputs) == 0,
        paste("Missing expected Plotly outputs:",
              paste(missing_outputs, collapse = ", ")))
  check(length(extra_outputs) == 0,
        paste("Unexpected Plotly outputs:",
              paste(extra_outputs, collapse = ", ")))

  helper_call_count <- gregexpr("dashboard_ggplotly\\s*\\(",
                                plot_source_text, perl = TRUE)[[1]]
  helper_call_count <- if (length(helper_call_count) == 1 &&
                             helper_call_count[1] == -1L) {
    0L
  } else {
    length(helper_call_count)
  }
  check(helper_call_count == length(expected_plotly_outputs),
        paste("Expected", length(expected_plotly_outputs),
              "dashboard_ggplotly() calls, found", helper_call_count))

  if (length(render_ids) > 0) {
    render_starts <- as.integer(render_matches)
    next_starts <- c(render_starts[-1], nchar(plot_source_text) + 1L)
    missing_bind_cache <- character()
    missing_dark_key <- character()
    for (i in seq_along(render_ids)) {
      segment <- substr(plot_source_text, render_starts[i], next_starts[i] - 1L)
      bind_start <- regexpr("bindCache\\s*\\(", segment, perl = TRUE)
      if (bind_start[1] == -1L) {
        missing_bind_cache <- c(missing_bind_cache, render_ids[i])
        next
      }
      bind_segment <- substr(segment, bind_start[1], nchar(segment))
      if (!grepl("is_dark\\s*\\(", bind_segment, perl = TRUE)) {
        missing_dark_key <- c(missing_dark_key, render_ids[i])
      }
    }

    check(length(missing_bind_cache) == 0,
          paste("renderPlotly outputs missing bindCache():",
                paste(missing_bind_cache, collapse = ", ")))
    check(length(missing_dark_key) == 0,
          paste("renderPlotly cache keys missing is_dark():",
                paste(missing_dark_key, collapse = ", ")))
  }
}

if (length(failures) > 0) {
  stop(
    paste(c("App Plotly cache contract checks failed:", paste0("- ", failures)),
          collapse = "\n"),
    call. = FALSE
  )
}

cat("App Plotly cache contract checks passed.\n")

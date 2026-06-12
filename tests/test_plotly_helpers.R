# Runs standalone via `Rscript tests/test_plotly_helpers.R` (repo root) and under
# testthat::test_dir("tests"); see tests/helper-contracts.R.
if (!exists("contracts_harness_loaded", mode = "function")) {
  source(file.path(if (basename(getwd()) == "tests") "." else "tests",
                   "helper-contracts.R"))
}

test_that("plotly_helpers contracts", {
  repo_root <- repo_root_path()


  helper_path <- file.path(repo_root, "R", "plotly_helpers.R")
  plot_setup_path <- file.path(repo_root, "plot_setup.R")
  description_path <- file.path(repo_root, "DESCRIPTION")

  check(file.exists(helper_path), "R/plotly_helpers.R does not exist")
  check(file.exists(plot_setup_path), "plot_setup.R does not exist")
  check(file.exists(description_path), "DESCRIPTION does not exist")

  if (file.exists(helper_path)) {
    parsed <- tryCatch({
      parse(helper_path)
      TRUE
    }, error = function(e) conditionMessage(e))
    check(identical(parsed, TRUE), paste("R/plotly_helpers.R does not parse:", parsed))
  }

  if (all(file.exists(c(helper_path, plot_setup_path)))) {
    use_fixture_data()
    source(plot_setup_path, local = TRUE)
    source(helper_path, local = TRUE)

    check(exists("dashboard_ggplotly", mode = "function"),
          "dashboard_ggplotly() must be defined")

    p <- ggplot2::ggplot(
      data.frame(x = 1:3, y = c(1, 4, 9)),
      ggplot2::aes(x = x, y = y)
    ) +
      ggplot2::geom_line()

    light_fig <- dashboard_ggplotly(p, dark = FALSE, tooltip = c("x", "y"))
    dark_fig <- dashboard_ggplotly(p, dark = TRUE, tooltip = c("x", "y"))
    sourced_fig <- dashboard_ggplotly(
      p,
      dark = FALSE,
      tooltip = c("x", "y"),
      source = "overview_afford_score"
    )
    hover_fig <- dashboard_ggplotly(
      p,
      dark = FALSE,
      tooltip = c("x", "y"),
      hovermode = "closest"
    )

    for (fig_name in c("light_fig", "dark_fig", "sourced_fig", "hover_fig")) {
      fig <- get(fig_name)
      check(inherits(fig, "plotly"), paste(fig_name, "must inherit from plotly"))
      check(inherits(fig, "htmlwidget"), paste(fig_name, "must inherit from htmlwidget"))
      check(length(fig$x$data) > 0, paste(fig_name, "must contain at least one trace"))
      check(!is.null(fig$x$layout), paste(fig_name, "must contain a Plotly layout"))
    }
    check(identical(sourced_fig$x$source, "overview_afford_score"),
          "dashboard_ggplotly() must forward a custom Plotly source")
    check(identical(light_fig$x$layout$hovermode, "x"),
          "dashboard_ggplotly() must preserve the default Plotly hovermode")
    check(identical(hover_fig$x$layout$hovermode, "closest"),
          "dashboard_ggplotly() must forward a custom Plotly hovermode")

    # Shared modebar policy (UX-07): every chart hides the Plotly logo and the
    # noisy selection/auto-scale tools via the central plotly_layout() hook.
    check(exists("dashboard_modebar_buttons_removed", mode = "function"),
          "dashboard_modebar_buttons_removed() must be defined")
    check(isFALSE(light_fig$x$config$displaylogo),
          "charts must hide the Plotly logo (displaylogo = FALSE)")
    removed <- light_fig$x$config$modeBarButtonsToRemove
    check(all(c("select2d", "lasso2d", "autoScale2d") %in% removed),
          "charts must remove the selection/auto-scale modebar tools")

    # Conditional date axis (UX-06): date_axis_config() scales tick density to
    # the data span. Tested as a pure span->spec helper; the spec is plumbed
    # through dashboard_ggplotly()/plotly_layout() (for native Plotly date axes
    # and hover formatting — ggplot's own scale_x_date governs ggplotly tick
    # text), and forwarding it must still yield a valid widget.
    check(exists("date_axis_config", mode = "function"),
          "date_axis_config() must be defined")
    short_axis <- date_axis_config(as.Date(c("2021-01-01", "2023-06-01")))
    check(identical(short_axis$tickformat, "%b %Y") &&
            identical(short_axis$dtick, "M3"),
          "a <=3 year span must use month-year ticks every quarter")
    mid_axis <- date_axis_config(as.Date(c("2016-01-01", "2024-01-01")))
    check(identical(mid_axis$tickformat, "%Y") &&
            identical(mid_axis$dtick, "M12"),
          "a 3-12 year span must use yearly ticks")
    long_axis <- date_axis_config(as.Date(c("1995-01-01", "2024-01-01")))
    check(identical(long_axis$dtick, "M60"),
          "a multi-decade span must keep five-year ticks")
    check(identical(date_axis_config(as.Date("2020-01-01"))$dtick, "M60"),
          "a single/empty date span must fall back to the long-history default")
    dated_fig <- dashboard_ggplotly(p, dark = FALSE, tooltip = c("x", "y"),
                                    date_axis = short_axis)
    check(inherits(dated_fig, "plotly"),
          "dashboard_ggplotly() must accept a conditional date axis spec")
  }

  if (file.exists(description_path)) {
    desc_text <- paste(readLines(description_path, warn = FALSE), collapse = "\n")
    forbidden_direct_dependencies <- c("memoise", "cachem", "DT", "shinycssloaders")
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
})
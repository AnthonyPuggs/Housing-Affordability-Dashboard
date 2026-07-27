# Shared Plotly rendering helpers for Shiny chart outputs.

dashboard_ggplotly <- function(p, dark, tooltip, hovermode = "x",
                               margin = NULL, source = "A",
                               date_axis = NULL) {
  fig <- plotly::ggplotly(p, tooltip = tooltip, source = source)
  fig <- plotly_layout(fig, dark = dark, hovermode = hovermode,
                       date_axis = date_axis)

  if (!is.null(margin)) {
    fig <- plotly::layout(fig, margin = margin)
  }

  fig
}

# ggplotly() renders geom_area/geom_ribbon as filled polygon traces with
# hoveron = "fills": the whole band gets a single tooltip pinned to the
# polygon's first vertex (the earliest date), wherever the cursor is.
# Switching those traces to point hover makes the tooltip follow the cursor
# along the band, showing the vertex's own date/value text.
# ggplotly names traces "(Series,1)" when a figure combines multiple scales
# (e.g. fill bands plus a colour line); strip the wrapper so the legend shows
# the plain series name.
plotly_clean_trace_names <- function(fig) {
  fig <- plotly::plotly_build(fig)
  fig$x$data <- lapply(fig$x$data, function(tr) {
    if (!is.null(tr$name) && is.character(tr$name)) {
      tr$name <- sub("^\\((.*),\\s*\\d+\\)$", "\\1", tr$name)
    }
    tr
  })
  fig
}

plotly_area_hover_points <- function(fig) {
  built <- plotly::plotly_build(fig)
  filled <- vapply(built$x$data, function(tr) {
    identical(tr$type, "scatter") &&
      !is.null(tr$fill) && !identical(tr$fill, "none")
  }, logical(1))
  if (any(filled)) {
    fig <- plotly::style(fig, hoveron = "points", traces = which(filled))
  }
  fig
}

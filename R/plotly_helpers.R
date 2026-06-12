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

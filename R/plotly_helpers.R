# Shared Plotly rendering helpers for Shiny chart outputs.

dashboard_ggplotly <- function(p, dark, tooltip, hovermode = "x", margin = NULL) {
  fig <- plotly::ggplotly(p, tooltip = tooltip)
  fig <- plotly_layout(fig, dark = dark, hovermode = hovermode)

  if (!is.null(margin)) {
    fig <- plotly::layout(fig, margin = margin)
  }

  fig
}

# Shared ggplot and Plotly theme helpers.

theme_afford <- function(dark = FALSE) {
  axis_col <- if (dark) "#E3EBF4" else "#374151"
  panel_bg <- if (dark) "#111B2E" else "#FFFFFF"
  grid_col <- if (dark) "#253A56" else "#E5EAF1"
  strip_col <- if (dark) "#E8EEF6" else "#182231"

  theme_minimal(base_size = 12.5) +
    theme(
      panel.background = element_rect(fill = panel_bg, color = NA),
      plot.background = element_rect(fill = panel_bg, color = NA),
      panel.grid.major = element_line(color = grid_col, linewidth = 0.35),
      panel.grid.minor = element_blank(),
      legend.position = "bottom",
      legend.box = "vertical",
      legend.key = element_rect(fill = panel_bg, color = NA),
      axis.text.x = element_text(color = axis_col),
      axis.text.y = element_text(color = axis_col),
      axis.title = element_text(color = axis_col),
      legend.text = element_text(color = axis_col),
      plot.title = element_text(color = strip_col, face = "bold"),
      plot.subtitle = element_text(color = axis_col),
      plot.margin = margin(8, 10, 8, 10),
      strip.text = element_text(face = "bold", color = strip_col)
    )
}

# Shared Plotly modebar policy (UX-07): the same toolbar on every chart. Drop
# the Plotly logo and the selection/auto-scale/spike tools that do nothing
# useful on these static time-series and bar charts, while keeping zoom, pan,
# reset and PNG download for exploration. Interactive charts that need a
# stricter bar (e.g. the click-to-filter score chart) re-`config()` on top.
dashboard_modebar_buttons_removed <- function() {
  c("select2d", "lasso2d", "autoScale2d",
    "hoverClosestCartesian", "hoverCompareCartesian", "toggleSpikelines")
}

# Conditional date-axis spec (UX-06): pick tick density and label format from
# the span the data actually covers, so a few-year monthly series is not forced
# onto the five-year/year-label ticks tuned for multi-decade series. Returns a
# list(tickformat, dtick) for use as `date_axis` in plotly_layout(); callers
# that omit it keep the long-history default (year labels every five years).
# Note: for ggplotly charts the ggplot scale_x_date() controls the rendered
# tick text (Plotly emits explicit array ticks); this spec sets the Plotly-level
# x-axis format used by native plot_ly date axes and date hover formatting.
date_axis_config <- function(dates) {
  dates <- suppressWarnings(as.Date(dates))
  dates <- dates[!is.na(dates)]
  if (length(dates) < 2) {
    return(list(tickformat = "%Y", dtick = "M60"))
  }
  span_years <- as.numeric(max(dates) - min(dates)) / 365.25
  if (span_years <= 3) {
    list(tickformat = "%b %Y", dtick = "M3")
  } else if (span_years <= 12) {
    list(tickformat = "%Y", dtick = "M12")
  } else {
    list(tickformat = "%Y", dtick = "M60")
  }
}

plotly_layout <- function(p, dark = FALSE, hovermode = "x",
                          force_markers = TRUE,
                          disable_hovertemplate = TRUE,
                          date_axis = NULL) {
  bg <- if (dark) "#111B2E" else "#FFFFFF"
  fg <- if (dark) "#E3EBF4" else "#374151"
  grid <- if (dark) "#253A56" else "#E5EAF1"

  # Default to the long-history year ticks; an explicit date_axis spec (e.g.
  # from date_axis_config()) overrides format/density for shorter series.
  tickformat <- if (is.null(date_axis)) "%Y" else date_axis$tickformat
  dtick <- if (is.null(date_axis)) "M60" else date_axis$dtick
  xax <- list(gridcolor = grid, title = "",
              tickformat = tickformat, dtick = dtick, tickangle = 0)
  yax <- list(gridcolor = grid)

  layout_args <- list(
    p,
    paper_bgcolor = bg,
    plot_bgcolor = bg,
    font = list(color = fg),
    legend = list(orientation = "h", y = -0.14, xanchor = "center", x = 0.5),
    margin = list(l = 54, r = 22, t = 34, b = 56),
    autosize = TRUE,
    hovermode = hovermode,
    xaxis = xax,
    yaxis = yax
  )
  for (i in 2:9) {
    layout_args[[paste0("xaxis", i)]] <- xax
    layout_args[[paste0("yaxis", i)]] <- yax
  }

  result <- do.call(plotly::layout, layout_args)

  if (disable_hovertemplate && !is.null(result$x$data) && length(result$x$data) > 0) {
    for (i in seq_along(result$x$data)) {
      result$x$data[[i]]$hovertemplate <- NULL
    }
  }

  result <- result %>% plotly::config(
    responsive = TRUE,
    displaylogo = FALSE,
    modeBarButtonsToRemove = dashboard_modebar_buttons_removed()
  )
  result$x$layout$hovermode <- hovermode
  result
}

plot_ts <- function(df, x = "date", y = "value", colour = "series",
                    dark = FALSE, y_label = NULL) {
  ggplot(df, aes(x = .data[[x]], y = .data[[y]], color = .data[[colour]])) +
    geom_line(linewidth = 1, alpha = 0.9) +
    scale_x_date(date_labels = "%Y", date_breaks = "5 years") +
    scale_y_continuous(labels = label_number(big.mark = ",", accuracy = 0.1)) +
    labs(x = NULL, y = y_label, color = NULL) +
    theme_afford(dark)
}

plot_bar <- function(df, x, y, fill = NULL, dark = FALSE,
                     position = "dodge", y_label = NULL,
                     coord_flip = FALSE) {
  mapping <- if (!is.null(fill)) {
    aes(x = .data[[x]], y = .data[[y]], fill = .data[[fill]])
  } else {
    aes(x = .data[[x]], y = .data[[y]])
  }
  p <- ggplot(df, mapping) +
    geom_col(position = position, alpha = 0.85, width = 0.7) +
    scale_y_continuous(labels = label_number(big.mark = ",", accuracy = 0.1)) +
    labs(x = NULL, y = y_label, fill = NULL) +
    theme_afford(dark)
  if (coord_flip) p <- p + coord_flip()
  p
}

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

plotly_layout <- function(p, dark = FALSE, hovermode = "x",
                          force_markers = TRUE,
                          disable_hovertemplate = TRUE) {
  bg <- if (dark) "#111B2E" else "#FFFFFF"
  fg <- if (dark) "#E3EBF4" else "#374151"
  grid <- if (dark) "#253A56" else "#E5EAF1"

  xax <- list(gridcolor = grid, title = "",
              tickformat = "%Y", dtick = "M60", tickangle = 0)
  yax <- list(gridcolor = grid)

  layout_args <- list(
    p,
    paper_bgcolor = bg,
    plot_bgcolor = bg,
    font = list(color = fg),
    legend = list(orientation = "h", y = -0.14, xanchor = "center", x = 0.5),
    margin = list(l = 54, r = 22, t = 34, b = 56),
    autosize = TRUE,
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

  result %>% plotly::config(responsive = TRUE)
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

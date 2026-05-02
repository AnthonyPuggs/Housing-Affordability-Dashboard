# Overview page module.

overview_cost_pressure_indicators <- c(
  "Rental Affordability Index",
  "Mortgage Serviceability Index",
  "Price-to-Income Ratio"
)
overview_cost_pressure_colours <- stats::setNames(
  cost_pressure_palette(indicator_chart_label(overview_cost_pressure_indicators)),
  indicator_chart_label(overview_cost_pressure_indicators)
)

overviewPageUI <- function(id) {
  ns <- NS(id)

  nav_panel(
    "Overview",
    div(
      class = "d-flex justify-content-between align-items-start mb-3 px-2",
      div(
        tags$h3("Housing Affordability", class = "mb-1",
                style = "font-weight: 700;"),
        tags$p("Analysing the state of the Australian market",
               style = "color: var(--app-muted); margin-bottom: 0;")
      )
    ),
    layout_column_wrap(
      width = 1/4,
      fill = FALSE,
      value_box(
        title = "National Median Price",
        value = textOutput(ns("vb_nat_price")),
        p(class = "kpi-subtitle", textOutput(ns("vb_nat_price_date"))),
        uiOutput(ns("vb_nat_price_change")),
        theme = value_box_theme(bg = "#0E5A8A", fg = "#fff")
      ),
      value_box(
        title = "Sydney Median Price",
        value = textOutput(ns("vb_syd_price")),
        p(class = "kpi-subtitle", textOutput(ns("vb_syd_price_date"))),
        uiOutput(ns("vb_syd_price_change")),
        theme = value_box_theme(bg = "#3B4C7A", fg = "#fff")
      ),
      value_box(
        title = "Modelled Serviceability",
        value = textOutput(ns("vb_service")),
        p(class = "kpi-subtitle", "Stylised mortgage scenario"),
        uiOutput(ns("vb_service_change")),
        theme = value_box_theme(bg = "#17415F", fg = "#fff")
      ),
      value_box(
        title = "Rental Affordability",
        value = textOutput(ns("vb_rental")),
        p(class = "kpi-subtitle", textOutput(ns("vb_rental_date"))),
        uiOutput(ns("vb_rental_change")),
        theme = value_box_theme(bg = "#984ea3", fg = "#fff")
      )
    ),
    layout_column_wrap(
      width = 1,
      fill = FALSE,
      card(
        fill = FALSE,
        card_header("Capital City Median House Prices"),
        uiOutput(ns("overview_price_subtitle")),
        card_body(plotlyOutput(ns("overview_median_prices"), height = "480px")),
        card_footer(
          sliderInput(ns("overview_price_dates"), "Date Range",
                      min = min(median_prices_combined$date, na.rm = TRUE),
                      max = max(median_prices_combined$date, na.rm = TRUE),
                      value = c(as.Date("2010-01-01"),
                                max(median_prices_combined$date, na.rm = TRUE)),
                      width = "100%", timeFormat = "%b %Y"),
          radioButtons(ns("overview_price_transform"), NULL,
                       choices = c("Nominal ($)" = "nominal",
                                   "Index (start = 100)" = "index"),
                       selected = "nominal", inline = TRUE)
        )
      )
    ),
    layout_column_wrap(
      width = 1,
      fill = FALSE,
      card(
        fill = FALSE,
        card_header("Affordability Indices"),
        source_note("Cost-pressure indexes; higher = less affordable. Rent uses ABS CPI rents/WPI, mortgage uses price\u00d7rate/WPI, deposit uses price/income, and price-to-income uses national dwelling prices/WPI."),
        card_body(plotlyOutput(ns("overview_afford_change"), height = "380px"))
      )
    )
  )
}

overviewPageServer <- function(id, is_dark) {
  moduleServer(id, function(input, output, session) {
    output$vb_nat_price <- renderText({
      v <- latest_val(national_mean_price, "city", "National Avg")
      if (is.na(v)) "N/A" else fmt_dollar_k(v * 1000)
    })
    output$vb_nat_price_date <- renderText({
      latest_date(national_mean_price, "city", "National Avg")
    })
    output$vb_nat_price_change <- renderUI({
      ch <- latest_change(national_mean_price, "city", "National Avg",
                          periods_back = 4, period_label = "YoY",
                          change_type = "relative_pct")
      css_class <- kpi_change_class(ch$change, favourable = "decrease")
      tags$p(class = paste("kpi-subtitle", css_class), ch$label)
    })

    output$vb_syd_price <- renderText({
      v <- latest_val(median_house_prices, "city", "Sydney")
      if (is.na(v)) "N/A" else fmt_dollar_k(v * 1000)
    })
    output$vb_syd_price_date <- renderText({
      latest_date(median_house_prices, "city", "Sydney")
    })
    output$vb_syd_price_change <- renderUI({
      ch <- latest_change(median_house_prices, "city", "Sydney",
                          periods_back = 4, period_label = "YoY",
                          change_type = "relative_pct")
      css_class <- kpi_change_class(ch$change, favourable = "decrease")
      tags$p(class = paste("kpi-subtitle", css_class), ch$label)
    })

    output$vb_service <- renderText({
      if (nrow(serviceability_ts) == 0) return("N/A")
      v <- serviceability_ts %>%
        filter(!is.na(serviceability_pct)) %>%
        arrange(desc(date)) %>%
        pull(serviceability_pct) %>%
        first()
      fmt_pct(v, 0.1)
    })
    output$vb_service_change <- renderUI({
      if (nrow(serviceability_ts) < 5) return(tags$p(class = "kpi-subtitle", ""))
      d <- serviceability_ts %>%
        filter(!is.na(serviceability_pct)) %>%
        arrange(desc(date))
      current <- d$serviceability_pct[1]
      previous <- d$serviceability_pct[5]
      if (is.na(previous) || previous == 0) return(tags$p(class = "kpi-subtitle", ""))
      diff_val <- current - previous
      direction <- if (diff_val >= 0) "\u2191" else "\u2193"
      label <- paste0(direction, " ", sprintf("%+.1f pp", diff_val), " YoY")
      css_class <- kpi_change_class(diff_val, favourable = "decrease")
      tags$p(class = paste("kpi-subtitle", css_class), label)
    })

    output$vb_rental <- renderText({
      v <- latest_val(afford_idx, "indicator", "Rental Affordability Index")
      fmt_index(v)
    })
    output$vb_rental_date <- renderText({
      latest_date(afford_idx, "indicator", "Rental Affordability Index")
    })
    output$vb_rental_change <- renderUI({
      ch <- latest_change(afford_idx, "indicator", "Rental Affordability Index",
                          periods_back = 4, period_label = "YoY",
                          change_type = "relative_pct")
      css_class <- kpi_change_class(ch$change, favourable = "decrease")
      tags$p(class = paste("kpi-subtitle", css_class), ch$label)
    })

    output$overview_price_subtitle <- renderUI({
      txt <- if (identical(input$overview_price_transform, "index")) {
        "Indexed to 100 at start of selected date range"
      } else {
        "Nominal values (in thousands AUD)"
      }
      tags$p(txt, class = "px-3",
             style = "color: var(--app-muted); font-size: 0.85rem; margin-bottom: 0;")
    })

    output$overview_median_prices <- renderPlotly({
      show_cities <- c("Sydney", "Melbourne", "Brisbane", "Adelaide",
                       "Perth", "Hobart", "Darwin", "Canberra", "National Avg")
      d <- median_prices_combined %>%
        filter(city %in% show_cities,
               date >= input$overview_price_dates[1],
               date <= input$overview_price_dates[2])

      validate(need(nrow(d) > 0, "No median house price data available."))

      is_index <- identical(input$overview_price_transform, "index")
      d <- overview_price_series_transform(
        d,
        input$overview_price_transform
      )

      price_colours <- c(
        "Sydney" = "#2196F3", "Melbourne" = "#7B1FA2", "Brisbane" = "#FF5722",
        "Adelaide" = "#984ea3", "Perth" = "#ff7f00", "Hobart" = "#a65628",
        "Darwin" = "#f781bf", "Canberra" = "#999999", "National Avg" = "#4CAF50"
      )

      p <- build_overview_median_prices_plot(
        d,
        is_index = is_index,
        price_colours = price_colours,
        show_cities = show_cities,
        dark = is_dark()
      )

      label_data <- d %>%
        group_by(city) %>%
        filter(date == max(date)) %>%
        ungroup()

      y_range <- range(d$plot_value, na.rm = TRUE)
      min_gap <- diff(y_range) * 0.045
      label_data$y_repelled <- repel_labels(label_data$plot_value, min_gap)

      fig <- dashboard_ggplotly(p, dark = is_dark(),
                                tooltip = c("x", "y", "color"))

      annotations <- lapply(seq_len(nrow(label_data)), function(i) {
        list(
          x = 1.01, xref = "paper", xanchor = "left",
          y = label_data$y_repelled[i], yref = "y",
          text = label_data$city[i],
          font = list(
            size = 13,
            color = price_colours[label_data$city[i]]
          ),
          showarrow = FALSE
        )
      })

      fig %>%
        plotly::layout(
          annotations = annotations,
          margin = list(r = 100)
        )
    }) %>%
      bindCache(input$overview_price_dates, input$overview_price_transform,
                is_dark())

    output$overview_afford_change <- renderPlotly({
      d <- afford_idx %>%
        filter(indicator %in% overview_cost_pressure_indicators) %>%
        mutate(indicator_label = indicator_chart_label(indicator))

      validate(need(nrow(d) > 0, "No affordability index data available."))

      p <- build_overview_affordability_plot(
        d,
        colours = overview_cost_pressure_colours,
        dark = is_dark()
      )

      dashboard_ggplotly(p, dark = is_dark(), tooltip = c("x", "y", "color"))
    }) %>%
      bindCache(is_dark())
  })
}

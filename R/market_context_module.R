# Market Context page module.

marketContextPageUI <- function(id) {
  ns <- NS(id)

  nav_panel(
    "Market Context",
    div(
      class = "d-flex justify-content-between align-items-start mb-3 px-2",
      div(
        tags$h3("Labour & Demographics", class = "mb-1",
                style = "font-weight: 700;"),
        tags$p("Analysing the state of the Australian market",
               style = "color: var(--app-muted); margin-bottom: 0;")
      )
    ),
    layout_column_wrap(
      width = 1/3,
      fill = FALSE,
      value_box(
        title = "Unemployment Rate",
        value = textOutput(ns("vb_unemp")),
        p(class = "kpi-subtitle", "Trend estimate"),
        uiOutput(ns("vb_unemp_change")),
        theme = value_box_theme(bg = "#0E5A8A", fg = "#fff")
      ),
      value_box(
        title = "Net Overseas Migration",
        value = textOutput(ns("vb_nom")),
        p(class = "kpi-subtitle", "Annual estimate"),
        uiOutput(ns("vb_nom_change")),
        theme = value_box_theme(bg = "#3B4C7A", fg = "#fff")
      ),
      value_box(
        title = "Participation Rate",
        value = textOutput(ns("vb_participation")),
        p(class = "kpi-subtitle", "Trend estimate"),
        uiOutput(ns("vb_participation_change")),
        theme = value_box_theme(bg = "#17415F", fg = "#fff")
      )
    ),
    sliderInput(ns("context_dates"), "Date Range",
                min = as.Date("1990-01-01"),
                max = Sys.Date(),
                value = c(as.Date("2000-01-01"), Sys.Date()),
                width = "100%", timeFormat = "%b %Y"),
    layout_column_wrap(
      width = 1,
      fill = FALSE,
      card(
        fill = FALSE,
        card_header("Interest Rates on Residential Mortgages"),
        source_note("RBA mortgage rates are market-rate inputs, not lender assessment outcomes or household serviceability approvals."),
        card_body(plotlyOutput(ns("context_rates"), height = "380px"))
      )
    ),
    layout_column_wrap(
      width = 1/2,
      card(
        fill = FALSE,
        card_header("Labour Market Spare Capacity"),
        source_note("ABS labour force rates. KPI changes are percentage-point changes, not relative percentage changes."),
        card_body(plotlyOutput(ns("context_labour"), height = "380px"))
      ),
      card(
        fill = FALSE,
        card_header("Population Demand"),
        source_note("ABS population data. Net overseas migration is shown as an annualised flow in thousands."),
        card_body(plotlyOutput(ns("context_pop"), height = "380px"))
      )
    )
  )
}

marketContextPageServer <- function(id, is_dark) {
  moduleServer(id, function(input, output, session) {
    output$vb_unemp <- renderText({
      v <- latest_val(abs_ts, "series", "Unemployment Rate")
      fmt_pct(v, 0.1)
    })
    output$vb_unemp_change <- renderUI({
      ch <- latest_change(abs_ts, "series", "Unemployment Rate",
                          periods_back = 12, period_label = "YoY",
                          change_type = "percentage_points")
      diff_val <- ch$change
      # For unemployment, down is good (green), up is bad (red).
      css_class <- if (!is.na(diff_val) && diff_val <= 0) "kpi-change-up" else "kpi-change-down"
      tags$p(class = paste("kpi-subtitle", css_class), ch$label)
    })

    output$vb_nom <- renderText({
      d <- supply_demand %>%
        filter(str_detect(series, "Net Overseas Migration"),
               !is.na(value)) %>%
        arrange(desc(date))
      if (nrow(d) < 4) return("N/A")
      annual <- sum(d$value[1:4], na.rm = TRUE)
      paste0(round(annual), "k")
    })
    output$vb_nom_change <- renderUI({
      d <- supply_demand %>%
        filter(str_detect(series, "Net Overseas Migration"),
               !is.na(value)) %>%
        arrange(desc(date))
      if (nrow(d) < 8) return(tags$p(class = "kpi-subtitle", ""))
      current_annual <- sum(d$value[1:4], na.rm = TRUE)
      previous_annual <- sum(d$value[5:8], na.rm = TRUE)
      if (previous_annual == 0) return(tags$p(class = "kpi-subtitle", ""))
      pct <- (current_annual / previous_annual - 1) * 100
      direction <- if (pct >= 0) "\u2191" else "\u2193"
      label <- paste0(direction, " ", sprintf("%+.0f%%", pct), " YoY")
      css_class <- if (pct >= 0) "kpi-change-up" else "kpi-change-down"
      tags$p(class = paste("kpi-subtitle", css_class), label)
    })

    output$vb_participation <- renderText({
      v <- latest_val(abs_ts, "series", "Participation Rate")
      fmt_pct(v, 0.1)
    })
    output$vb_participation_change <- renderUI({
      ch <- latest_change(abs_ts, "series", "Participation Rate",
                          periods_back = 12, period_label = "YoY",
                          change_type = "percentage_points")
      diff_val <- ch$change
      css_class <- if (!is.na(diff_val) && diff_val >= 0) "kpi-change-up" else "kpi-change-down"
      lbl <- if (is.na(diff_val)) "" else if (abs(diff_val) < 0.3) "\u2192 Stable" else ch$label
      tags$p(class = paste("kpi-subtitle", css_class), lbl)
    })

    output$context_rates <- renderPlotly({
      d <- bind_rows(
        rba_cash_rate %>% mutate(series = "RBA Cash Rate"),
        rba_mortgage_var %>% mutate(series = "Owner-occ Variable (Discounted)"),
        rba_mortgage_fixed %>% mutate(series = "Owner-occ 3yr Fixed"),
        rba_investor_var %>% mutate(series = "Investor Variable (Discounted)"),
        rba_investor_fixed %>% mutate(series = "Investor 3yr Fixed")
      ) %>%
        distinct(date, series, .keep_all = TRUE) %>%
        filter(date >= input$context_dates[1],
               date <= input$context_dates[2])

      validate(need(nrow(d) > 0, "No rate data available."))

      rate_colours <- c(
        "RBA Cash Rate" = "#1B5E20",
        "Owner-occ Variable (Discounted)" = "#2196F3",
        "Owner-occ 3yr Fixed" = "#1565C0",
        "Investor Variable (Discounted)" = "#FF9800",
        "Investor 3yr Fixed" = "#E65100"
      )

      p <- ggplot(d, aes(x = date, y = value, color = series)) +
        geom_line(linewidth = 1, alpha = 0.9) +
        scale_color_manual(values = rate_colours) +
        scale_x_date(date_labels = "%Y", date_breaks = "5 years") +
        scale_y_continuous(labels = label_number(big.mark = ",", accuracy = 0.1)) +
        labs(x = NULL, y = "%", color = NULL) +
        theme_afford(is_dark())

      dashboard_ggplotly(p, dark = is_dark(), tooltip = c("x", "y", "color"))
    }) %>%
      bindCache(input$context_dates, is_dark())

    output$context_labour <- renderPlotly({
      d <- abs_ts %>%
        filter(series %in% c("Unemployment Rate", "Underemployment Rate",
                              "Labour Underutilisation Rate"),
               date >= input$context_dates[1],
               date <= input$context_dates[2]) %>%
        mutate(series = factor(series,
          levels = c("Labour Underutilisation Rate", "Underemployment Rate",
                     "Unemployment Rate")))
      validate(need(nrow(d) > 0, "No labour market data available."))

      labour_fills <- c("Unemployment Rate" = "#2196F3",
                        "Underemployment Rate" = "#AB47BC",
                        "Labour Underutilisation Rate" = "#78909C")

      p <- ggplot(d, aes(x = date, y = value, fill = series)) +
        geom_area(alpha = 0.6, linewidth = 0.5, colour = "white") +
        scale_fill_manual(values = labour_fills) +
        scale_x_date(date_labels = "%Y", date_breaks = "5 years") +
        scale_y_continuous(labels = label_percent(scale = 1, accuracy = 0.1)) +
        labs(x = NULL, y = NULL, fill = NULL) +
        theme_afford(is_dark())

      dashboard_ggplotly(p, dark = is_dark(), tooltip = c("x", "y", "fill"))
    }) %>%
      bindCache(input$context_dates, is_dark())

    output$context_pop <- renderPlotly({
      d <- supply_demand %>%
        filter(str_detect(series, "Net Overseas Migration"),
               date >= input$context_dates[1],
               date <= input$context_dates[2])
      validate(need(nrow(d) > 0,
        "Run pipeline/05_driver.R to fetch population data (ABS 3101.0)"))

      p <- ggplot(d, aes(x = date, y = value)) +
        geom_col(fill = "#29B6F6", alpha = 0.85, width = 60) +
        scale_x_date(date_labels = "%Y", date_breaks = "5 years") +
        scale_y_continuous(labels = label_number(big.mark = ",", accuracy = 1)) +
        labs(x = NULL, y = "Thousands") +
        theme_afford(is_dark())

      dashboard_ggplotly(p, dark = is_dark(), tooltip = c("x", "y"))
    }) %>%
      bindCache(input$context_dates, is_dark())
  })
}

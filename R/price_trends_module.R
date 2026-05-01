# Price Trends page module.

priceTrendsPageUI <- function(id) {
  ns <- NS(id)

  nav_panel(
    "Price Trends",
    navset_card_tab(
      nav_panel(
        "Dwelling Price Index",
        layout_sidebar(
          sidebar = sidebar(
            width = 300, open = "desktop",
            selectizeInput(ns("price_cities"), "Capital Cities",
                           choices = rppi_cities,
                           selected = c("Sydney", "Melbourne", "Brisbane",
                                        "Weighted average of eight capital cities"),
                           multiple = TRUE),
            radioButtons(ns("price_dwelling"), "Dwelling Type",
                         choices = c("Total", "Houses", "Units"),
                         selected = "Total"),
            dateRangeInput(ns("price_dates"), "Date Range",
                           start = as.Date("2003-01-01"),
                           end = Sys.Date(),
                           min = as.Date("1990-01-01")),
            radioButtons(ns("price_transform"), "Transform",
                         choices = c("Levels" = "levels",
                                     "YoY %" = "yoy",
                                     "Index (start=100)" = "index"),
                         selected = "levels")
          ),
          card(
            card_header("Dwelling Price Index by Capital City"),
            source_note("ABS dwelling price data. Price indexes describe market price movements, not household affordability or borrowing capacity."),
            card_body(div(class = "chart-wide", plotlyOutput(ns("price_chart"), height = "100%", width = "100%")))
          )
        )
      ),
      nav_panel(
        "Rent CPI",
        layout_sidebar(
          sidebar = sidebar(
            width = 300, open = "desktop",
            selectizeInput(ns("rent_cpi_cities"), "Capital Cities",
                           choices = rent_cpi_cities,
                           selected = rent_cpi_cities,
                           multiple = TRUE),
            radioButtons(ns("rent_cpi_datatype"), "Data Type",
                         choices = c("Index numbers" = "index",
                                     "Annual change (%)" = "yoy",
                                     "Quarterly change (%)" = "qoq"),
                         selected = "index")
          ),
          card(
            card_header("Rent Consumer Price Index (CPI) by Greater Capital City"),
            source_note("ABS CPI rents are price indexes. They measure rental price movements, not the housing cost burden of lower-income renters."),
            card_body(div(class = "chart-wide", plotlyOutput(ns("rent_cpi_chart"), height = "100%", width = "100%"))),
            card_footer(
              sliderInput(ns("rent_cpi_dates"), "Date Range",
                          min = min(rent_cpi_combined$date, na.rm = TRUE),
                          max = max(rent_cpi_combined$date, na.rm = TRUE),
                          value = c(as.Date("2012-01-01"),
                                    max(rent_cpi_combined$date, na.rm = TRUE)),
                          width = "100%", timeFormat = "%Y Q%q"),
              tags$p("Source: ABS \u2014 Consumer Price Index, Australia",
                     class = "text-muted small mt-2 mb-0")
            )
          )
        )
      )
    )
  )
}

priceTrendsPageServer <- function(id, is_dark) {
  moduleServer(id, function(input, output, session) {
    price_data <- reactive({
      req(input$price_cities)
      d <- rppi_combined %>%
        filter(city %in% input$price_cities,
               dwelling_type == input$price_dwelling,
               date >= input$price_dates[1],
               date <= input$price_dates[2])

      if (nrow(d) == 0) return(d)

      if (input$price_transform == "yoy") {
        d <- d %>%
          group_by(city) %>%
          arrange(date) %>%
          mutate(value = 100 * (value / lag(value, 4) - 1)) %>%
          filter(!is.na(value)) %>%
          ungroup()
      } else if (input$price_transform == "index") {
        d <- d %>%
          group_by(city) %>%
          arrange(date) %>%
          mutate(value = 100 * value / first(value)) %>%
          ungroup()
      }
      d
    })

    output$price_chart <- renderPlotly({
      d <- price_data()
      validate(need(nrow(d) > 0,
        "No data for selected cities/dwelling type. Try 'Total' or check dates."))

      y_lab <- switch(input$price_transform,
                      levels = "Index", yoy = "YoY %", index = "Index (start=100)")

      p <- ggplot(d, aes(x = date, y = value, color = city)) +
        geom_line(linewidth = 1, alpha = 0.9) +
        scale_color_manual(values = city_colours, na.value = "grey50") +
        scale_x_date(date_labels = "%Y", date_breaks = "3 years") +
        scale_y_continuous(labels = label_number(big.mark = ",", accuracy = 0.1)) +
        labs(x = NULL, y = y_lab, color = NULL) +
        theme_afford(is_dark())

      dashboard_ggplotly(p, dark = is_dark(), tooltip = c("x", "y", "color"))
    }) %>%
      bindCache(input$price_cities, input$price_dwelling, input$price_dates,
                input$price_transform, is_dark())

    rent_cpi_data <- reactive({
      req(input$rent_cpi_cities)
      d <- rent_cpi_combined %>%
        filter(city %in% input$rent_cpi_cities,
               date >= input$rent_cpi_dates[1],
               date <= input$rent_cpi_dates[2])
      if (nrow(d) == 0) return(d)

      if (input$rent_cpi_datatype == "yoy") {
        d <- d %>%
          group_by(city) %>%
          arrange(date) %>%
          mutate(value = 100 * (value / lag(value, 4) - 1)) %>%
          filter(!is.na(value)) %>%
          ungroup()
      } else if (input$rent_cpi_datatype == "qoq") {
        d <- d %>%
          group_by(city) %>%
          arrange(date) %>%
          mutate(value = 100 * (value / lag(value, 1) - 1)) %>%
          filter(!is.na(value)) %>%
          ungroup()
      }
      d
    })

    output$rent_cpi_chart <- renderPlotly({
      d <- rent_cpi_data()
      validate(need(nrow(d) > 0, "No CPI Rents data for selected cities/dates."))

      y_lab <- switch(input$rent_cpi_datatype,
                      index = "Index",
                      yoy = "Annual change (%)",
                      qoq = "Quarterly change (%)")

      datatype_label <- switch(input$rent_cpi_datatype,
                               index = "index numbers",
                               yoy = "annual change (%)",
                               qoq = "quarterly change (%)")

      date_range_label <- paste(
        format(min(d$date), "%b %Y"), "to", format(max(d$date), "%b %Y"))

      p <- ggplot(d, aes(x = date, y = value, color = city)) +
        geom_line(linewidth = 1, alpha = 0.9) +
        scale_color_manual(values = city_colours, na.value = "grey50") +
        scale_x_date(date_labels = "%Y", date_breaks = "3 years") +
        scale_y_continuous(labels = label_number(big.mark = ",", accuracy = 0.1)) +
        labs(x = NULL, y = y_lab, color = NULL,
             title = paste0("Rent CPI, ", datatype_label,
                            ", by greater capital city, ", date_range_label)) +
        theme_afford(is_dark())

      dashboard_ggplotly(p, dark = is_dark(), tooltip = c("x", "y", "color"))
    }) %>%
      bindCache(input$rent_cpi_cities, input$rent_cpi_dates,
                input$rent_cpi_datatype, is_dark())
  })
}

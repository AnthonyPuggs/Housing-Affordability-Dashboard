# Price Trends page module.

priceTrendsPageUI <- function(id) {
  ns <- NS(id)

  nav_panel(
    "Price Trends",
    policy_page_header(
      "Price Trends",
      "Capital-city dwelling price indexes and ABS rent CPI movements."
    ),
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
          policy_chart_card(
            "Dwelling Price Index by Capital City",
            note = "ABS dwelling price data. Price indexes describe market price movements, not household affordability or borrowing capacity.",
            div(class = "chart-wide",
                plotlyOutput(ns("price_chart"), height = "100%", width = "100%"))
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
          policy_chart_card(
            "Rent Consumer Price Index (CPI) by Greater Capital City",
            note = "ABS CPI rents are price indexes. They measure rental price movements, not the housing cost burden of lower-income renters.",
            div(class = "chart-wide",
                plotlyOutput(ns("rent_cpi_chart"), height = "100%", width = "100%")),
            footer = card_footer(
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
      price_series_transform(d, input$price_transform)
    })

    output$price_chart <- renderPlotly({
      d <- price_data()
      validate(need(nrow(d) > 0,
        "No data for selected cities/dwelling type. Try 'Total' or check dates."))

      p <- build_dwelling_price_plot(d, input$price_transform, is_dark())

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
      rent_cpi_series_transform(d, input$rent_cpi_datatype)
    })

    output$rent_cpi_chart <- renderPlotly({
      d <- rent_cpi_data()
      validate(need(nrow(d) > 0, "No CPI Rents data for selected cities/dates."))

      p <- build_rent_cpi_plot(d, input$rent_cpi_datatype, is_dark())

      dashboard_ggplotly(p, dark = is_dark(), tooltip = c("x", "y", "color"))
    }) %>%
      bindCache(input$rent_cpi_cities, input$rent_cpi_dates,
                input$rent_cpi_datatype, is_dark())
  })
}

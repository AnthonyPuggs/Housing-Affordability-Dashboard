# Price Trends page module.

rent_cpi_default_national_dates <- function() {
  start_date <- max(as.Date("2012-01-01"), rent_cpi_national_range[1])
  as.Date(c(start_date, rent_cpi_national_range[2]))
}

rent_cpi_view_range <- function(view) {
  if (identical(view, "city")) {
    return(rent_cpi_city_range)
  }
  rent_cpi_national_range
}

rent_cpi_view_default_dates <- function(view) {
  if (identical(view, "city")) {
    return(rent_cpi_city_range)
  }
  rent_cpi_default_national_dates()
}

# Geography choices and defaults per dwelling-type view. "Total" shows
# whole-of-state/territory mean-price indexes (ABS 6432.0 Table 1) under
# their true geography; "Houses"/"Units" show genuine capital-city median
# transfer prices (Table 2). See review STAT-01.
price_geography_choices <- function(dwelling_type) {
  if (identical(dwelling_type, "Total")) rppi_states else rppi_cities
}

price_geography_default <- function(dwelling_type) {
  if (identical(dwelling_type, "Total")) {
    intersect(c("New South Wales", "Victoria", "Queensland", "Australia"),
              rppi_states)
  } else {
    intersect(c("Sydney", "Melbourne", "Brisbane"), rppi_cities)
  }
}

price_geography_label <- function(dwelling_type) {
  if (identical(dwelling_type, "Total")) {
    "States & Territories"
  } else {
    "Capital Cities"
  }
}

price_value_label <- function(dwelling_type, transform) {
  if (identical(transform, "yoy")) {
    return("YoY %")
  }
  if (identical(transform, "index")) {
    return("Index (common start = 100)")
  }
  if (identical(dwelling_type, "Total")) {
    "State mean price index (start = 100)"
  } else {
    "Median transfer price ($'000)"
  }
}

priceTrendsPageUI <- function(id) {
  ns <- NS(id)

  nav_panel(
    "Price Trends",
    policy_page_header(
      "Price Trends",
      "State mean dwelling price indexes, capital-city median prices and ABS rent CPI movements."
    ),
    navset_card_tab(
      nav_panel(
        "Dwelling Price Index",
        layout_sidebar(
          sidebar = sidebar(
            width = 300, open = "desktop",
            radioButtons(ns("price_dwelling"), "Dwelling Type",
                         choices = c(
                           "All dwellings - state means" = "Total",
                           "Houses - city medians" = "Houses",
                           "Units - city medians" = "Units"
                         ),
                         selected = "Total"),
            selectizeInput(ns("price_cities"), "States & Territories",
                           choices = rppi_states,
                           selected = price_geography_default("Total"),
                           multiple = TRUE),
            dateRangeInput(ns("price_dates"), "Date Range",
                           start = as.Date("2003-01-01"),
                           end = Sys.Date(),
                           min = as.Date("1990-01-01")),
            radioButtons(ns("price_transform"), "Transform",
                         choices = c("Levels" = "levels",
                                     "YoY %" = "yoy",
                                     "Index (common start = 100)" = "index"),
                         selected = "levels"),
            source_note("The all-dwellings view shows whole-of-state mean-price indexes (ABS 6432.0 Table 1); capital-city growth can differ from its state. The houses/units views show capital-city median transfer prices (Table 2).")
          ),
          policy_chart_card(
            "Dwelling Prices by Geography",
            note = "ABS dwelling price data. Price series describe market price movements, not household affordability or borrowing capacity. Index transform rebases every selected series at the first quarter they all cover.",
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
            radioButtons(ns("rent_cpi_view"), "Rent CPI View",
                         choices = c("Eight-capital-city average (long run)" = "national",
                                     "Capital-city comparison" = "city"),
                         selected = "national"),
            conditionalPanel(
              condition = "input.rent_cpi_view == 'city'",
              ns = ns,
              selectizeInput(ns("rent_cpi_cities"), "Capital Cities",
                             choices = rent_cpi_city_cities,
                             selected = rent_cpi_default_cities,
                             multiple = TRUE),
              checkboxInput(ns("rent_cpi_include_national"),
                            "Include weighted-average reference",
                            value = FALSE)
            ),
            tags$p(
              "City CPI rent series in the saved data are post-rebase and available only from July 2022. The weighted average of the eight capital cities (rest-of-state areas are not covered) has longer ABS SDMX history.",
              class = "source-note small mb-3"
            ),
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
                          min = rent_cpi_national_range[1],
                          max = rent_cpi_national_range[2],
                          value = rent_cpi_default_national_dates(),
                          width = "100%", timeFormat = "%Y Q%q"),
              tags$p("Source: ABS - Consumer Price Index, Australia",
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
    # Swap geography choices when the dwelling-type view changes: state
    # mean-price indexes vs genuine capital-city medians.
    observeEvent(input$price_dwelling, {
      updateSelectizeInput(
        session,
        "price_cities",
        label = price_geography_label(input$price_dwelling),
        choices = price_geography_choices(input$price_dwelling),
        selected = price_geography_default(input$price_dwelling)
      )
    }, ignoreInit = TRUE)

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
        "No data for the selected geographies/dwelling type. Check the dates or selection."))

      p <- build_dwelling_price_plot(
        d,
        input$price_transform,
        is_dark(),
        value_label = price_value_label(input$price_dwelling,
                                        input$price_transform)
      )

      dashboard_ggplotly(p, dark = is_dark(), tooltip = c("x", "y", "color"))
    }) %>%
      bindCache(input$price_cities, input$price_dwelling, input$price_dates,
                input$price_transform, is_dark())

    observeEvent(input$rent_cpi_view, {
      view <- input$rent_cpi_view
      if (is.null(view) || !view %in% c("national", "city")) {
        view <- "national"
      }

      range <- rent_cpi_view_range(view)
      updateSliderInput(
        session,
        "rent_cpi_dates",
        min = range[1],
        max = range[2],
        value = rent_cpi_view_default_dates(view)
      )
      updateRadioButtons(
        session,
        "rent_cpi_datatype",
        selected = if (identical(view, "city")) "yoy" else "index"
      )
    }, ignoreInit = TRUE)

    rent_cpi_data <- reactive({
      view <- input$rent_cpi_view
      if (is.null(view) || !view %in% c("national", "city")) {
        view <- "national"
      }

      selected_cities <- if (identical(view, "national")) {
        rent_cpi_national_city
      } else {
        req(input$rent_cpi_cities)
        cities <- input$rent_cpi_cities
        if (isTRUE(input$rent_cpi_include_national)) {
          cities <- unique(c(rent_cpi_national_city, cities))
        }
        cities
      }

      req(selected_cities)
      view_range <- rent_cpi_view_range(view)
      date_start <- max(as.Date(input$rent_cpi_dates[1]), view_range[1])
      date_end <- min(as.Date(input$rent_cpi_dates[2]), view_range[2])
      d <- rent_cpi_combined %>%
        filter(city %in% selected_cities,
               date >= date_start,
               date <= date_end)
      if (nrow(d) == 0) return(d)
      rent_cpi_series_transform(d, input$rent_cpi_datatype)
    })

    output$rent_cpi_chart <- renderPlotly({
      d <- rent_cpi_data()
      validate(need(nrow(d) > 0, "No CPI Rents data for selected cities/dates."))

      view <- input$rent_cpi_view
      if (is.null(view) || !view %in% c("national", "city")) {
        view <- "national"
      }
      p <- build_rent_cpi_plot(d, input$rent_cpi_datatype, is_dark(),
                               view = view)

      dashboard_ggplotly(p, dark = is_dark(), tooltip = c("x", "y", "color"))
    }) %>%
      bindCache(input$rent_cpi_cities, input$rent_cpi_dates,
                input$rent_cpi_datatype, input$rent_cpi_view,
                input$rent_cpi_include_national, is_dark())
  })
}

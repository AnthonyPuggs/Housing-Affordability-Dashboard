# Housing Supply page module.

if (!exists("selected_approvals_latest", mode = "function", inherits = TRUE)) {
  contextual_kpi_path <- if (exists("project_path", mode = "function", inherits = TRUE)) {
    project_path("R", "contextual_kpi_helpers.R")
  } else {
    file.path("R", "contextual_kpi_helpers.R")
  }
  if (!file.exists(contextual_kpi_path)) {
    stop("Could not locate R/contextual_kpi_helpers.R for housing supply module.",
         call. = FALSE)
  }
  source(contextual_kpi_path, local = environment())
}

supply_state_choices <- function() {
  supply_data <- if (exists("supply_demand", inherits = TRUE)) {
    supply_demand
  } else {
    NULL
  }
  available_supply_states(supply_data)
}

supply_building_type_choices <- c(
  "Total approvals" = "Total approvals",
  "Houses" = "Houses",
  "Dwellings excluding houses" = "Dwellings excluding houses"
)

supply_sector_choices <- c(
  "Total sectors" = "Total sectors",
  "Private sector" = "Private sector"
)

supply_approval_series_components <- function(series) {
  contextual_approval_series_components(series)
}

housingSupplyPageUI <- function(id) {
  ns <- NS(id)
  states <- supply_state_choices()

  nav_panel(
    "Housing Supply",
    policy_page_header(
      "Housing Supply",
      "Building activity and construction costs across approvals, dwelling mix and construction-cost pressure."
    ),
    layout_column_wrap(
      width = 1/4,
      fill = FALSE,
      policy_kpi_box(
        title = "Selected Approvals",
        value = textOutput(ns("vb_selected_approvals")),
        subtitle = p(class = "kpi-subtitle",
                     textOutput(ns("vb_selected_approvals_date"))),
        change = uiOutput(ns("vb_selected_approvals_change")),
        accent = "blue"
      ),
      policy_kpi_box(
        title = "Largest Selected Jurisdiction",
        value = textOutput(ns("vb_largest_approval")),
        subtitle = p(class = "kpi-subtitle",
                     textOutput(ns("vb_largest_approval_state"))),
        accent = "teal"
      ),
      policy_kpi_box(
        title = "Construction Costs",
        value = textOutput(ns("vb_construction")),
        subtitle = p(class = "kpi-subtitle", "CPI New Dwelling Index"),
        change = uiOutput(ns("vb_construction_change")),
        accent = "navy"
      ),
      policy_kpi_box(
        title = "Houses Share",
        value = textOutput(ns("vb_houses_share")),
        subtitle = p(class = "kpi-subtitle", "% of total approvals (national)"),
        change = uiOutput(ns("vb_houses_share_change")),
        accent = "purple"
      )
    ),
    sliderInput(ns("supply_dates"), "Date Range",
                min = as.Date("1990-01-01"),
                max = Sys.Date(),
                value = c(as.Date("2000-01-01"), Sys.Date()),
                width = "100%", timeFormat = "%b %Y"),
    layout_column_wrap(
      width = 1/3,
      fill = FALSE,
      selectizeInput(ns("supply_states"), "States/Territories",
                     choices = states,
                     selected = states,
                     multiple = TRUE),
      selectInput(ns("supply_building_type"), "Building type",
                  choices = supply_building_type_choices,
                  selected = "Total approvals",
                  selectize = FALSE),
      selectInput(ns("supply_sector"), "Sector",
                  choices = supply_sector_choices,
                  selected = "Total sectors",
                  selectize = FALSE)
    ),
    layout_column_wrap(
      width = 1,
      fill = FALSE,
      policy_chart_card(
        title = "Building Approvals",
        fill = FALSE,
        note = "ABS building approvals. Approval counts are supply pipeline indicators, not completed dwellings. State, building type and sector controls filter the same ABS series rather than changing methodology.",
        plotlyOutput(ns("supply_approvals"), height = "420px")
      ),
      policy_chart_card(
        title = "CPI New Dwelling Purchase (Construction Cost)",
        note = "ABS CPI new dwelling purchase is a construction-cost price index, not a household burden measure.",
        div(class = "chart-wide", plotlyOutput(ns("supply_cpi_construction"), height = "100%", width = "100%"))
      )
    )
  )
}

housingSupplyPageServer <- function(id, is_dark) {
  moduleServer(id, function(input, output, session) {
    selected_supply_states <- reactive({
      states <- input$supply_states
      if (is.null(states) || length(states) == 0) {
        return(supply_state_choices())
      }
      states
    })

    selected_approvals <- reactive({
      req(input$supply_building_type, input$supply_sector)
      selected_approvals_latest(
        supply_demand,
        states = selected_supply_states(),
        building_type = input$supply_building_type,
        sector = input$supply_sector
      )
    })

    largest_approval <- reactive({
      req(input$supply_building_type, input$supply_sector)
      largest_selected_approval(
        supply_demand,
        states = selected_supply_states(),
        building_type = input$supply_building_type,
        sector = input$supply_sector
      )
    })

    output$vb_selected_approvals <- renderText({
      d <- selected_approvals()
      if (nrow(d) == 0) return("N/A")
      fmt_number(d$value[1])
    })
    output$vb_selected_approvals_date <- renderText({
      d <- selected_approvals()
      if (nrow(d) == 0) return("Monthly dwelling units")
      paste0(format(d$date[1], "%b %Y"), " | ", d$n_jurisdictions[1],
             " selected")
    })
    output$vb_selected_approvals_change <- renderUI({
      ch <- selected_approvals_yoy_change(
        supply_demand,
        states = selected_supply_states(),
        building_type = input$supply_building_type,
        sector = input$supply_sector
      )
      pct <- ch$change[1]
      css_class <- kpi_change_class(pct, favourable = "increase")
      lbl <- if (is.na(pct)) "" else ch$label[1]
      tags$p(class = paste("kpi-subtitle", css_class), lbl)
    })

    output$vb_largest_approval <- renderText({
      d <- largest_approval()
      if (nrow(d) == 0) return("N/A")
      fmt_number(d$value[1])
    })
    output$vb_largest_approval_state <- renderText({
      d <- largest_approval()
      if (nrow(d) == 0) return("")
      paste0(d$approval_label[1], " | ", format(d$date[1], "%b %Y"))
    })

    approvals_latest <- function(state_name) {
      d <- selected_approvals_latest(
        supply_demand,
        states = state_name,
        building_type = "Total approvals",
        sector = "Total sectors"
      )
      if (nrow(d) == 0) return(data.frame())
      d
    }

    output$vb_construction <- renderText({
      v <- latest_val(abs_ts, "series", "CPI New Dwelling Purchase")
      fmt_index(v)
    })
    output$vb_construction_change <- renderUI({
      ch <- latest_change(abs_ts, "series", "CPI New Dwelling Purchase",
                          periods_back = 12, period_label = "YoY",
                          change_type = "relative_pct")
      diff_val <- ch$change
      css_class <- kpi_change_class(diff_val, favourable = "decrease")
      lbl <- if (is.na(diff_val)) "" else ch$label
      tags$p(class = paste("kpi-subtitle", css_class), lbl)
    })

    output$vb_houses_share <- renderText({
      d_total <- supply_demand %>%
        filter(category == "Building Approvals",
               str_detect(series, "Total \\(Type of Building\\)"),
               str_detect(series, "Total Sectors"),
               !is.na(value)) %>%
        arrange(desc(date))
      if (nrow(d_total) == 0) return("N/A")
      latest_month <- d_total$date[1]

      total_val <- d_total %>% filter(date == latest_month) %>% summarise(s = sum(value)) %>% pull(s)
      houses_val <- supply_demand %>%
        filter(category == "Building Approvals",
               str_detect(series, "Houses"),
               str_detect(series, "Total Sectors"),
               !is.na(value),
               date == latest_month) %>%
        summarise(s = sum(value)) %>% pull(s)

      if (total_val == 0) return("N/A")
      fmt_pct(houses_val / total_val * 100, 0.1)
    })
    output$vb_houses_share_change <- renderUI({
      calc_share <- function(target_date) {
        total_val <- supply_demand %>%
          filter(category == "Building Approvals",
                 str_detect(series, "Total \\(Type of Building\\)"),
                 str_detect(series, "Total Sectors"),
                 !is.na(value), date == target_date) %>%
          summarise(s = sum(value)) %>% pull(s)
        houses_val <- supply_demand %>%
          filter(category == "Building Approvals",
                 str_detect(series, "Houses"),
                 str_detect(series, "Total Sectors"),
                 !is.na(value), date == target_date) %>%
          summarise(s = sum(value)) %>% pull(s)
        if (total_val == 0) return(NA_real_)
        houses_val / total_val * 100
      }

      dates <- supply_demand %>%
        filter(category == "Building Approvals",
               str_detect(series, "Total \\(Type of Building\\)"),
               str_detect(series, "Total Sectors"),
               !is.na(value)) %>%
        distinct(date) %>% arrange(desc(date)) %>% pull(date)

      if (length(dates) < 13) return(tags$p(class = "kpi-subtitle", ""))
      current_share <- calc_share(dates[1])
      previous_share <- calc_share(dates[13])
      if (is.na(current_share) || is.na(previous_share)) return(tags$p(class = "kpi-subtitle", ""))

      diff_pp <- current_share - previous_share
      direction <- if (diff_pp >= 0) "\u2191" else "\u2193"
      label <- paste0(direction, " ", sprintf("%+.1f pp", diff_pp), " YoY")
      css_class <- kpi_change_class(diff_pp, favourable = "neutral")
      tags$p(class = paste("kpi-subtitle", css_class), label)
    })

    output$supply_approvals <- renderPlotly({
      req(input$supply_states, input$supply_building_type, input$supply_sector)

      d <- supply_demand %>%
        filter(category == "Building Approvals",
               date >= input$supply_dates[1],
               date <= input$supply_dates[2]) %>%
        left_join(supply_approval_series_components(unique(.$series)),
                  by = "series") %>%
        filter(approval_state %in% input$supply_states,
               approval_building_type == input$supply_building_type,
               approval_sector == input$supply_sector)

      validate(need(nrow(d) > 0,
        "Run pipeline/05_driver.R to fetch building approvals data (ABS 8731.0)"))

      p <- build_supply_approvals_plot(
        d,
        title = paste(input$supply_building_type, "-", input$supply_sector),
        dark = is_dark()
      )

      dashboard_ggplotly(p, dark = is_dark(), tooltip = c("x", "y", "color"))
    }) %>%
      bindCache(input$supply_dates, input$supply_states, input$supply_building_type, input$supply_sector, is_dark())

    output$supply_cpi_construction <- renderPlotly({
      d <- abs_ts %>%
        filter(series == "CPI New Dwelling Purchase",
               date >= input$supply_dates[1],
               date <= input$supply_dates[2])
      validate(need(nrow(d) > 0, "No CPI construction cost data available."))

      p <- build_supply_construction_cpi_plot(d, dark = is_dark())

      dashboard_ggplotly(p, dark = is_dark(), tooltip = c("x", "y"))
    }) %>%
      bindCache(input$supply_dates, is_dark())
  })
}

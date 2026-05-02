# Housing Supply page module.

supply_state_choices <- c("New South Wales", "Victoria")

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
  parse_one <- function(x) {
    parts <- trimws(strsplit(x, ";", fixed = TRUE)[[1]])
    parts <- parts[nzchar(parts)]

    approval_state <- if (length(parts) >= 2) parts[[2]] else NA_character_
    approval_building_type_raw <- if (length(parts) >= 3) parts[[3]] else NA_character_
    approval_sector_raw <- if (length(parts) >= 4) parts[[4]] else NA_character_

    approval_building_type <- dplyr::case_when(
      approval_building_type_raw == "Total (Type of Building)" ~ "Total approvals",
      approval_building_type_raw == "Houses" ~ "Houses",
      approval_building_type_raw == "Dwellings excluding houses" ~ "Dwellings excluding houses",
      TRUE ~ approval_building_type_raw
    )

    approval_sector <- dplyr::case_when(
      approval_sector_raw == "Total Sectors" ~ "Total sectors",
      approval_sector_raw == "Private Sector" ~ "Private sector",
      TRUE ~ approval_sector_raw
    )

    approval_label <- dplyr::case_when(
      approval_state == "New South Wales" ~ "NSW",
      approval_state == "Victoria" ~ "VIC",
      TRUE ~ approval_state
    )

    data.frame(
      series = x,
      approval_state = approval_state,
      approval_building_type = approval_building_type,
      approval_sector = approval_sector,
      approval_label = approval_label,
      stringsAsFactors = FALSE
    )
  }

  dplyr::bind_rows(lapply(as.character(series), parse_one))
}

housingSupplyPageUI <- function(id) {
  ns <- NS(id)

  nav_panel(
    "Housing Supply",
    div(
      class = "d-flex justify-content-between align-items-start mb-3 px-2",
      div(
        tags$h3("Housing Supply", class = "mb-1",
                style = "font-weight: 700;"),
        tags$p("Building activity and construction costs",
               style = "color: var(--app-muted); margin-bottom: 0;")
      )
    ),
    layout_column_wrap(
      width = 1/4,
      fill = FALSE,
      value_box(
        title = "NSW Approvals",
        value = textOutput(ns("vb_approvals_nsw")),
        p(class = "kpi-subtitle", "Monthly dwelling units"),
        uiOutput(ns("vb_approvals_nsw_change")),
        theme = value_box_theme(bg = "#0E5A8A", fg = "#fff")
      ),
      value_box(
        title = "VIC Approvals",
        value = textOutput(ns("vb_approvals_vic")),
        p(class = "kpi-subtitle", "Monthly dwelling units"),
        uiOutput(ns("vb_approvals_vic_change")),
        theme = value_box_theme(bg = "#3B4C7A", fg = "#fff")
      ),
      value_box(
        title = "Construction Costs",
        value = textOutput(ns("vb_construction")),
        p(class = "kpi-subtitle", "CPI New Dwelling Index"),
        uiOutput(ns("vb_construction_change")),
        theme = value_box_theme(bg = "#17415F", fg = "#fff")
      ),
      value_box(
        title = "Houses Share",
        value = textOutput(ns("vb_houses_share")),
        p(class = "kpi-subtitle", "% of total approvals"),
        uiOutput(ns("vb_houses_share_change")),
        theme = value_box_theme(bg = "#984ea3", fg = "#fff")
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
                     choices = supply_state_choices,
                     selected = supply_state_choices,
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
      card(
        fill = FALSE,
        card_header("Building Approvals"),
        source_note("ABS building approvals. Approval counts are supply pipeline indicators, not completed dwellings. State, building type and sector controls filter the same ABS series rather than changing methodology."),
        card_body(plotlyOutput(ns("supply_approvals"), height = "420px"))
      ),
      card(
        card_header("CPI New Dwelling Purchase (Construction Cost)"),
        source_note("ABS CPI new dwelling purchase is a construction-cost price index, not a household burden measure."),
        card_body(div(class = "chart-wide", plotlyOutput(ns("supply_cpi_construction"), height = "100%", width = "100%")))
      )
    )
  )
}

housingSupplyPageServer <- function(id, is_dark) {
  moduleServer(id, function(input, output, session) {
    approvals_latest <- function(state_name) {
      supply_demand %>%
        filter(category == "Building Approvals",
               str_detect(series, state_name),
               str_detect(series, "Total \\(Type of Building\\)"),
               str_detect(series, "Total Sectors")) %>%
        filter(!is.na(value)) %>%
        arrange(desc(date)) %>%
        slice(1)
    }

    output$vb_approvals_nsw <- renderText({
      d <- approvals_latest("New South Wales")
      if (nrow(d) == 0) return("N/A")
      fmt_number(d$value[1])
    })
    output$vb_approvals_nsw_change <- renderUI({
      d <- supply_demand %>%
        filter(category == "Building Approvals",
               str_detect(series, "New South Wales"),
               str_detect(series, "Total \\(Type of Building\\)"),
               str_detect(series, "Total Sectors"),
               !is.na(value)) %>%
        arrange(desc(date))
      if (nrow(d) < 13) return(tags$p(class = "kpi-subtitle", ""))
      current <- d$value[1]; previous <- d$value[13]
      if (is.na(previous) || previous == 0) return(tags$p(class = "kpi-subtitle", ""))
      pct <- (current / previous - 1) * 100
      direction <- if (pct >= 0) "\u2191" else "\u2193"
      label <- paste0(direction, " ", sprintf("%+.1f%%", pct), " YoY")
      css_class <- if (pct >= 0) "kpi-change-up" else "kpi-change-down"
      tags$p(class = paste("kpi-subtitle", css_class), label)
    })

    output$vb_approvals_vic <- renderText({
      d <- approvals_latest("Victoria")
      if (nrow(d) == 0) return("N/A")
      fmt_number(d$value[1])
    })
    output$vb_approvals_vic_change <- renderUI({
      d <- supply_demand %>%
        filter(category == "Building Approvals",
               str_detect(series, "Victoria"),
               str_detect(series, "Total \\(Type of Building\\)"),
               str_detect(series, "Total Sectors"),
               !is.na(value)) %>%
        arrange(desc(date))
      if (nrow(d) < 13) return(tags$p(class = "kpi-subtitle", ""))
      current <- d$value[1]; previous <- d$value[13]
      if (is.na(previous) || previous == 0) return(tags$p(class = "kpi-subtitle", ""))
      pct <- (current / previous - 1) * 100
      direction <- if (pct >= 0) "\u2191" else "\u2193"
      label <- paste0(direction, " ", sprintf("%+.1f%%", pct), " YoY")
      css_class <- if (pct >= 0) "kpi-change-up" else "kpi-change-down"
      tags$p(class = paste("kpi-subtitle", css_class), label)
    })

    output$vb_construction <- renderText({
      v <- latest_val(abs_ts, "series", "CPI New Dwelling Purchase")
      fmt_index(v)
    })
    output$vb_construction_change <- renderUI({
      ch <- latest_change(abs_ts, "series", "CPI New Dwelling Purchase",
                          periods_back = 12, period_label = "YoY",
                          change_type = "relative_pct")
      diff_val <- ch$change
      css_class <- if (!is.na(diff_val) && diff_val >= 0) "kpi-change-up" else "kpi-change-down"
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
      css_class <- if (diff_pp >= 0) "kpi-change-up" else "kpi-change-down"
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

      p <- ggplot(d, aes(x = date, y = value, color = approval_label)) +
        geom_line(linewidth = 0.8, alpha = 0.85) +
        scale_x_date(date_labels = "%Y", date_breaks = "5 years") +
        scale_y_continuous(labels = label_number(big.mark = ",")) +
        labs(
          x = NULL,
          y = "Number of Dwellings",
          color = NULL,
          title = paste(input$supply_building_type, "-", input$supply_sector)
        ) +
        theme_afford(is_dark()) +
        theme(legend.position = "bottom")

      dashboard_ggplotly(p, dark = is_dark(), tooltip = c("x", "y", "color"))
    }) %>%
      bindCache(input$supply_dates, input$supply_states, input$supply_building_type, input$supply_sector, is_dark())

    output$supply_cpi_construction <- renderPlotly({
      d <- abs_ts %>%
        filter(series == "CPI New Dwelling Purchase",
               date >= input$supply_dates[1],
               date <= input$supply_dates[2])
      validate(need(nrow(d) > 0, "No CPI construction cost data available."))

      p <- ggplot(d, aes(x = date, y = value)) +
        geom_line(linewidth = 1, color = "#0E5A8A") +
        scale_x_date(date_labels = "%Y", date_breaks = "5 years") +
        labs(x = NULL, y = "Index") +
        theme_afford(is_dark())

      dashboard_ggplotly(p, dark = is_dark(), tooltip = c("x", "y"))
    }) %>%
      bindCache(input$supply_dates, is_dark())
  })
}

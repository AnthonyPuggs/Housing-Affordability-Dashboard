# Recent Buyers page module.
#
# Official ABS SIH File 9 evidence on recent home buyer households (2019-20):
# dwelling values, mortgages, equity, housing costs and household profiles by
# buyer type, plus timely official ABS 5601.0 first home buyer lending
# aggregates. Everything on this page is official ABS evidence (survey
# estimates or administrative lending aggregates), kept separate from the
# dashboard's modelled market-entry indicators.

if (!exists("normalise_recent_buyers", mode = "function", inherits = TRUE)) {
  recent_buyers_helper_path <- if (exists("project_path", mode = "function", inherits = TRUE)) {
    project_path("R", "recent_buyers_helpers.R")
  } else {
    file.path("R", "recent_buyers_helpers.R")
  }
  if (!file.exists(recent_buyers_helper_path)) {
    stop("Could not locate R/recent_buyers_helpers.R for recent buyers module.",
         call. = FALSE)
  }
  source(recent_buyers_helper_path, local = environment())
}

recentBuyersPageUI <- function(id) {
  ns <- NS(id)

  nav_panel(
    "Recent Buyers",
    div(
      class = "recent-buyers-page",
      policy_page_header(
        "Recent Buyers",
        "Official SIH File 9 evidence on recent home buyers: dwelling values, mortgages, equity and household profiles by buyer type, alongside timely ABS lending aggregates on new first home buyer loans."
      ),
      layout_sidebar(
        sidebar = sidebar(
          width = 300, open = "desktop",
          selectInput(ns("recent_buyers_metric"), "Financial metric",
                      choices = recent_buyers_metric_choices(),
                      selected = "mean_dwelling_value"),
          checkboxGroupInput(ns("recent_buyers_dwelling"), "Dwelling type",
                             choices = c("New" = "new",
                                         "Established" = "established",
                                         "Total" = "total"),
                             selected = c("new", "established", "total")),
          selectInput(ns("recent_buyers_profile"), "Household profile",
                      choices = recent_buyers_profile_choices(),
                      selected = "age_band"),
          source_note("2019-20 SIH recent home buyer households (owners with a mortgage). Official survey evidence, not a live market-entry index."),
          source_note("The ABS does not publish sampling-error metadata for File 9 in this extract, so reliability markers appear only where metadata exists.")
        ),
        uiOutput(ns("recent_buyers_summary")),
        policy_chart_card(
          "Financial Characteristics by Buyer Type",
          note = policy_source_note("ABS Survey of Income and Housing File 9. Values describe 2019-20 recent buyer households and should be read separately from stylised deposit/serviceability scenarios. ", sih_sampling_error_note),
          div(class = "chart-wide recent-buyers-chart",
              plotlyOutput(ns("recent_buyers_chart"), height = "100%",
                           width = "100%"))
        ),
        policy_chart_card(
          "Household Profile by Buyer Type",
          note = policy_source_note("Share of recent buyer households in each band (total dwellings). First-home and changeover buyer profiles are official SIH File 9 proportions. ", sih_sampling_error_note),
          div(class = "chart-wide recent-buyers-chart",
              plotlyOutput(ns("recent_buyers_profile_chart"), height = "100%",
                           width = "100%"))
        ),
        policy_chart_card(
          "First Home Buyer Lending (Timely Official Aggregates)",
          note = policy_source_note("ABS 5601.0 Lending Indicators Table 24: new owner-occupier first home buyer loan commitments, Australia, seasonally adjusted, quarterly. Official lending aggregates describing a flow of new commitments - not the SIH household stock above, and not a modelled scenario."),
          radioButtons(ns("fhb_lending_measure"), NULL,
                       choices = c("New loan commitments (number)" = "count",
                                   "Average loan size ($)" = "avg_loan_size"),
                       selected = "count", inline = TRUE),
          div(class = "chart-wide recent-buyers-chart",
              plotlyOutput(ns("fhb_lending_chart"), height = "100%",
                           width = "100%"))
        )
      )
    )
  )
}

recentBuyersPageServer <- function(id, is_dark) {
  moduleServer(id, function(input, output, session) {
    recent_buyers_data <- reactive({
      raw <- if (exists("sih_recent_buyers", inherits = TRUE)) {
        sih_recent_buyers
      } else {
        data.frame()
      }
      normalise_recent_buyers(raw)
    })

    recent_buyers_profile_data <- reactive({
      raw <- if (exists("sih_recent_buyers", inherits = TRUE)) {
        sih_recent_buyers
      } else {
        data.frame()
      }
      normalise_recent_buyers_profile(raw)
    })

    output$recent_buyers_summary <- renderUI({
      d <- recent_buyers_summary(recent_buyers_data())
      validate(need(nrow(d) > 0, "No recent buyer summary data available."))

      accents <- c("blue", "teal", "navy", "purple")
      boxes <- lapply(seq_len(nrow(d)), function(i) {
        row <- d[i, ]
        policy_kpi_box(
          title = row$title,
          value = tags$span(row$formatted_value),
          subtitle = tags$p(row$subtitle, class = "kpi-subtitle"),
          accent = accents[((i - 1) %% length(accents)) + 1]
        )
      })
      do.call(layout_column_wrap, c(list(width = 1/4, fill = FALSE), boxes))
    })

    output$recent_buyers_chart <- renderPlotly({
      validate(need(nrow(recent_buyers_data()) > 0,
                    "Recent buyer data is unavailable - run the data pipeline."))
      req(input$recent_buyers_metric, input$recent_buyers_dwelling)
      d <- recent_buyers_data() %>%
        filter(
          metric_id == input$recent_buyers_metric,
          dwelling_type %in% input$recent_buyers_dwelling,
          buyer_type %in% c("first_home", "changeover", "all_recent")
        ) %>%
        join_sih_quality(if (exists("sih_quality", inherits = TRUE)) sih_quality else NULL) %>%
        mutate(
          hover_text = paste0(
            buyer_type_label,
            "<br>Dwelling type: ", dwelling_type_label,
            "<br>", metric_label, ": ", formatted_value,
            "<br>Survey year: ", survey_year,
            "<br>", quality_hover,
            "<br>Official SIH File 9 recent buyer evidence."
          )
        )
      validate(need(nrow(d) > 0, "No recent buyer data for selected filters."))

      metric_label <- unique(d$metric_label)[[1]]
      p <- build_recent_buyers_plot(d, metric_label = metric_label,
                                    dark = is_dark())

      dashboard_ggplotly(p, dark = is_dark(), tooltip = "text",
                         hovermode = "closest")
    }) %>%
      bindCache(input$recent_buyers_metric, input$recent_buyers_dwelling,
                is_dark())

    output$recent_buyers_profile_chart <- renderPlotly({
      validate(need(nrow(recent_buyers_profile_data()) > 0,
                    "Recent buyer profile data is unavailable - run the data pipeline."))
      req(input$recent_buyers_profile)
      d <- recent_buyers_profile_data() %>%
        filter(profile_dimension == input$recent_buyers_profile) %>%
        join_sih_quality(if (exists("sih_quality", inherits = TRUE)) sih_quality else NULL) %>%
        mutate(
          hover_text = paste0(
            buyer_type_label,
            "<br>", profile_label, ": ", number(value, accuracy = 0.1), "%",
            "<br>Survey year: ", survey_year,
            "<br>", quality_hover,
            "<br>Official SIH File 9 recent buyer evidence."
          )
        )
      validate(need(nrow(d) > 0, "No profile data for the selected dimension."))

      dimension_label <- recent_buyers_profile_dimension_label(
        input$recent_buyers_profile
      )
      p <- build_recent_buyers_profile_plot(
        d,
        dimension_label = dimension_label,
        dark = is_dark()
      )

      dashboard_ggplotly(p, dark = is_dark(), tooltip = "text",
                         hovermode = "closest")
    }) %>%
      bindCache(input$recent_buyers_profile, is_dark())

    output$fhb_lending_chart <- renderPlotly({
      req(input$fhb_lending_measure)
      indicator_name <- if (identical(input$fhb_lending_measure,
                                      "avg_loan_size")) {
        "FHB Average Loan Size"
      } else {
        "FHB New Loan Commitments"
      }
      d <- if (exists("afford_idx", inherits = TRUE)) {
        afford_idx %>% filter(indicator == indicator_name)
      } else {
        data.frame()
      }
      validate(need(nrow(d) > 0,
                    "FHB lending data is unavailable - run the data pipeline."))

      p <- build_fhb_lending_plot(d, measure = input$fhb_lending_measure,
                                  dark = is_dark())

      dashboard_ggplotly(p, dark = is_dark(), tooltip = c("x", "y"))
    }) %>%
      bindCache(input$fhb_lending_measure, is_dark())
  })
}

# Geographic Affordability page module.

if (!exists("join_sih_quality", mode = "function", inherits = TRUE)) {
  sih_quality_helper_path <- if (exists("project_path", mode = "function", inherits = TRUE)) {
    project_path("R", "sih_quality_helpers.R")
  } else {
    file.path("R", "sih_quality_helpers.R")
  }
  if (!file.exists(sih_quality_helper_path)) {
    stop("Could not locate R/sih_quality_helpers.R for geographic affordability module.",
         call. = FALSE)
  }
  source(sih_quality_helper_path, local = environment())
}

geo_state_metric_choices <- c(
  "Cost-to-income ratio" = "cost_income_ratio",
  "Real weekly housing cost" = "mean_weekly_cost_real"
)

geo_lower_metric_choices <- c(
  "Stress over 30%" = "pct_over_30",
  "Median cost-to-income ratio" = "median_cost_income_ratio",
  "Median weekly housing cost" = "median_weekly_cost"
)

geo_gcc_metric_choices <- c(
  "Median cost-to-income ratio" = "median_cost_income_ratio",
  "Median weekly housing cost" = "median_weekly_cost",
  "Mean cost-to-income ratio" = "mean_cost_income_ratio",
  "Mean weekly housing cost" = "mean_weekly_cost"
)

geo_tenure_choices <- c(
  "Renters" = "renter_total",
  "Mortgage owners" = "owner_mortgage",
  "All households" = "all"
)

geo_metric_label <- function(metric) {
  switch(metric,
         cost_income_ratio = "Cost-to-income ratio",
         mean_weekly_cost_real = "Real weekly housing cost",
         pct_over_30 = "Households paying more than 30% of income",
         median_cost_income_ratio = "Median cost-to-income ratio",
         mean_cost_income_ratio = "Mean cost-to-income ratio",
         median_weekly_cost = "Median weekly housing cost",
         mean_weekly_cost = "Mean weekly housing cost",
         metric)
}

geo_axis_label <- function(metric) {
  if (metric %in% c("cost_income_ratio", "pct_over_30",
                    "median_cost_income_ratio", "mean_cost_income_ratio")) {
    return("% of gross income")
  }
  "Weekly housing cost ($)"
}

geo_value_labels <- function(metric) {
  if (metric %in% c("cost_income_ratio", "pct_over_30",
                    "median_cost_income_ratio", "mean_cost_income_ratio")) {
    return(label_percent(scale = 1, accuracy = 1))
  }
  label_dollar(prefix = "$", accuracy = 1)
}

geo_selected_states <- function(selected, states) {
  if (is.null(selected) || length(selected) == 0 || "all" %in% selected) {
    return(states)
  }
  selected
}

geo_selected_regions <- function(selected, regions) {
  if (is.null(selected) || length(selected) == 0 || "all" %in% selected) {
    return(regions)
  }
  selected
}

geo_keep_largest_estimate <- function(df, key_cols) {
  if (nrow(df) == 0) {
    return(df)
  }

  df %>%
    arrange(across(all_of(key_cols)), desc(value)) %>%
    distinct(across(all_of(key_cols)), .keep_all = TRUE)
}

geo_sih_note <- function(extra = NULL) {
  source_note(
    "ABS Survey of Income and Housing. These are geography-aligned SIH survey measures; not modelled market-entry indexes. ",
    extra,
    sih_sampling_error_note
  )
}

geographicAffordabilityPageUI <- function(id) {
  ns <- NS(id)

  state_choices <- if (nrow(sih_state_ts) > 0) {
    sort(unique(sih_state_ts$geography))
  } else {
    character(0)
  }

  region_choices <- if (nrow(sih_geographic) > 0) {
    sort(unique(sih_geographic$geography))
  } else {
    character(0)
  }

  default_regions <- intersect(
    c("Total GCC", "Total rest of state", "Gr. Sydney", "Rest of NSW",
      "Gr. Melbourne", "Rest of Vic."),
    region_choices
  )
  if (length(default_regions) == 0 && length(region_choices) > 0) {
    default_regions <- head(region_choices, 8)
  }

  nav_panel(
    "Geographic Affordability",
    layout_sidebar(
      sidebar = sidebar(
        width = 310, open = "desktop",
        tags$h5("Geography-aligned SIH affordability measures"),
        source_note(
          "Comparisons on this page use housing-cost and household-income measures from the same SIH geography. They should not be read as modelled market-entry indexes."
        ),
        selectizeInput(ns("geo_states"), "States/Territories",
                       choices = c("All" = "all", state_choices),
                       selected = "all", multiple = TRUE),
        selectInput(ns("geo_state_metric"), "State trend measure",
                    choices = geo_state_metric_choices,
                    selected = "cost_income_ratio"),
        selectInput(ns("geo_state_tenure"), "State tenure",
                    choices = geo_tenure_choices,
                    selected = "renter_total"),
        selectInput(ns("geo_lower_metric"), "Lower-income state measure",
                    choices = geo_lower_metric_choices,
                    selected = "pct_over_30"),
        selectInput(ns("geo_lower_tenure"), "Lower-income tenure",
                    choices = geo_tenure_choices,
                    selected = "renter_total"),
        selectizeInput(ns("geo_gcc_geographies"),
                       "Capital/rest-of-state geographies",
                       choices = c("All" = "all", region_choices),
                       selected = default_regions, multiple = TRUE),
        selectInput(ns("geo_gcc_metric"), "Capital/rest-of-state measure",
                    choices = geo_gcc_metric_choices,
                    selected = "median_cost_income_ratio"),
        selectInput(ns("geo_gcc_tenure"), "Capital/rest-of-state tenure",
                    choices = geo_tenure_choices,
                    selected = "renter_total")
      ),
      layout_column_wrap(
        width = "520px",
        card(
          card_header("State SIH Cost-to-Income Trend"),
          geo_sih_note("Trend uses File 12 state/territory SIH estimates."),
          card_body(div(class = "chart-wide",
                        plotlyOutput(ns("geo_state_trend"),
                                     height = "100%", width = "100%")))
        ),
        card(
          card_header("Latest State Comparison"),
          geo_sih_note("Latest available survey year from the state SIH series."),
          card_body(div(class = "chart-square",
                        plotlyOutput(ns("geo_state_latest"),
                                     height = "100%", width = "100%")))
        ),
        card(
          card_header("Lower-Income State Burden"),
          geo_sih_note("Lower-income household estimates from SIH File 8."),
          card_body(div(class = "chart-square",
                        plotlyOutput(ns("geo_lower_income"),
                                     height = "100%", width = "100%")))
        ),
        card(
          card_header("Capital City / Rest-of-State Comparison"),
          geo_sih_note("Greater-capital-city and rest-of-state estimates from SIH File 11."),
          card_body(div(class = "chart-square",
                        plotlyOutput(ns("geo_gcc_comparison"),
                                     height = "100%", width = "100%")))
        )
      )
    )
  )
}

geographicAffordabilityPageServer <- function(id, is_dark) {
  moduleServer(id, function(input, output, session) {
    state_geographies <- reactive({
      states <- sort(unique(sih_state_ts$geography))
      geo_selected_states(input$geo_states, states)
    })

    state_series <- reactive({
      d <- sih_state_ts %>%
        filter(
          breakdown_var == "tenure",
          metric == input$geo_state_metric,
          tenure == input$geo_state_tenure,
          geography %in% state_geographies()
        ) %>%
        geo_keep_largest_estimate(
          c("survey_year", "metric", "tenure", "geography")
        )

      year_levels <- unique(sih_state_ts$survey_year)
      d %>%
        mutate(survey_year = factor(survey_year,
                                    levels = year_levels[year_levels %in% survey_year]))
    })

    output$geo_state_trend <- renderPlotly({
      d <- state_series()
      validate(need(nrow(d) > 0, "No state SIH trend data for the selected options."))

      p <- build_geo_state_trend_plot(
        d,
        metric_label = geo_metric_label(input$geo_state_metric),
        axis_label = geo_axis_label(input$geo_state_metric),
        value_labels = geo_value_labels(input$geo_state_metric),
        dark = is_dark()
      )

      dashboard_ggplotly(p, dark = is_dark(), tooltip = c("x", "y", "color"))
    }) %>%
      bindCache(input$geo_states, input$geo_state_metric, input$geo_state_tenure, is_dark())

    output$geo_state_latest <- renderPlotly({
      d <- state_series()
      validate(need(nrow(d) > 0, "No state SIH comparison data for the selected options."))

      year_levels <- levels(d$survey_year)
      latest_year <- tail(year_levels[year_levels %in% as.character(d$survey_year)], 1)
      d <- d %>% filter(as.character(survey_year) == latest_year)

      p <- build_geo_state_latest_plot(
        d,
        latest_year = latest_year,
        axis_label = geo_axis_label(input$geo_state_metric),
        value_labels = geo_value_labels(input$geo_state_metric),
        dark = is_dark()
      )

      dashboard_ggplotly(p, dark = is_dark(), tooltip = c("x", "y"))
    }) %>%
      bindCache(input$geo_states, input$geo_state_metric, input$geo_state_tenure, is_dark())

    output$geo_lower_income <- renderPlotly({
      states <- sort(setdiff(unique(sih_lower_income_states$geography), "Australia"))
      selected_states <- geo_selected_states(input$geo_states, states)

      d <- sih_lower_income_states %>%
        filter(
          metric == input$geo_lower_metric,
          tenure == input$geo_lower_tenure,
          geography %in% selected_states
        ) %>%
        geo_keep_largest_estimate(
          c("survey_year", "metric", "tenure", "geography")
        )

      nat <- sih_lower_income_states %>%
        filter(
          metric == input$geo_lower_metric,
          tenure == input$geo_lower_tenure,
          geography == "Australia"
        ) %>%
        geo_keep_largest_estimate(
          c("survey_year", "metric", "tenure", "geography")
        )

      validate(need(nrow(d) > 0, "No lower-income state data for the selected options."))

      d <- d %>%
        join_sih_quality(sih_quality) %>%
        mutate(
          reliability_marker = sih_reliability_marker(rse_reliability_flag),
          quality_hover = sih_quality_hover_text(
            rse_pct,
            moe_95,
            rse_reliability_flag
          ),
          hover_text = paste0(
            geography,
            "<br>Survey year: ", survey_year,
            "<br>", geo_metric_label(input$geo_lower_metric), ": ",
            if (input$geo_lower_metric %in% c("pct_over_30",
                                              "median_cost_income_ratio",
                                              "mean_cost_income_ratio")) {
              paste0(number(value, accuracy = 0.1), "%")
            } else {
              dollar(value, accuracy = 1)
            },
            "<br>", quality_hover,
            ifelse(nzchar(interval_label), paste0("<br>", interval_label), "")
          )
        )

      p <- build_geo_lower_income_plot(
        d,
        national_value = if (nrow(nat) > 0) nat$value[1] else NULL,
        metric_label = geo_metric_label(input$geo_lower_metric),
        axis_label = geo_axis_label(input$geo_lower_metric),
        value_labels = geo_value_labels(input$geo_lower_metric),
        dark = is_dark()
      )

      dashboard_ggplotly(p, dark = is_dark(), tooltip = c("x", "y", "text"))
    }) %>%
      bindCache(input$geo_states, input$geo_lower_metric, input$geo_lower_tenure, is_dark())

    output$geo_gcc_comparison <- renderPlotly({
      geographies <- sort(unique(sih_geographic$geography))
      selected_regions <- geo_selected_regions(input$geo_gcc_geographies, geographies)

      d <- sih_geographic %>%
        filter(
          breakdown_var %in% c("owner", "renter"),
          metric == input$geo_gcc_metric,
          tenure == input$geo_gcc_tenure,
          geography %in% selected_regions
        ) %>%
        geo_keep_largest_estimate(
          c("survey_year", "metric", "tenure", "geography")
        )

      validate(need(nrow(d) > 0, "No capital/rest-of-state data for the selected options."))

      p <- build_geo_gcc_comparison_plot(
        d,
        metric_label = geo_metric_label(input$geo_gcc_metric),
        axis_label = geo_axis_label(input$geo_gcc_metric),
        value_labels = geo_value_labels(input$geo_gcc_metric),
        dark = is_dark()
      )

      dashboard_ggplotly(p, dark = is_dark(), tooltip = c("x", "y"))
    }) %>%
      bindCache(input$geo_gcc_metric, input$geo_gcc_tenure,
                input$geo_gcc_geographies, is_dark())
  })
}

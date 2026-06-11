# Overview page module.

if (!exists("official_burden_summary", mode = "function", inherits = TRUE)) {
  official_burden_path <- if (exists("project_path", mode = "function", inherits = TRUE)) {
    project_path("R", "official_burden_summary.R")
  } else {
    file.path("R", "official_burden_summary.R")
  }
  if (!file.exists(official_burden_path)) {
    stop("Could not locate R/official_burden_summary.R for overview module.",
         call. = FALSE)
  }
  source(official_burden_path, local = environment())
}

if (!exists("latest_capital_price_extreme", mode = "function", inherits = TRUE)) {
  contextual_kpi_path <- if (exists("project_path", mode = "function", inherits = TRUE)) {
    project_path("R", "contextual_kpi_helpers.R")
  } else {
    file.path("R", "contextual_kpi_helpers.R")
  }
  if (!file.exists(contextual_kpi_path)) {
    stop("Could not locate R/contextual_kpi_helpers.R for overview module.",
         call. = FALSE)
  }
  source(contextual_kpi_path, local = environment())
}

overview_cost_pressure_indicators <- c(
  "Rental Affordability Index",
  "Mortgage Serviceability Index",
  "Price-to-Income Ratio"
)
overview_cost_pressure_colours <- stats::setNames(
  cost_pressure_palette(indicator_chart_label(overview_cost_pressure_indicators)),
  indicator_chart_label(overview_cost_pressure_indicators)
)

overview_affordability_indices_note <- "Cost-pressure indexes are burden measures where higher = less affordable. The National Housing Affordability Score above is the reverse: higher = more affordable. Rent uses ABS CPI rents/WPI, mortgage indexes 30-year principal-and-interest repayments at actual new-loan rates against WPI, deposit uses price/income, and price-to-income uses national dwelling prices/WPI."

score_component_short_labels <- c(
  mortgage_serviceability = "Mortgage",
  rental_entry = "Rental",
  deposit_barrier = "Deposit"
)

score_component_explanations <- c(
  mortgage_serviceability = "Monthly repayment burden",
  rental_entry = "Rent pressure relative to wages",
  deposit_barrier = "Upfront saving barrier"
)

overview_score_change <- function(score_data, selected_date = NULL) {
  if (nrow(score_data) == 0) {
    return(list(change = NA_real_, label = ""))
  }

  d <- score_data %>%
    filter(!is.na(score)) %>%
    arrange(desc(date))
  if (nrow(d) == 0) {
    return(list(change = NA_real_, label = ""))
  }

  current_date <- if (is.null(selected_date) || is.na(selected_date)) {
    d$date[1]
  } else {
    as.Date(selected_date)
  }
  current <- d %>% filter(date == current_date)
  if (nrow(current) == 0) {
    return(list(change = NA_real_, label = ""))
  }
  # Feb-29-safe exact t-12 target (the previous sprintf construction
  # produced NA for a Feb-29 date); no fallback — a missing t-12 blanks
  # the label rather than mislabelling a shorter change as YoY.
  target_date <- lubridate::add_with_rollback(current_date,
                                              -lubridate::years(1))
  previous <- d %>% filter(date == target_date)
  if (nrow(previous) == 0) {
    return(list(change = NA_real_, label = ""))
  }

  change <- current$score[1] - previous$score[1]
  direction <- if (change >= 0) "Up" else "Down"
  list(
    change = change,
    label = paste0(direction, " ", sprintf("%+.1f pts YoY", change))
  )
}

overview_snap_score_date <- function(clicked_date, available_dates) {
  available_dates <- sort(unique(as.Date(available_dates)))
  available_dates <- available_dates[!is.na(available_dates)]
  if (length(available_dates) == 0 || is.null(clicked_date) ||
      length(clicked_date) == 0 || is.na(clicked_date)) {
    return(NULL)
  }

  clicked_date <- as.Date(clicked_date)
  available_dates[which.min(abs(as.numeric(available_dates - clicked_date)))]
}

overview_parse_score_click_date <- function(event_x, available_dates) {
  if (is.null(event_x) || length(event_x) == 0) {
    return(NULL)
  }

  parsed <- tryCatch(
    suppressWarnings(as.Date(event_x)),
    error = function(e) as.Date(NA)
  )
  if (is.na(parsed) && is.numeric(event_x)) {
    parsed <- suppressWarnings(as.Date(event_x, origin = "1970-01-01"))
  }
  if (is.na(parsed) && is.numeric(event_x)) {
    parsed <- suppressWarnings(as.Date(event_x / 86400000,
                                      origin = "1970-01-01"))
  }

  overview_snap_score_date(parsed, available_dates)
}

overview_score_date_should_update <- function(clicked_date, current_date) {
  if (is.null(clicked_date) || length(clicked_date) == 0 ||
      is.na(clicked_date)) {
    return(FALSE)
  }
  if (is.null(current_date) || length(current_date) == 0 ||
      is.na(current_date)) {
    return(TRUE)
  }

  !identical(as.Date(clicked_date), as.Date(current_date))
}

overviewPageUI <- function(id) {
  ns <- NS(id)

  nav_panel(
    "Overview",
    policy_page_header(
      "Housing Affordability",
      "Analysing the state of the Australian market through prices, serviceability and rental cost pressure."
    ),
    layout_column_wrap(
      width = 1,
      fill = FALSE,
      policy_card(
        "National Market-Entry Affordability Score",
        class = "affordability-score-card",
        div(
          class = "affordability-score-panel",
          div(
            class = "affordability-score-summary",
            div(
              class = "affordability-score-value-group",
              div(class = "affordability-score-value",
                  textOutput(ns("vb_afford_score"), inline = TRUE)),
              tags$span(
                "Relative index, not household stress",
                class = "affordability-score-badge"
              )
            ),
            tags$p(class = "affordability-score-date",
                   textOutput(ns("vb_afford_score_date"), inline = TRUE)),
            tags$p(class = "affordability-score-basis",
                   textOutput(ns("vb_afford_score_basis"), inline = TRUE)),
            tags$p(
              class = "affordability-score-howto",
              "Higher = easier market entry relative to 2012-2025 history. It is not the share of households who can afford housing."
            ),
            uiOutput(ns("vb_afford_score_change")),
            tags$p(
              class = "affordability-score-note",
              "Modelled national score for entering ownership or renting. It combines mortgage serviceability, rental cost pressure and deposit barriers. Component weights are judgement weights, not causal estimates. Not an official ABS/NHHA statistic or lender assessment. The score is published only for quarters with all three components, so it can lag the semiannual AWE wage input."
            ),
            actionButton(
              ns("reset_afford_score_date"),
              "Reset to latest",
              class = "btn btn-outline-secondary btn-sm affordability-score-reset"
            )
          ),
          div(
            class = "affordability-score-trend",
            plotlyOutput(ns("overview_afford_score_trend"), height = "220px")
          ),
          div(
            class = "affordability-score-components",
            tags$h3("Component scores and weighted contribution",
                    class = "affordability-score-components-title"),
            uiOutput(ns("overview_afford_score_components")),
            tags$div(
              class = "affordability-score-interpretation-strip",
              lapply(names(score_component_short_labels), function(component) {
                tags$span(paste0(
                  score_component_short_labels[[component]],
                  " = ",
                  tolower(score_component_explanations[[component]])
                ))
              })
            )
          )
        ),
        tags$div(
          class = "official-burden-snapshot",
          tags$h3("Official SIH/NHHA burden snapshot",
                  class = "affordability-score-components-title"),
          tags$p(
            "Observed household burden measures from SIH/NHHA, kept separate from the stylised market-entry score.",
            class = "affordability-score-note"
          ),
          uiOutput(ns("official_burden_summary"))
        )
      )
    ),
    layout_column_wrap(
      width = 1/4,
      fill = FALSE,
      policy_kpi_box(
        title = "National Mean Dwelling Price",
        value = textOutput(ns("vb_nat_price")),
        subtitle = p(class = "kpi-subtitle", textOutput(ns("vb_nat_price_date"))),
        change = uiOutput(ns("vb_nat_price_change")),
        accent = "blue"
      ),
      policy_kpi_box(
        title = "Highest Capital Median Price",
        value = textOutput(ns("vb_high_capital_price")),
        subtitle = p(class = "kpi-subtitle",
                     textOutput(ns("vb_high_capital_price_city"))),
        change = uiOutput(ns("vb_high_capital_price_change")),
        accent = "teal"
      ),
      policy_kpi_box(
        title = "Modelled Serviceability",
        value = textOutput(ns("vb_service")),
        subtitle = p(class = "kpi-subtitle", "Stylised mortgage scenario"),
        change = uiOutput(ns("vb_service_change")),
        accent = "navy"
      ),
      policy_kpi_box(
        title = "Rental Affordability",
        value = textOutput(ns("vb_rental")),
        subtitle = p(class = "kpi-subtitle", textOutput(ns("vb_rental_date"))),
        change = uiOutput(ns("vb_rental_change")),
        accent = "purple"
      )
    ),
    layout_column_wrap(
      width = 1,
      fill = FALSE,
      policy_chart_card(
        title = "Capital City Median House Prices",
        fill = FALSE,
        note = "City lines are ABS 6432.0 capital-city median established-house transfer prices; the dashed national line is the whole-of-Australia mean price across all dwelling types. Mean and median levels are not directly comparable - read the national line as context, not as another city.",
        uiOutput(ns("overview_price_subtitle")),
        plotlyOutput(ns("overview_median_prices"), height = "480px"),
        footer = card_footer(
          sliderInput(ns("overview_price_dates"), "Date Range",
                      min = min(median_prices_combined$date, na.rm = TRUE),
                      max = max(median_prices_combined$date, na.rm = TRUE),
                      value = c(max(as.Date("2010-01-01"),
                                    min(median_prices_combined$date,
                                        na.rm = TRUE)),
                                max(median_prices_combined$date, na.rm = TRUE)),
                      width = "100%", timeFormat = "%b %Y"),
          radioButtons(ns("overview_price_transform"), NULL,
                       choices = c("Nominal ($)" = "nominal",
                                   "Index (common start = 100)" = "index"),
                       selected = "nominal", inline = TRUE)
        )
      )
    ),
    layout_column_wrap(
      width = 1,
      fill = FALSE,
      policy_chart_card(
        title = tags$span(
          class = "overview-affordability-indices-title-wrap",
          tags$span("Affordability Indices",
                    class = "overview-affordability-indices-title"),
          policy_info_icon(
            "Affordability indices note",
            overview_affordability_indices_note,
            class = "policy-info-icon-left-aligned"
          )
        ),
        fill = FALSE,
        plotlyOutput(ns("overview_afford_change"), height = "380px")
      )
    )
  )
}

overviewPageServer <- function(id, is_dark) {
  moduleServer(id, function(input, output, session) {
    score_dates <- sort(unique(national_affordability_score_ts$date[
      !is.na(national_affordability_score_ts$score)
    ]))
    latest_score_date <- if (length(score_dates) == 0) {
      as.Date(NA)
    } else {
      max(score_dates)
    }
    selected_score_date <- reactiveVal(latest_score_date)
    session$userData$plotlyShinyEventIDs <- unique(c(
      session$userData$plotlyShinyEventIDs,
      "plotly_click-overview_afford_score"
    ))

    score_click <- reactive({
      event_data("plotly_click", source = "overview_afford_score",
                 priority = "event")
    })

    observeEvent(score_click(), {
      clicked <- overview_parse_score_click_date(score_click()$x, score_dates)
      if (overview_score_date_should_update(clicked, selected_score_date())) {
        selected_score_date(clicked)
      }
    }, ignoreNULL = TRUE)

    observeEvent(input$reset_afford_score_date, {
      selected_score_date(latest_score_date)
    }, ignoreInit = TRUE)

    selected_score_row <- reactive({
      if (nrow(national_affordability_score_ts) == 0) {
        return(national_affordability_score_ts)
      }
      d <- national_affordability_score_ts %>%
        filter(!is.na(score))
      selected <- selected_score_date()
      row <- d %>% filter(date == selected)
      if (nrow(row) == 0) {
        row <- d %>% filter(date == max(date))
      }
      row
    })

    output$vb_afford_score <- renderText({
      row <- selected_score_row()
      if (nrow(row) == 0) return("N/A")
      v <- row$score[1]
      paste0(fmt_index(v), " / 100")
    })
    output$vb_afford_score_date <- renderText({
      row <- selected_score_row()
      if (nrow(row) == 0) return("")
      prefix <- if (identical(row$date[1], latest_score_date)) {
        "Latest:"
      } else {
        "Selected:"
      }
      paste(prefix, format(row$date[1], "%b %Y"))
    })
    output$vb_afford_score_basis <- renderText({
      if (length(score_dates) == 0) return("")
      # v2: scores are normalised against a frozen reference window, so the
      # basis reflects the window constants, not the growing score sample.
      paste0(
        "Relative to the frozen ",
        format(NATIONAL_AFFORDABILITY_SCORE_START_DATE, "%Y"),
        "-",
        format(NATIONAL_AFFORDABILITY_SCORE_REFERENCE_END, "%Y"),
        " reference window"
      )
    })
    output$vb_afford_score_change <- renderUI({
      ch <- overview_score_change(
        national_affordability_score_ts,
        selected_date = selected_score_date()
      )
      css_class <- kpi_change_class(ch$change, favourable = "increase")
      tags$p(class = paste("affordability-score-change", css_class),
             ch$label)
    })
    output$overview_afford_score_components <- renderUI({
      d <- national_affordability_score_components %>%
        filter(!is.na(value), date == selected_score_date()) %>%
        arrange(display_order)
      if (nrow(d) == 0) {
        return(tags$p(class = "affordability-score-empty",
                      "No score component data available."))
      }

      component_classes <- c(
        mortgage_serviceability = "score-component-mortgage",
        rental_entry = "score-component-rental",
        deposit_barrier = "score-component-deposit"
      )

      tags$div(
        class = "affordability-score-component-list",
        lapply(seq_len(nrow(d)), function(i) {
          row <- d[i, ]
          tags$div(
            class = "affordability-score-component-row",
            tags$div(
              class = "affordability-score-component-header",
              tags$span(
                class = "affordability-score-component-label-wrap",
                tags$span(row$component_label,
                          class = "affordability-score-component-label"),
                tags$span(
                  score_component_explanations[[row$component]],
                  class = "affordability-score-component-context"
                )
              ),
              tags$span(
                paste0("Score ", fmt_index(row$value), " / 100",
                       " | Weight ", scales::percent(row$weight, accuracy = 1),
                       " | Contribution ", fmt_index(row$value * row$weight),
                       " pts"),
                class = "affordability-score-component-meta"
              )
            ),
            tags$div(
              class = "affordability-score-component-track",
              tags$div(
                class = paste(
                  "affordability-score-component-fill",
                  component_classes[[row$component]]
                ),
                style = paste0("width: ", max(min(row$value, 100), 0), "%;")
              )
            )
          )
        })
      )
    })
    output$official_burden_summary <- renderUI({
      nhha_data <- if (exists("sih_nhha", inherits = TRUE)) sih_nhha else data.frame()
      stress_data <- if (exists("sih_stress", inherits = TRUE)) sih_stress else data.frame()
      cost_ratio_data <- if (exists("sih_cost_ratios", inherits = TRUE)) {
        sih_cost_ratios
      } else {
        data.frame()
      }
      d <- official_burden_summary(
        sih_nhha = nhha_data,
        sih_stress = stress_data,
        sih_cost_ratios = cost_ratio_data
      )
      validate(need(nrow(d) > 0, "No official SIH/NHHA burden summary available."))

      accents <- c("blue", "teal", "navy", "purple")
      boxes <- lapply(seq_len(nrow(d)), function(i) {
        row <- d[i, ]
        policy_kpi_box(
          title = row$title,
          value = tags$span(row$formatted_value),
          subtitle = tags$p(
            paste0(row$survey_year, " | ", row$source),
            class = "kpi-subtitle"
          ),
          change = tags$p(row$subtitle, class = "kpi-subtitle"),
          accent = accents[((i - 1) %% length(accents)) + 1]
        )
      })

      do.call(layout_column_wrap, c(list(width = 1/4, fill = FALSE), boxes))
    })
    output$overview_afford_score_trend <- renderPlotly({
      d <- national_affordability_score_ts %>%
        filter(!is.na(score))
      validate(need(nrow(d) > 0, "No national affordability score data available."))

      p <- build_national_affordability_score_plot(
        d,
        selected_date = selected_score_date(),
        dark = is_dark()
      )
      dashboard_ggplotly(
        p,
        dark = is_dark(),
        tooltip = c("x", "y"),
        source = "overview_afford_score"
      ) %>%
        plotly::layout(
          margin = list(l = 44, r = 18, t = 10, b = 38),
          showlegend = FALSE,
          dragmode = FALSE
        ) %>%
        plotly::config(
          scrollZoom = FALSE,
          doubleClick = FALSE,
          modeBarButtonsToRemove = c(
            "zoom2d",
            "pan2d",
            "select2d",
            "lasso2d",
            "zoomIn2d",
            "zoomOut2d",
            "autoScale2d",
            "resetScale2d"
          )
        ) %>%
        plotly::event_register("plotly_click")
    }) %>%
      bindCache(is_dark(), selected_score_date())

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

    highest_capital_price <- reactive({
      latest_capital_price_extreme(median_house_prices, direction = "highest")
    })

    output$vb_high_capital_price <- renderText({
      d <- highest_capital_price()
      if (nrow(d) == 0 || is.na(d$value[1])) {
        return("N/A")
      }
      fmt_dollar_k(d$value[1] * 1000)
    })
    output$vb_high_capital_price_city <- renderText({
      d <- highest_capital_price()
      if (nrow(d) == 0) {
        return("")
      }
      paste0(d$city[1], " | ", format(d$date[1], "%b %Y"))
    })
    output$vb_high_capital_price_change <- renderUI({
      d <- highest_capital_price()
      if (nrow(d) == 0) {
        return(tags$p(class = "kpi-subtitle", ""))
      }
      ch <- latest_change(median_house_prices, "city", d$city[1],
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
        "Indexed to 100 at the first quarter all series cover within the selected range"
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

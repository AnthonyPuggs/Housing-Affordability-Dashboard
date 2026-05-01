# Affordability page module.

if (!exists("indicator_chart_label", mode = "function", inherits = TRUE)) {
  registry_path <- if (exists("project_path", mode = "function", inherits = TRUE)) {
    project_path("R", "indicator_registry.R")
  } else {
    file.path("R", "indicator_registry.R")
  }
  if (!file.exists(registry_path)) {
    stop("Could not locate R/indicator_registry.R for affordability module.",
         call. = FALSE)
  }
  source(registry_path, local = environment())
}

if (!exists("market_entry_scenario", mode = "function", inherits = TRUE)) {
  scenario_path <- if (exists("project_path", mode = "function", inherits = TRUE)) {
    project_path("R", "market_entry_scenarios.R")
  } else {
    file.path("R", "market_entry_scenarios.R")
  }
  if (!file.exists(scenario_path)) {
    stop("Could not locate R/market_entry_scenarios.R for affordability module.",
         call. = FALSE)
  }
  source(scenario_path, local = environment())
}

affordability_ui_indicators <- c(
  "Price-to-Income Ratio",
  "Mortgage Serviceability Index",
  "Rental Affordability Index",
  "Deposit Gap (Years)"
)

affordability_indicator_choices <- c(
  stats::setNames(affordability_ui_indicators,
                  indicator_chart_label(affordability_ui_indicators)),
  "Modelled Serviceability" = "Housing Serviceability"
)

affordabilityPageUI <- function(id) {
  ns <- NS(id)

  nav_panel(
    "Affordability",
    navset_card_tab(
      title = "Affordability Analysis",
      nav_panel(
        "Indices",
        layout_sidebar(
          sidebar = sidebar(
            width = 280, open = "desktop",
            checkboxGroupInput(ns("afford_indices"), "Indicators",
                               choices = affordability_indicator_choices,
                               selected = c(affordability_ui_indicators,
                                            "Housing Serviceability")),
            sliderInput(ns("afford_dates"), "Date Range",
                        min = min(afford_idx$date, na.rm = TRUE),
                        max = max(afford_idx$date, na.rm = TRUE),
                        value = c(as.Date("2003-01-01"),
                                  max(afford_idx$date, na.rm = TRUE)),
                        width = "100%", timeFormat = "%b %Y"),
            sliderInput(ns("serviceability_buffer"),
                        "Assessment buffer (pp)",
                        min = 0, max = 5, value = 3, step = 0.25),
            source_note("Assessment buffer and expense inputs are sensitivity assumptions, not a lender assessment.")
          ),
          card(
            card_header("Affordability Indicators"),
            source_note("Cost-pressure indexes; higher = less affordable. Market-entry measures use wage, price and rate proxies, not official ABS stress definitions."),
            card_body(div(class = "chart-wide", plotlyOutput(ns("afford_indices_chart"), height = "100%", width = "100%")))
          ),
          conditionalPanel(
            condition = "input.afford_indices.indexOf('Housing Serviceability') >= 0",
            ns = ns,
            card(
              card_header("Modelled Serviceability"),
              source_note("Modelled annual repayment share using an 80% LVR, 30-year loan and RBA mortgage-rate inputs. The 30% line is a stress reference, not a lender pass/fail rule. ", stylised_scenario_note),
              card_body(plotlyOutput(ns("afford_serviceability"), height = "380px"))
            )
          )
        )
      ),
      nav_panel(
        "Calculator",
        layout_sidebar(
          sidebar = sidebar(
            width = 320, open = "desktop",
            source_note(stylised_scenario_note),
            numericInput(ns("calc_price"), "Dwelling Price ($)",
                         value = 800000, min = 100000, max = 5000000,
                         step = 50000),
            numericInput(ns("calc_income"), "Household Gross Income ($/yr)",
                         value = 120000, min = 20000, max = 1000000,
                         step = 5000),
            sliderInput(ns("calc_rate"), "Interest Rate (%)",
                        min = 1, max = 12, value = 6.0, step = 0.1),
            sliderInput(ns("calc_assessment_buffer"),
                        "Assessment buffer (pp)",
                        min = 0, max = 5, value = 3, step = 0.25),
            sliderInput(ns("calc_deposit_pct"), "Deposit (%)",
                        min = 5, max = 40, value = 20, step = 1),
            sliderInput(ns("calc_term"), "Loan Term (years)",
                        min = 10, max = 30, value = 30, step = 1),
            sliderInput(ns("calc_savings_rate"), "Savings Rate (%)",
                        min = 5, max = 40, value = 15, step = 1),
            numericInput(ns("calc_annual_expenses"),
                         "Annual non-housing expenses ($)",
                         value = 30000, min = 0, max = 1000000,
                         step = 5000),
            numericInput(ns("calc_monthly_debt"),
                         "Other debt repayments ($/month)",
                         value = 0, min = 0, max = 50000,
                         step = 100),
            source_note("Assessment buffer and expense inputs are sensitivity assumptions, not a lender assessment.")
          ),
          layout_column_wrap(
            width = 1/2,
            fill = FALSE,
            value_box(
              title = "Monthly Repayment",
              value = textOutput(ns("calc_repayment")),
              theme = value_box_theme(bg = "#0E5A8A", fg = "#fff")
            ),
            value_box(
              title = "Nominal Repayment / Gross Income",
              value = textOutput(ns("calc_ratio")),
              theme = value_box_theme(bg = "#1F9D8C", fg = "#fff")
            ),
            value_box(
              title = "Assessed Repayment / Gross Income",
              value = textOutput(ns("calc_assessed_ratio")),
              theme = value_box_theme(bg = "#5B6C8F", fg = "#fff")
            ),
            value_box(
              title = "Years to Save Deposit",
              value = textOutput(ns("calc_years")),
              theme = value_box_theme(bg = "#3B4C7A", fg = "#fff")
            ),
            value_box(
              title = "Loan-to-Value Ratio",
              value = textOutput(ns("calc_lvr")),
              theme = value_box_theme(bg = "#326273", fg = "#fff")
            ),
            value_box(
              title = "Total Interest Paid",
              value = textOutput(ns("calc_total_interest")),
              theme = value_box_theme(bg = "#984ea3", fg = "#fff")
            ),
            value_box(
              title = "Deposit Amount",
              value = textOutput(ns("calc_deposit_amt")),
              theme = value_box_theme(bg = "#17415F", fg = "#fff")
            )
          )
        )
      ),
      nav_panel(
        "Housing Stress",
        layout_sidebar(
          sidebar = sidebar(
            width = 280, open = "desktop",
            selectInput(ns("stress_breakdown"), "Breakdown By",
                        choices = c("Age Group" = "age_group",
                                    "Family Type" = "family_type",
                                    "Income Quintile" = "equiv_income_quintile",
                                    "Dwelling Structure" = "dwelling_structure",
                                    "By Tenure (Owners)" = "owner",
                                    "By Tenure (Renters)" = "renter")),
            radioButtons(ns("stress_population"), "Population",
                         choices = c("All Households" = "all_households",
                                     "Lower Income (Bottom 40%)" = "lower_income"))
          ),
          card(
            card_header("Housing Cost Stress Bands (2019-20)"),
            source_note("ABS Survey of Income and Housing. Official survey-based housing cost burden bands by household group. ", sih_sampling_error_note),
            card_body(div(class = "chart-square", plotlyOutput(ns("stress_chart"), height = "100%", width = "100%")))
          )
        )
      ),
      nav_panel(
        "Cost Burden",
        layout_sidebar(
          sidebar = sidebar(
            width = 280, open = "desktop",
            selectInput(ns("burden_breakdown"), "Breakdown By",
                        choices = c("Age Group" = "age_group",
                                    "Family Type" = "family_type",
                                    "Income Quintile" = "equiv_income_quintile")),
            radioButtons(ns("burden_stat"), "Statistic",
                         choices = c("Mean" = "mean", "Median" = "median"))
          ),
          card(
            card_header("Housing Cost-to-Income Ratio by Tenure & Demographics (2019-20)"),
            source_note("ABS Survey of Income and Housing. Gross-income housing cost ratios by tenure and demographic group. ", sih_sampling_error_note),
            card_body(div(class = "chart-square", plotlyOutput(ns("burden_heatmap"), height = "100%", width = "100%")))
          )
        )
      )
    )
  )
}

affordabilityPageServer <- function(id, is_dark) {
  moduleServer(id, function(input, output, session) {
    output$afford_indices_chart <- renderPlotly({
      req(input$afford_indices)
      idx_selected <- setdiff(input$afford_indices, "Housing Serviceability")
      validate(need(length(idx_selected) > 0,
                    "Select at least one index indicator (or Housing Serviceability)."))
      d <- afford_idx %>%
        filter(indicator %in% idx_selected,
               date >= input$afford_dates[1],
               date <= input$afford_dates[2]) %>%
        mutate(indicator_label = indicator_chart_label(indicator))
      validate(need(nrow(d) > 0, "No data for selected indicators in this date range."))

      p <- ggplot(d, aes(x = date, y = value, color = indicator_label)) +
        geom_line(linewidth = 1) +
        facet_wrap(~indicator_label, scales = "free_y") +
        scale_x_date(date_labels = "%Y", date_breaks = "5 years") +
        labs(x = NULL, y = NULL, color = NULL) +
        theme_afford(is_dark()) +
        theme(legend.position = "none")

      dashboard_ggplotly(p, dark = is_dark(), tooltip = c("x", "y"))
    }) %>%
      bindCache(input$afford_indices, input$afford_dates, is_dark())

    output$afford_serviceability <- renderPlotly({
      req("Housing Serviceability" %in% input$afford_indices)
      d <- market_entry_serviceability_series(
        price_ts = rppi_national_ts,
        income_ts = awe_ts,
        rate_ts = mortgage_rate_qtr,
        assessment_buffer_pp = input$serviceability_buffer
      ) %>%
        filter(!is.na(serviceability_pct),
               date >= input$afford_dates[1],
               date <= input$afford_dates[2])

      validate(need(nrow(d) > 0, "No serviceability data in this date range."))

      p <- ggplot(d, aes(x = date, y = serviceability_pct,
                         color = scenario)) +
        geom_line(linewidth = 1.1) +
        geom_hline(yintercept = 30, linetype = "dashed", color = "#FF9800",
                   linewidth = 0.8) +
        annotate("text", x = max(d$date) - 2500, y = 31,
                 label = "30% stress reference",
                 color = "#FF9800", size = 3.5, hjust = 0, vjust = 0) +
        scale_color_manual(values = c("Nominal rate" = "#0E5A8A",
                                      "Assessed rate" = "#e53935")) +
        scale_x_date(date_labels = "%Y", date_breaks = "3 years") +
        scale_y_continuous(labels = label_percent(scale = 1, accuracy = 0.1)) +
        labs(x = NULL, y = NULL, color = NULL) +
        theme_afford(is_dark())

      dashboard_ggplotly(p, dark = is_dark(), tooltip = c("x", "y", "color"))
    }) %>%
      bindCache(input$afford_indices, input$afford_dates, input$serviceability_buffer, is_dark())

    calc_vals <- reactive({
      tryCatch(
        market_entry_scenario(
          dwelling_price = input$calc_price,
          gross_annual_income = input$calc_income,
          annual_rate_pct = input$calc_rate,
          deposit_pct = input$calc_deposit_pct,
          term_years = input$calc_term,
          savings_rate_pct = input$calc_savings_rate,
          assessment_buffer_pp = input$calc_assessment_buffer,
          annual_non_housing_expenses = input$calc_annual_expenses,
          monthly_other_debt = input$calc_monthly_debt
        ),
        error = function(e) {
          validate(need(FALSE, conditionMessage(e)))
        }
      )
    })

    output$calc_repayment      <- renderText(fmt_dollar(calc_vals()$monthly_nominal_repayment))
    output$calc_ratio          <- renderText(fmt_pct(calc_vals()$nominal_repayment_to_gross_income_pct, 0.1))
    output$calc_assessed_ratio <- renderText(fmt_pct(calc_vals()$assessed_repayment_to_gross_income_pct, 0.1))
    output$calc_years          <- renderText(fmt_years(calc_vals()$years_to_save_deposit))
    output$calc_lvr            <- renderText(fmt_pct(calc_vals()$lvr_pct, 1))
    output$calc_total_interest <- renderText(fmt_dollar(calc_vals()$total_nominal_interest))
    output$calc_deposit_amt    <- renderText(fmt_dollar(calc_vals()$deposit))

    output$stress_chart <- renderPlotly({
      bd <- input$stress_breakdown
      pop <- input$stress_population

      if (bd %in% c("owner", "renter")) {
        d <- sih_stress %>%
          filter(breakdown_var == bd,
                 stat_type == pop,
                 metric %in% c("pct_25_or_less", "pct_25_to_30",
                               "pct_30_to_50", "pct_over_50"),
                 breakdown_val != "Total")
      } else {
        d <- sih_stress %>%
          filter(breakdown_var == bd,
                 stat_type == pop,
                 tenure == "all",
                 metric %in% c("pct_25_or_less", "pct_25_to_30",
                               "pct_30_to_50", "pct_over_50"),
                 breakdown_val != "Total")
      }

      validate(need(nrow(d) > 0, "No data for selected filters."))

      d <- d %>%
        mutate(stress_band = case_when(
          metric == "pct_25_or_less" ~ "<25%",
          metric == "pct_25_to_30"   ~ "25-30%",
          metric == "pct_30_to_50"   ~ "30-50%",
          metric == "pct_over_50"    ~ ">50%"
        )) %>%
        mutate(stress_band = factor(stress_band,
                                    levels = c("<25%", "25-30%", "30-50%", ">50%")))

      stress_cols <- c("<25%" = "#2ecc71", "25-30%" = "#f39c12",
                       "30-50%" = "#e74c3c", ">50%" = "#8e44ad")

      p <- ggplot(d, aes(x = breakdown_val, y = value, fill = stress_band)) +
        geom_col(position = "stack", alpha = 0.9) +
        scale_fill_manual(values = stress_cols) +
        labs(x = NULL, y = "% of Households", fill = "Cost/Income") +
        coord_flip() +
        theme_afford(is_dark())

      dashboard_ggplotly(p, dark = is_dark(), tooltip = c("x", "y", "fill"))
    }) %>%
      bindCache(input$stress_breakdown, input$stress_population, is_dark())

    output$burden_heatmap <- renderPlotly({
      bd <- input$burden_breakdown
      st <- input$burden_stat

      d <- sih_cost_ratios %>%
        filter(breakdown_var == bd,
               stat_type == st,
               breakdown_val != "Total",
               tenure %in% c("owner_mortgage", "renter_private",
                             "renter_total", "all"))

      validate(need(nrow(d) > 0, "No cost-to-income ratio data for selected filters."))

      d <- d %>% mutate(tenure_label = label_tenure(tenure))

      p <- ggplot(d, aes(x = tenure_label, y = breakdown_val, fill = value)) +
        geom_tile(color = "white", linewidth = 0.5) +
        geom_text(aes(label = round(value, 1)), size = 3.5) +
        scale_fill_gradient2(low = "#2ecc71", mid = "#f39c12", high = "#e74c3c",
                             midpoint = 25, name = "Cost/Income %") +
        labs(x = NULL, y = NULL) +
        theme_afford(is_dark()) +
        theme(axis.text.x = element_text(angle = 30, hjust = 1))

      dashboard_ggplotly(p, dark = is_dark(), tooltip = c("x", "y", "fill"))
    }) %>%
      bindCache(input$burden_breakdown, input$burden_stat, is_dark())
  })
}

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

if (!exists("join_sih_quality", mode = "function", inherits = TRUE)) {
  sih_quality_helper_path <- if (exists("project_path", mode = "function", inherits = TRUE)) {
    project_path("R", "sih_quality_helpers.R")
  } else {
    file.path("R", "sih_quality_helpers.R")
  }
  if (!file.exists(sih_quality_helper_path)) {
    stop("Could not locate R/sih_quality_helpers.R for affordability module.",
         call. = FALSE)
  }
  source(sih_quality_helper_path, local = environment())
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

distributional_stress_data <- function(measure = "nhha_state",
                                       tenure = "renter_lower_income",
                                       group = "state",
                                       quality = NULL) {
  if (identical(measure, "nhha_state")) {
    d <- sih_nhha %>%
      filter(metric == "pct_rental_stress_over_30",
             tenure == "renter_lower_income",
             breakdown_val == "Total",
             geography != "Aust.") %>%
      group_by(geography) %>%
      filter(survey_year == max(survey_year)) %>%
      ungroup()
    measure_label <- "Official SIH/NHHA burden measure: lower-income renters paying >30% of income"
    group_label <- d$geography
  } else if (identical(measure, "lower_income_state")) {
    d <- sih_lower_income_states %>%
      filter(metric == "pct_over_30",
             tenure == tenure,
             geography != "Australia") %>%
      group_by(geography) %>%
      filter(survey_year == max(survey_year)) %>%
      ungroup()
    measure_label <- "Official SIH lower-income burden measure: households paying >30% of income"
    group_label <- d$geography
  } else {
    breakdown <- if (group %in% c("age_group", "family_type",
                                  "equiv_income_quintile")) {
      group
    } else {
      "equiv_income_quintile"
    }
    d <- sih_cost_ratios %>%
      filter(metric == "cost_income_ratio",
             tenure == tenure,
             breakdown_var == breakdown,
             stat_type == "mean",
             breakdown_val != "Total")
    measure_label <- "Official SIH cost-to-income burden measure by household group"
    group_label <- d$breakdown_val
  }

  if (nrow(d) == 0) {
    return(d)
  }

  d <- d %>%
    join_sih_quality(quality) %>%
    mutate(
      group_label = group_label,
      measure_label = measure_label,
      measure_class = "official_survey",
      reliability_marker = sih_reliability_marker(rse_reliability_flag),
      quality_hover = sih_quality_hover_text(
        rse_pct,
        moe_95,
        rse_reliability_flag
      ),
      hover_text = paste0(
        group_label,
        "<br>Survey year: ", survey_year,
        "<br>Value: ", number(value, accuracy = 0.1), "%",
        "<br>", quality_hover,
        ifelse(nzchar(interval_label), paste0("<br>", interval_label), ""),
        "<br>Official SIH/NHHA burden measure; not modelled market-entry."
      )
    )

  d
}

affordabilityPageUI <- function(id) {
  ns <- NS(id)

  nav_panel(
    "Affordability",
    policy_page_header(
      "Affordability",
      "Cost-pressure indexes, stylised market-entry scenarios and SIH survey burden measures."
    ),
    navset_card_tab(
      title = "Affordability Analysis",
      nav_panel(
        "Indices",
        div(
          class = "affordability-indices-page",
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
              sliderInput(ns("serviceability_deposit_pct"),
                          "Deposit (%)",
                          min = 5, max = 40, value = 20, step = 1),
              sliderInput(ns("serviceability_term"),
                          "Loan term (years)",
                          min = 10, max = 30, value = 30, step = 1),
              source_note("Assessment buffer and expense inputs are sensitivity assumptions, not a lender assessment.")
            ),
            policy_chart_card(
              "Affordability Indicators",
              note = "Cost-pressure indexes; higher = less affordable. Market-entry measures use wage, price and rate proxies, not official ABS stress definitions.",
              div(class = "chart-wide",
                  plotlyOutput(ns("afford_indices_chart"), height = "100%", width = "100%"))
            ),
            conditionalPanel(
              condition = "input.afford_indices.indexOf('Housing Serviceability') >= 0",
              ns = ns,
              policy_chart_card(
                "Modelled Serviceability",
                note = "Modelled annual repayment share using selected deposit, implied LVR, loan term and RBA mortgage-rate inputs; uses AWE individual earnings as the income proxy. The 30% line is a stress reference, not a lender pass/fail rule. Stylised scenario, not an official ABS measure or lender assessment.",
                plotlyOutput(ns("afford_serviceability"), height = "380px")
              )
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
            selectInput(ns("calc_preset"), "Scenario preset",
                        choices = c("First-home buyer" = "first_home_buyer",
                                    "Mortgage-stress" = "mortgage_stress",
                                    "Renter entry" = "renter_entry"),
                        selected = "first_home_buyer"),
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
            policy_kpi_box(
              title = "Monthly Repayment",
              value = textOutput(ns("calc_repayment")),
              accent = "blue"
            ),
            policy_kpi_box(
              title = "Nominal Repayment / Gross Income",
              value = textOutput(ns("calc_ratio")),
              accent = "teal"
            ),
            policy_kpi_box(
              title = "Assessed Repayment / Gross Income",
              value = textOutput(ns("calc_assessed_ratio")),
              accent = "navy"
            ),
            policy_kpi_box(
              title = "Years to Save Deposit",
              value = textOutput(ns("calc_years")),
              accent = "purple"
            ),
            policy_kpi_box(
              title = "Loan-to-Value Ratio",
              value = textOutput(ns("calc_lvr")),
              accent = "blue"
            ),
            policy_kpi_box(
              title = "Total Interest Paid",
              value = textOutput(ns("calc_total_interest")),
              accent = "teal"
            ),
            policy_kpi_box(
              title = "Deposit Amount",
              value = textOutput(ns("calc_deposit_amt")),
              accent = "navy"
            )
          ),
          policy_chart_card(
            "Scenario Sensitivity",
            note = "Stylised sensitivity chart. Higher values mean a larger expense-adjusted repayment burden; it is not an official ABS/NHHA measure or lender assessment.",
            plotlyOutput(ns("calc_sensitivity"), height = "360px")
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
          policy_chart_card(
            "Housing Cost Stress Bands (2019-20)",
            note = policy_source_note("ABS Survey of Income and Housing. Official survey-based housing cost burden bands by household group. ", sih_sampling_error_note),
            div(class = "chart-square",
                plotlyOutput(ns("stress_chart"), height = "100%", width = "100%"))
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
          policy_chart_card(
            "Housing Cost-to-Income Ratio by Tenure & Demographics (2019-20)",
            note = policy_source_note("ABS Survey of Income and Housing. Gross-income housing cost ratios by tenure and demographic group. ", sih_sampling_error_note),
            div(class = "chart-square",
                plotlyOutput(ns("burden_heatmap"), height = "100%", width = "100%"))
          )
        )
      ),
      nav_panel(
        "Distributional Stress Explorer",
        layout_sidebar(
          sidebar = sidebar(
            width = 300, open = "desktop",
            selectInput(ns("dist_measure"), "Official measure",
                        choices = c(
                          "NHHA lower-income renter stress by state" = "nhha_state",
                          "Lower-income housing stress by state" = "lower_income_state",
                          "Cost-to-income by household group" = "cost_income_demographic"
                        ),
                        selected = "nhha_state"),
            selectInput(ns("dist_tenure"), "Tenure",
                        choices = c("Lower-income renters" = "renter_lower_income",
                                    "Private renters" = "renter_private",
                                    "Mortgage owners" = "owner_mortgage",
                                    "All households" = "all"),
                        selected = "renter_lower_income"),
            selectInput(ns("dist_group"), "Group",
                        choices = c("State/Territory" = "state",
                                    "Income quintile" = "equiv_income_quintile",
                                    "Age group" = "age_group",
                                    "Family type" = "family_type"),
                        selected = "state"),
            source_note("Official SIH/NHHA burden measure. These estimates are not modelled market-entry indexes.")
          ),
          policy_chart_card(
            "Distributional Stress Explorer",
            note = policy_source_note("Official SIH/NHHA burden measure with SIH reliability markers where available. This surface is not modelled market-entry analysis. ", sih_sampling_error_note),
            div(class = "chart-square",
                plotlyOutput(ns("distributional_stress"),
                             height = "100%", width = "100%"))
          )
        )
      )
    )
  )
}

affordabilityPageServer <- function(id, is_dark) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$calc_preset, {
      presets <- market_entry_scenario_presets()
      preset <- presets[presets$preset_id == input$calc_preset, , drop = FALSE]
      if (nrow(preset) != 1) {
        return(NULL)
      }

      updateNumericInput(session, "calc_price",
                         value = preset$dwelling_price[[1]])
      updateNumericInput(session, "calc_income",
                         value = preset$gross_annual_income[[1]])
      updateSliderInput(session, "calc_rate",
                        value = preset$annual_rate_pct[[1]])
      updateSliderInput(session, "calc_assessment_buffer",
                        value = preset$assessment_buffer_pp[[1]])
      updateSliderInput(session, "calc_deposit_pct",
                        value = preset$deposit_pct[[1]])
      updateSliderInput(session, "calc_term",
                        value = preset$term_years[[1]])
      updateSliderInput(session, "calc_savings_rate",
                        value = preset$savings_rate_pct[[1]])
      updateNumericInput(session, "calc_annual_expenses",
                         value = preset$annual_non_housing_expenses[[1]])
      updateNumericInput(session, "calc_monthly_debt",
                         value = preset$monthly_other_debt[[1]])
    }, ignoreInit = TRUE)

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

      p <- build_affordability_indices_plot(d, dark = is_dark())

      dashboard_ggplotly(p, dark = is_dark(), tooltip = c("x", "y"))
    }) %>%
      bindCache(input$afford_indices, input$afford_dates, is_dark())

    output$afford_serviceability <- renderPlotly({
      req("Housing Serviceability" %in% input$afford_indices)
      d <- market_entry_serviceability_series(
        price_ts = rppi_national_ts,
        income_ts = awe_ts,
        rate_ts = mortgage_rate_qtr,
        deposit_pct = input$serviceability_deposit_pct,
        term_years = input$serviceability_term,
        assessment_buffer_pp = input$serviceability_buffer
      ) %>%
        filter(!is.na(serviceability_pct),
               date >= input$afford_dates[1],
               date <= input$afford_dates[2])

      validate(need(nrow(d) > 0, "No serviceability data in this date range."))

      p <- build_market_entry_serviceability_plot(d, dark = is_dark())

      dashboard_ggplotly(p, dark = is_dark(), tooltip = c("x", "y", "color"))
    }) %>%
      bindCache(input$afford_indices, input$afford_dates, input$serviceability_deposit_pct, input$serviceability_term, input$serviceability_buffer, is_dark())

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

    output$calc_sensitivity <- renderPlotly({
      d <- market_entry_sensitivity_grid(
        dwelling_price = input$calc_price,
        gross_annual_income = input$calc_income,
        annual_rate_pct = input$calc_rate,
        deposit_pct = input$calc_deposit_pct,
        term_years = input$calc_term,
        savings_rate_pct = input$calc_savings_rate,
        assessment_buffer_pp = input$calc_assessment_buffer,
        annual_non_housing_expenses = input$calc_annual_expenses,
        monthly_other_debt = input$calc_monthly_debt
      )
      validate(need(nrow(d) > 0, "No scenario sensitivity data available."))

      p <- build_market_entry_sensitivity_plot(d, dark = is_dark())

      dashboard_ggplotly(p, dark = is_dark(), tooltip = c("x", "y"))
    }) %>%
      bindCache(input$calc_price, input$calc_income, input$calc_rate,
                input$calc_deposit_pct, input$calc_term,
                input$calc_savings_rate, input$calc_assessment_buffer,
                input$calc_annual_expenses, input$calc_monthly_debt,
                is_dark())

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
        join_sih_quality(sih_quality) %>%
        mutate(stress_band = case_when(
          metric == "pct_25_or_less" ~ "<25%",
          metric == "pct_25_to_30"   ~ "25-30%",
          metric == "pct_30_to_50"   ~ "30-50%",
          metric == "pct_over_50"    ~ ">50%"
        )) %>%
        mutate(
          reliability_marker = sih_reliability_marker(rse_reliability_flag),
          quality_hover = sih_quality_hover_text(
            rse_pct,
            moe_95,
            rse_reliability_flag
          ),
          hover_text = paste0(
            breakdown_val,
            "<br>Band: ", stress_band,
            "<br>Share: ", number(value, accuracy = 0.1), "%",
            "<br>", quality_hover,
            ifelse(nzchar(interval_label), paste0("<br>", interval_label), "")
          )
        ) %>%
        mutate(stress_band = factor(stress_band,
                                    levels = c("<25%", "25-30%", "30-50%", ">50%")))

      p <- build_housing_stress_bands_plot(d, dark = is_dark())

      dashboard_ggplotly(p, dark = is_dark(), tooltip = c("x", "y", "fill", "text"))
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

      p <- build_cost_burden_heatmap_plot(d, dark = is_dark())

      dashboard_ggplotly(p, dark = is_dark(), tooltip = c("x", "y", "fill"))
    }) %>%
      bindCache(input$burden_breakdown, input$burden_stat, is_dark())

    output$distributional_stress <- renderPlotly({
      d <- distributional_stress_data(
        measure = input$dist_measure,
        tenure = input$dist_tenure,
        group = input$dist_group,
        quality = sih_quality
      )
      validate(need(nrow(d) > 0, "No official SIH/NHHA distributional stress data for selected filters."))

      p <- build_distributional_stress_plot(d, dark = is_dark())

      dashboard_ggplotly(p, dark = is_dark(), tooltip = c("x", "y", "text"))
    }) %>%
      bindCache(input$dist_measure, input$dist_tenure, input$dist_group,
                is_dark())
  })
}

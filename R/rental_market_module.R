# Rental Market page module.

if (!exists("join_sih_quality", mode = "function", inherits = TRUE)) {
  sih_quality_helper_path <- if (exists("project_path", mode = "function", inherits = TRUE)) {
    project_path("R", "sih_quality_helpers.R")
  } else {
    file.path("R", "sih_quality_helpers.R")
  }
  if (!file.exists(sih_quality_helper_path)) {
    stop("Could not locate R/sih_quality_helpers.R for rental market module.",
         call. = FALSE)
  }
  source(sih_quality_helper_path, local = environment())
}

rental_plot_margins <- list(
  state = list(l = 82, r = 20, b = 76, t = 38),
  trend = list(l = 86, r = 20, b = 92, t = 32),
  index = list(l = 74, r = 20, b = 72, t = 30),
  costs = list(l = 150, r = 20, b = 78, t = 30)
)

normalise_rental_states <- function(selected, geographies) {
  available <- sort(setdiff(unique(geographies), "Aust."))
  selected <- selected[!is.na(selected) & nzchar(selected)]

  if (length(selected) == 0 || identical(selected, "all")) {
    return(available)
  }

  explicit <- selected[selected != "all"]
  explicit <- explicit[explicit %in% available]

  if (length(explicit) > 0) {
    return(explicit)
  }

  available
}

rentalMarketPageUI <- function(id) {
  ns <- NS(id)

  nav_panel(
    "Rental Market",
    div(class = "rental-market-page",
      layout_sidebar(
        sidebar = sidebar(
          width = 280, open = "open",
          selectInput(ns("rental_year"), "Survey Year (NHHA)",
                      choices = if (nrow(sih_nhha) > 0)
                        rev(sort(unique(sih_nhha$survey_year))) else "2019-20",
                      selected = "2019-20"),
          selectInput(ns("rental_states"), "States/Territories",
                      choices = c("All" = "all",
                                  if (nrow(sih_nhha) > 0)
                                    sort(unique(sih_nhha$geography[
                                      sih_nhha$geography != "Aust."]))
                                  else character(0)),
                      multiple = TRUE,
                      selected = "all"),
          selectInput(ns("rental_cost_breakdown"), "Rental Costs By",
                      choices = c("Age Group" = "age_group",
                                  "Family Type" = "family_type",
                                  "Income Quintile" = "equiv_income_quintile"))
        ),
        div(class = "rental-market-grid",
          layout_column_wrap(
            width = "420px",
            card(
              card_header("NHHA Rental Stress by State"),
              source_note("ABS Survey of Income and Housing, NHHA lower-income renter stress. Official survey burden/stress measure. ", sih_sampling_error_note),
              card_body(div(class = "chart-square rental-market-chart rental-market-chart-square", plotlyOutput(ns("rental_stress_state"), height = "100%", width = "100%")))
            ),
            card(
              card_header("NHHA Rental Stress Trends (Over Time)"),
              source_note("ABS Survey of Income and Housing, NHHA lower-income renter stress. Values are proportions of lower-income renter households. ", sih_sampling_error_note),
              card_body(div(class = "chart-wide rental-market-chart rental-market-chart-wide rental-market-chart-trend", plotlyOutput(ns("rental_stress_trend"), height = "100%", width = "100%")))
            ),
            card(
              card_header("Rental Affordability Index"),
              source_note("Cost-pressure index using ABS CPI rents and WPI; higher = less affordable."),
              card_body(div(class = "chart-wide rental-market-chart rental-market-chart-wide", plotlyOutput(ns("rental_afford_index"), height = "100%", width = "100%")))
            ),
            card(
              card_header("Weekly Rental Costs by Demographics (2019-20)"),
              source_note("ABS Survey of Income and Housing. Survey rental-cost estimates by household characteristic. ", sih_sampling_error_note),
              card_body(div(class = "chart-square rental-market-chart rental-market-chart-square", plotlyOutput(ns("rental_costs_demo"), height = "100%", width = "100%")))
            )
          )
        )
      )
    )
  )
}

rentalMarketPageServer <- function(id, is_dark) {
  moduleServer(id, function(input, output, session) {
    output$rental_stress_state <- renderPlotly({
      yr <- input$rental_year
      selected_states <- normalise_rental_states(input$rental_states,
                                                 sih_nhha$geography)

      d <- sih_nhha %>%
        filter(survey_year == yr,
               metric == "pct_rental_stress_over_30",
               geography %in% selected_states) %>%
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
            "<br>Rental stress: ", number(value, accuracy = 0.1), "%",
            "<br>", quality_hover,
            ifelse(nzchar(interval_label), paste0("<br>", interval_label), "")
          )
        )

      validate(need(nrow(d) > 0, "No NHHA rental stress data for selected year."))

      nat <- sih_nhha %>%
        filter(survey_year == yr,
               metric == "pct_rental_stress_over_30",
               geography == "Aust.")

      p <- build_rental_stress_state_plot(
        d,
        national_value = if (nrow(nat) > 0) nat$value[1] else NULL,
        dark = is_dark()
      )

      dashboard_ggplotly(p, dark = is_dark(), tooltip = c("x", "y", "text"),
                         margin = rental_plot_margins$state)
    }) %>%
      bindCache(input$rental_year, input$rental_states, is_dark())

    output$rental_stress_trend <- renderPlotly({
      states <- c("Aust.",
                  normalise_rental_states(input$rental_states,
                                          sih_nhha$geography))

      d <- sih_nhha %>%
        filter(metric == "pct_rental_stress_over_30",
               geography %in% states) %>%
        join_sih_quality(sih_quality) %>%
        mutate(
          reliability_marker = sih_reliability_marker(rse_reliability_flag),
          quality_hover = sih_quality_hover_text(
            rse_pct,
            moe_95,
            rse_reliability_flag
          ),
          geography = factor(geography,
            levels = rev(c("Aust.", sort(setdiff(unique(geography), "Aust."))))),
          tile_label = paste0(sprintf("%.0f", value), reliability_marker),
          hover_text = paste0(
            geography,
            "<br>Survey year: ", survey_year,
            "<br>Rental stress: ", number(value, accuracy = 0.1), "%",
            "<br>", quality_hover,
            ifelse(nzchar(interval_label), paste0("<br>", interval_label), "")
          ),
          tile_text_colour = ifelse(value >= 32 & value <= 48,
                                    "#172033", "#F8FAFC")
        )

      validate(need(nrow(d) > 0, "No NHHA trend data."))

      dark <- is_dark()
      p <- build_rental_stress_trend_plot(d, dark = dark)

      pl <- dashboard_ggplotly(p, dark = dark, tooltip = c("fill", "text"),
                               margin = rental_plot_margins$trend)
      for (i in seq_along(pl$x$data)) {
        if (!is.null(pl$x$data[[i]]$mode) && grepl("text", pl$x$data[[i]]$mode)) {
          pl$x$data[[i]]$hoverinfo <- "skip"
        }
      }
      pl
    }) %>%
      bindCache(input$rental_states, is_dark())

    output$rental_afford_index <- renderPlotly({
      d <- afford_idx %>%
        filter(indicator == "Rental Affordability Index")
      validate(need(nrow(d) > 0, "No rental affordability index data."))

      p <- build_rental_affordability_index_plot(d, dark = is_dark())

      dashboard_ggplotly(p, dark = is_dark(), tooltip = c("x", "y"),
                         margin = rental_plot_margins$index)
    }) %>%
      bindCache(is_dark())

    output$rental_costs_demo <- renderPlotly({
      bd <- input$rental_cost_breakdown

      d <- sih_costs %>%
        filter(tenure %in% c("renter_private", "renter_total"),
               breakdown_var == bd,
               stat_type == "mean",
               breakdown_val != "Total")

      validate(need(nrow(d) > 0, "No rental cost data for selected breakdown."))

      d <- d %>%
        mutate(
          tenure_label = label_tenure(tenure),
          breakdown_label = stringr::str_wrap(breakdown_val, width = 22)
        )

      p <- build_rental_costs_demographic_plot(d, dark = is_dark())

      dashboard_ggplotly(p, dark = is_dark(), tooltip = c("x", "y", "fill"),
                         margin = rental_plot_margins$costs)
    }) %>%
      bindCache(input$rental_cost_breakdown, is_dark())
  })
}

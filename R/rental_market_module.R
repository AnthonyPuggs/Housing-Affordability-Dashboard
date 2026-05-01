# Rental Market page module.

rentalMarketPageUI <- function(id) {
  ns <- NS(id)

  nav_panel(
    "Rental Market",
    layout_sidebar(
      sidebar = sidebar(
        width = 280, open = "desktop",
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
      layout_column_wrap(
        width = "420px",
        card(
          card_header("NHHA Rental Stress by State"),
          source_note("ABS Survey of Income and Housing, NHHA lower-income renter stress. Official survey burden/stress measure. ", sih_sampling_error_note),
          card_body(div(class = "chart-square", plotlyOutput(ns("rental_stress_state"), height = "100%", width = "100%")))
        ),
        card(
          card_header("NHHA Rental Stress Trends (Over Time)"),
          source_note("ABS Survey of Income and Housing, NHHA lower-income renter stress. Values are proportions of lower-income renter households. ", sih_sampling_error_note),
          card_body(div(class = "chart-wide", plotlyOutput(ns("rental_stress_trend"), height = "100%", width = "100%")))
        ),
        card(
          card_header("Rental Affordability Index"),
          source_note("Cost-pressure index using ABS CPI rents and WPI; higher = less affordable."),
          card_body(div(class = "chart-wide", plotlyOutput(ns("rental_afford_index"), height = "100%", width = "100%")))
        ),
        card(
          card_header("Weekly Rental Costs by Demographics (2019-20)"),
          source_note("ABS Survey of Income and Housing. Survey rental-cost estimates by household characteristic. ", sih_sampling_error_note),
          card_body(div(class = "chart-square", plotlyOutput(ns("rental_costs_demo"), height = "100%", width = "100%")))
        )
      )
    )
  )
}

rentalMarketPageServer <- function(id, is_dark) {
  moduleServer(id, function(input, output, session) {
    output$rental_stress_state <- renderPlotly({
      yr <- input$rental_year

      d <- sih_nhha %>%
        filter(survey_year == yr,
               metric == "pct_rental_stress_over_30",
               geography != "Aust.")

      if (!is.null(input$rental_states) && !"all" %in% input$rental_states) {
        d <- d %>% filter(geography %in% input$rental_states)
      }

      validate(need(nrow(d) > 0, "No NHHA rental stress data for selected year."))

      nat <- sih_nhha %>%
        filter(survey_year == yr,
               metric == "pct_rental_stress_over_30",
               geography == "Aust.")

      p <- ggplot(d, aes(x = reorder(geography, -value), y = value)) +
        geom_col(fill = "#e74c3c", alpha = 0.85, width = 0.7) +
        {if (nrow(nat) > 0) geom_hline(yintercept = nat$value[1],
                                       linetype = "dashed", color = "#333")} +
        labs(x = NULL, y = "% in Rental Stress (>30% of income)") +
        theme_afford(is_dark())

      dashboard_ggplotly(p, dark = is_dark(), tooltip = c("x", "y"))
    }) %>%
      bindCache(input$rental_year, input$rental_states, is_dark())

    output$rental_stress_trend <- renderPlotly({
      states <- if (is.null(input$rental_states) || "all" %in% input$rental_states) {
        unique(sih_nhha$geography)
      } else {
        c(input$rental_states, "Aust.")
      }

      d <- sih_nhha %>%
        filter(metric == "pct_rental_stress_over_30",
               geography %in% states) %>%
        mutate(
          geography = factor(geography,
            levels = rev(c("Aust.", sort(setdiff(unique(geography), "Aust.")))))
        )

      validate(need(nrow(d) > 0, "No NHHA trend data."))

      dark <- is_dark()
      text_col <- if (dark) "#E3EBF4" else "#1F2D3D"

      p <- ggplot(d, aes(x = survey_year, y = geography, fill = value)) +
        geom_tile(color = if (dark) "#1B2A44" else "#FFFFFF", linewidth = 1.5) +
        geom_text(aes(label = sprintf("%.0f%%", value)),
                  size = 3.2, color = text_col, show.legend = FALSE) +
        scale_fill_gradient2(
          low = "#2196F3", mid = "#FFB74D", high = "#e74c3c",
          midpoint = 40, limits = c(10, 60),
          name = "% in Stress"
        ) +
        labs(x = NULL, y = NULL) +
        theme_afford(dark) +
        theme(
          panel.grid = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1)
        )

      pl <- dashboard_ggplotly(p, dark = dark, tooltip = "fill")
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

      p <- ggplot(d, aes(x = date, y = value)) +
        geom_line(linewidth = 1, color = "#e74c3c") +
        scale_x_date(date_labels = "%Y", date_breaks = "5 years") +
        labs(x = NULL, y = "Index (CPI Rents / WPI)") +
        theme_afford(is_dark())

      dashboard_ggplotly(p, dark = is_dark(), tooltip = c("x", "y"))
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

      d <- d %>% mutate(tenure_label = label_tenure(tenure))

      p <- ggplot(d, aes(x = breakdown_val, y = value, fill = tenure_label)) +
        geom_col(position = "dodge", alpha = 0.85) +
        labs(x = NULL, y = "Mean Weekly Rent ($)", fill = NULL) +
        coord_flip() +
        theme_afford(is_dark())

      dashboard_ggplotly(p, dark = is_dark(), tooltip = c("x", "y", "fill"))
    }) %>%
      bindCache(input$rental_cost_breakdown, is_dark())
  })
}

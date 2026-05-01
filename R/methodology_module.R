# Methodology page module.

methodologyPageUI <- function(id) {
  ns <- NS(id)

  nav_panel("Methodology",
    div(
      class = "d-flex justify-content-between align-items-start mb-3 px-2",
      div(
        tags$h3("Methodology & Provenance", class = "mb-1",
                style = "font-weight: 700;"),
        tags$p("Indicator definitions, source series and interpretation caveats",
               style = "color: var(--app-muted); margin-bottom: 0;")
      ),
      div(
        class = "methodology-download",
        downloadButton(ns("provenance_download"),
          "Download Methodology Summary",
          class = "btn btn-outline-primary btn-sm"
        )
      )
    ),
    layout_column_wrap(
      width = 1/3,
      fill = FALSE,
      card(
        card_header("Official SIH/NHHA burden measures"),
        card_body(
          tags$p("ABS Survey of Income and Housing measures describe observed household housing costs, gross-income cost ratios and NHHA lower-income renter stress."),
          tags$p("These are official survey burden and stress measures, separate from price-index or market-entry proxy indicators."),
          tags$p(sih_sampling_error_note)
        )
      ),
      card(
        card_header("Market-entry cost-pressure indexes"),
        card_body(
          tags$p("Derived dashboard indicators use prices, wages, rents and rates to summarise cost pressure over time."),
          tags$p("Higher values generally mean less affordable unless the table states otherwise. These are analytical indexes, not official ABS affordability measures.")
        )
      ),
      card(
        card_header("Stylised scenario calculators"),
        card_body(
          tags$p("Serviceability, deposit-gap and calculator outputs use fixed modelling assumptions for a stylised household."),
          tags$p("These stylised scenarios are not official ABS measures or lender assessments."),
          tags$p("R/market_entry_scenarios.R defines the app-only market-entry scenarios used by the calculator and assessed-rate sensitivity chart."),
          tags$p("Assessment buffer and expense inputs are sensitivity assumptions, not a lender assessment.")
        )
      )
    ),
    layout_column_wrap(
      width = 1,
      fill = FALSE,
      card(
        card_header("Derived Indicator Registry"),
        source_note("The table is generated from R/indicator_registry.R. The provenance chain is pipeline/05_driver.R -> pipeline/06_validate_outputs.R -> data/*.csv -> R/indicator_registry.R -> dashboard labels."),
        card_body(
          div(class = "methodology-table-wrap",
              tableOutput(ns("indicator_table")))
        )
      )
    ),
    layout_column_wrap(
      width = 1/2,
      fill = FALSE,
      card(
        card_header("Economic Caveats"),
        card_body(
          tags$ul(
            tags$li("AWE is individual earnings, not household disposable income."),
            tags$li("WPI is a wage price index, not an income distribution measure."),
            tags$li("CPI rents and CPI new dwelling indexes are price indexes, not household burden measures."),
            tags$li("Assessment buffer and expense inputs are sensitivity assumptions, not a lender assessment."),
            tags$li("SIH relative standard error and 95% margin of error metadata are provided in data/sih_estimate_quality.csv; users should interpret with caution when estimates have high RSE values."),
            tags$li("Gross-income SIH cost ratios, NHHA lower-income rental stress and modelled market-entry scenarios should not be interpreted as the same concept.")
          )
        )
      ),
      card(
        card_header("Release Provenance"),
        card_body(
          tags$ul(
            tags$li("Run pipeline/05_driver.R to refresh local SIH, ABS and RBA-derived CSV outputs."),
            tags$li("Run pipeline/06_validate_outputs.R to gate required schemas, source series and minimum row counts."),
            tags$li("The app reads saved data/*.csv outputs at launch."),
            tags$li("R/indicator_registry.R documents derived indicator formulas, source series, units and interpretation direction.")
          )
        )
      )
    )
  )
}

methodologyPageServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$indicator_table <- renderTable({
      indicator_registry_methodology_table()
    }, striped = TRUE, bordered = TRUE, width = "100%", rownames = FALSE)

    output$provenance_download <- downloadHandler(
      filename = function() {
        methodology_provenance_filename(Sys.Date())
      },
      content = function(file) {
        writeLines(methodology_provenance_report(), con = file, useBytes = TRUE)
      },
      contentType = "text/markdown"
    )
  })
}

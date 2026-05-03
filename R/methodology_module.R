# Methodology page module.

methodologyPageUI <- function(id) {
  ns <- NS(id)

  nav_panel("Methodology",
    policy_page_header(
      "Methodology & Provenance",
      "Indicator definitions, source series and interpretation caveats.",
      actions = div(
        class = "methodology-download",
        downloadButton(ns("provenance_download"),
                       "Download Methodology Summary",
                       class = "btn btn-outline-primary btn-sm")
      )
    ),
    layout_column_wrap(
      width = 1/3,
      fill = FALSE,
      policy_card(
        "Official SIH/NHHA burden measures",
        tags$p("ABS Survey of Income and Housing measures describe observed household housing costs, gross-income cost ratios and NHHA lower-income renter stress."),
        tags$p("These are official survey burden and stress measures, separate from price-index or market-entry proxy indicators."),
        tags$p(sih_sampling_error_note)
      ),
      policy_card(
        "Market-entry cost-pressure indexes",
        tags$p("Derived dashboard indicators use prices, wages, rents and rates to summarise cost pressure over time."),
        tags$p("Higher values generally mean less affordable unless the table states otherwise. These are analytical indexes, not official ABS affordability measures.")
      ),
      policy_card(
        "Stylised scenario calculators",
        tags$p("Serviceability, deposit-gap and calculator outputs use fixed modelling assumptions for a stylised household."),
        tags$p("These stylised scenarios are not official ABS measures or lender assessments."),
        tags$p("R/market_entry_scenarios.R defines the app-only market-entry scenarios used by the calculator and assessed-rate sensitivity chart."),
        tags$p("Assessment buffer and expense inputs are sensitivity assumptions, not a lender assessment."),
        tags$p("Deposit, LVR and loan-term controls are stylised serviceability assumptions; the serviceability chart uses AWE individual earnings as the income proxy.")
      )
    ),
    layout_column_wrap(
      width = 1,
      fill = FALSE,
      policy_card(
        "Derived Indicator Registry",
        note = "The table is generated from R/indicator_registry.R. The provenance chain is pipeline/05_driver.R -> pipeline/06_validate_outputs.R -> data/*.csv -> R/indicator_registry.R -> dashboard labels.",
        div(class = "methodology-table-wrap",
            tableOutput(ns("indicator_table")))
      )
    ),
    layout_column_wrap(
      width = 1/2,
      fill = FALSE,
      policy_card(
        "Economic Caveats",
        tags$ul(
          tags$li("AWE is individual earnings, not household disposable income."),
          tags$li("WPI is a wage price index, not an income distribution measure."),
          tags$li("CPI rents and CPI new dwelling indexes are price indexes, not household burden measures."),
          tags$li("Assessment buffer and expense inputs are sensitivity assumptions, not a lender assessment."),
          tags$li("Deposit, LVR and loan-term controls are stylised serviceability assumptions; the serviceability chart uses AWE individual earnings as the income proxy."),
          tags$li("KPI colours encode economic interpretation as better, worse or neutral/contextual rather than raw up/down movement."),
          tags$li("SIH relative standard error and 95% margin of error metadata are provided in data/sih_estimate_quality.csv; users should interpret with caution when estimates have high RSE values."),
          tags$li("Chart-level reliability markers (\u2020) are driven by data/sih_estimate_quality.csv where matching SIH RSE metadata is available."),
          tags$li("Where available, visible error bars and interval hover text use 95% margin-of-error metadata from data/sih_estimate_quality.csv."),
          tags$li("Gross-income SIH cost ratios, NHHA lower-income rental stress and modelled market-entry scenarios should not be interpreted as the same concept.")
        )
      ),
      policy_card(
        "Release Provenance",
        tags$ul(
          tags$li("Run pipeline/05_driver.R to refresh local SIH, ABS and RBA-derived CSV outputs."),
          tags$li("Run pipeline/06_validate_outputs.R to gate required schemas, source series and minimum row counts."),
          tags$li("The app reads saved data/*.csv outputs at launch."),
          tags$li("R/indicator_registry.R documents derived indicator formulas, source series, units and interpretation direction.")
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

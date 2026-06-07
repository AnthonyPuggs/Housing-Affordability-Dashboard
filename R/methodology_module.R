# Methodology page module.

if (!exists("indicator_quality_coverage_summary", mode = "function", inherits = TRUE)) {
  indicator_context_path <- if (exists("project_path", mode = "function", inherits = TRUE)) {
    project_path("R", "indicator_context.R")
  } else {
    file.path("R", "indicator_context.R")
  }
  if (file.exists(indicator_context_path)) {
    source(indicator_context_path, local = environment())
  }
}

if (!exists("release_confidence_summary", mode = "function", inherits = TRUE)) {
  release_checklist_path <- if (exists("project_path", mode = "function", inherits = TRUE)) {
    project_path("R", "release_checklist.R")
  } else {
    file.path("R", "release_checklist.R")
  }
  if (file.exists(release_checklist_path)) {
    source(release_checklist_path, local = environment())
  }
}

if (!exists("source_audit_methodology_table", mode = "function", inherits = TRUE)) {
  source_audit_path <- if (exists("project_path", mode = "function", inherits = TRUE)) {
    project_path("R", "source_audit_registry.R")
  } else {
    file.path("R", "source_audit_registry.R")
  }
  if (file.exists(source_audit_path)) {
    source(source_audit_path, local = environment())
  }
}

if (!exists("feature_source_note", mode = "function", inherits = TRUE)) {
  feature_metadata_path <- if (exists("project_path", mode = "function", inherits = TRUE)) {
    project_path("R", "feature_metadata.R")
  } else {
    file.path("R", "feature_metadata.R")
  }
  if (file.exists(feature_metadata_path)) {
    source(feature_metadata_path, local = environment())
  }
}

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
    navset_tab(
      id = ns("methodology_layer"),
      nav_panel(
        "Public Summary",
        layout_column_wrap(
          width = 1/3,
          heights_equal = "row",
          fill = FALSE,
          policy_card(
            "Official SIH/NHHA burden measures",
            tags$p("ABS Survey of Income and Housing measures describe observed household housing costs, gross-income cost ratios and NHHA lower-income renter stress."),
            tags$p("These are official survey burden and stress measures, separate from price-index or market-entry proxy indicators."),
            tags$p("The Overview official burden snapshot surfaces selected SIH/NHHA measures beside the stylised market-entry score."),
            tags$p(sih_sampling_error_note)
          ),
          policy_card(
            "Recent buyer empirical evidence",
            tags$p("The Recent Buyers tab uses data/sih_recent_buyers_2020.csv from SIH File 9 to describe 2019-20 first-home, changeover and all recent buyer households."),
            tags$p("These survey estimates ground the deposit and serviceability discussion, but they are not live market-entry indexes."),
            tags$p("Rental entry calculator pathway outputs are stylised cash-flow scenarios, not official ABS/NHHA stress measures or tenancy eligibility assessments.")
          ),
          policy_card(
            "Market-entry cost-pressure indexes",
            tags$p("Derived dashboard indicators use prices, wages, rents and rates to summarise cost pressure over time."),
            tags$p("Higher values generally mean less affordable unless the table states otherwise. These are analytical indexes, not official ABS affordability measures.")
          ),
          policy_card(
            "National Housing Affordability Score",
            tags$p("The National Housing Affordability Score is a descriptive composite indicator for national market-entry affordability."),
            tags$p("The v1 score uses fixed 40/35/25 weights for mortgage serviceability, rental entry and deposit barrier component scores."),
            tags$p("Higher values mean more affordable relative to the score history; it is not an official ABS/NHHA statistic or lender assessment.")
          ),
          policy_card(
            "Plain English Interpretation",
            tags$p("A low score means market-entry conditions are poor relative to the score history; it is not a pass/fail affordability threshold."),
            tags$p("A high score does not mean housing is affordable for all households, and the score is not the share of households who can afford housing."),
            tags$p("Official SIH/NHHA stress measures remain separate because they measure observed household burden, not market-entry conditions."),
            tags$p("Rental-entry stress may be understated relative to advertised-rent or new-lease evidence because v1 uses public index-style inputs rather than a direct new-tenancy rent series.")
          ),
          policy_card(
            "Stylised scenario calculators",
            tags$p("Serviceability, deposit-gap and calculator outputs use fixed modelling assumptions for a stylised household."),
            tags$p("These stylised scenarios are not official ABS measures or lender assessments."),
            tags$p("R/market_entry_scenarios.R defines the app-only market-entry scenarios used by the calculator and assessed-rate sensitivity chart."),
            tags$p("The calculator now separates ownership serviceability from the rental entry calculator pathway so rent, bond and upfront moving costs are not presented as mortgage calculations."),
            tags$p("Assessment buffer and expense inputs are sensitivity assumptions, not a lender assessment."),
            tags$p("Deposit, LVR and loan-term controls are stylised serviceability assumptions; the serviceability chart uses AWE individual earnings as the income proxy.")
          )
        )
      ),
      nav_panel(
        "Technical Details",
        layout_column_wrap(
          width = 1,
          fill = FALSE,
          policy_card(
            "Score Diagnostics",
            tags$p("The score is a historical-relative monitoring index, not an absolute affordability threshold."),
            tags$p("A score near 0 or 100 is low or high versus the score window, not a statement that housing is affordable or unaffordable for all households."),
            tags$p("Mortgage serviceability and deposit barrier have an ownership-channel overlap, but v1 keeps both because they describe monthly servicing versus upfront deposit constraints."),
            tags$p("Sensitivity diagnostics compare equal weights, ownership-heavy weights, rental-heavy weights, leave-one-out variants and geometric aggregation."),
            uiOutput(ns("score_diagnostics_summary")),
            tableOutput(ns("score_contribution_table")),
            tableOutput(ns("score_sensitivity_table"))
          )
        ),
        layout_column_wrap(
          width = 1/2,
          fill = FALSE,
          policy_card(
            "Quality & Coverage",
            tags$p("Indicator confidence groups separate official survey measures, derived indexes, stylised scenarios and context series."),
            div(class = "methodology-table-wrap",
                tableOutput(ns("quality_coverage_table")))
          ),
          policy_card(
            "Release Confidence",
            tags$p("Read-only release confidence uses saved CSVs, data vintage metadata and the public release checklist; it does not refresh ABS or RBA data inside the app."),
            div(class = "methodology-table-wrap",
                tableOutput(ns("release_confidence_table")))
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
              tags$li("Rental-entry stress may be understated relative to advertised-rent or new-lease evidence because v1 uses public index-style inputs."),
              tags$li("Assessment buffer and expense inputs are sensitivity assumptions, not a lender assessment."),
              tags$li("Deposit, LVR and loan-term controls are stylised serviceability assumptions; the serviceability chart uses AWE individual earnings as the income proxy."),
              tags$li("KPI colours encode economic interpretation as better, worse or neutral/contextual rather than raw up/down movement."),
              tags$li("SIH relative standard error and 95% margin of error metadata are provided in data/sih_estimate_quality.csv; users should interpret with caution when estimates have high RSE values."),
              tags$li("Chart-level reliability markers (\u2020) are driven by data/sih_estimate_quality.csv where matching SIH RSE metadata is available."),
              tags$li("Where available, visible error bars and interval hover text use 95% margin-of-error metadata from data/sih_estimate_quality.csv."),
              tags$li("Gross-income SIH cost ratios, NHHA lower-income rental stress and modelled market-entry scenarios should not be interpreted as the same concept."),
              tags$li("Recent buyer empirical evidence comes from SIH File 9 and should not be interpreted as a current advertised-price or new-loan measure.")
            )
          ),
          policy_card(
            "Release Provenance",
            tags$ul(
              tags$li("Run pipeline/05_driver.R to refresh local SIH, ABS and RBA-derived CSV outputs."),
              tags$li("Run pipeline/06_validate_outputs.R to gate required schemas, source series and minimum row counts."),
              tags$li("data/data_vintage.csv records the last successful refresh time and observation-period coverage."),
              tags$li("The app reads saved data/*.csv outputs at launch."),
              tags$li("R/indicator_registry.R documents derived indicator formulas, source series, units and interpretation direction.")
            ),
            uiOutput(ns("data_vintage_summary"))
          )
        )
      ),
      nav_panel(
        "Data Source Audit",
        layout_column_wrap(
          width = 1,
          fill = FALSE,
          policy_card(
            "Unresolved Core Data Gaps",
            note = if (exists("feature_source_note", mode = "function")) {
              feature_source_note("methodology_source_audit")
            } else {
              "Candidate sources require acceptance before becoming dashboard feeds."
            },
            tags$p("Household disposable income, new-tenancy or advertised rents, and residual income / living costs remain source-audit items, not implemented data feeds."),
            tags$ul(
              tags$li("Household disposable income"),
              tags$li("New-tenancy or advertised rents"),
              tags$li("Residual income / living costs")
            ),
            tags$p("Rows marked candidate are plausible sources for later implementation. Rows marked not suitable yet should remain context only until the concept and coverage match the dashboard question."),
            div(class = "methodology-table-wrap",
                tableOutput(ns("source_audit_table")))
          )
        )
      )
    )
  )
}

methodologyPageServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    score_diagnostics <- reactive({
      if (exists("national_affordability_score_diagnostics_data")) {
        return(national_affordability_score_diagnostics_data)
      }
      if (exists("afford_idx") &&
          exists("national_affordability_score_diagnostics", mode = "function")) {
        return(national_affordability_score_diagnostics(afford_idx))
      }
      NULL
    })

    output$score_diagnostics_summary <- renderUI({
      diagnostics <- score_diagnostics()
      if (is.null(diagnostics)) {
        return(tags$p("Score diagnostics are unavailable in this session."))
      }
      sample <- diagnostics$sample_window
      tags$ul(
        tags$li(paste0(
          "Common score sample: ",
          format(sample$start_date[1], "%b %Y"),
          " to ",
          format(sample$latest_date[1], "%b %Y"),
          " (",
          sample$score_rows[1],
          " complete score dates)."
        )),
        tags$li(paste0(
          "Latest headline score: ",
          fmt_index(sample$latest_score[1]),
          " / 100."
        )),
        tags$li(diagnostics$interpretation_warning)
      )
    })

    output$score_contribution_table <- renderTable({
      diagnostics <- score_diagnostics()
      if (is.null(diagnostics)) {
        return(data.frame())
      }
      d <- diagnostics$latest_contributions
      data.frame(
        Component = d$component_label,
        Score = paste0(fmt_index(d$score), " / 100"),
        Weight = scales::percent(d$weight, accuracy = 1),
        `Contribution Points` = fmt_index(d$contribution_points),
        check.names = FALSE
      )
    }, striped = TRUE, bordered = TRUE, width = "100%", rownames = FALSE)

    output$score_sensitivity_table <- renderTable({
      diagnostics <- score_diagnostics()
      if (is.null(diagnostics)) {
        return(data.frame())
      }
      d <- diagnostics$sensitivity_scores
      data.frame(
        Scenario = gsub("_", " ", d$scenario),
        Score = fmt_index(d$score),
        `Difference From Default` = sprintf("%+.1f", d$difference_from_default),
        check.names = FALSE
      )
    }, striped = TRUE, bordered = TRUE, width = "100%", rownames = FALSE)

    output$indicator_table <- renderTable({
      indicator_registry_methodology_table()
    }, striped = TRUE, bordered = TRUE, width = "100%", rownames = FALSE)

    output$quality_coverage_table <- renderTable({
      summary <- indicator_quality_coverage_summary()
      data.frame(
        "Measure Class" = indicator_measure_class_label(summary$measure_class),
        "Indicators" = summary$indicators,
        "Latest Period" = summary$latest_period,
        "Quality Note" = summary$quality_note,
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
    }, striped = TRUE, bordered = TRUE, width = "100%", rownames = FALSE)

    output$release_confidence_table <- renderTable({
      release_confidence_summary()
    }, striped = TRUE, bordered = TRUE, width = "100%", rownames = FALSE)

    output$source_audit_table <- renderTable({
      source_audit_methodology_table()
    }, striped = TRUE, bordered = TRUE, width = "100%", rownames = FALSE)

    output$data_vintage_summary <- renderUI({
      vintage <- read_data_vintage(fallback = TRUE)
      tags$p(data_vintage_summary(vintage), class = "source-note policy-source-note")
    })

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

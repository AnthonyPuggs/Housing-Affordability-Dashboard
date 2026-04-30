# ==============================================================================
# Australian Housing Affordability Dashboard
# ==============================================================================
# Single-file Shiny app — reads pre-processed CSVs from data/ pipeline
# 6 pages: Overview, Price Trends, Affordability, Market Context, Housing Supply, Rental Market
# ==============================================================================

library(shiny)
library(bslib)
library(plotly)

# Shared data loading, helpers, theme, and pre-computed datasets
source("plot_setup.R")

# Concise method/source note used below chart cards.
source_note <- function(...) {
  tags$p(...,
         class = "source-note px-3",
         style = "color: var(--app-muted); font-size: 0.85rem; margin-bottom: 0;")
}

stylised_scenario_note <- "Stylised scenario, not an official ABS measure or lender assessment."

# ==============================================================================
# UI
# ==============================================================================

ui <- page_navbar(
  title = "Australian Housing Affordability",
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    base_font = font_google("Source Sans 3"),
    heading_font = font_google("Poppins"),
    primary = "#0E5A8A",
    secondary = "#1F9D8C"
  ),
  header = tags$head(
    tags$style(HTML("
      /* ---- Light theme (default) ---- */
      :root {
        --app-bg: #f4f7fb;
        --app-text: #1f2d3d;
        --app-panel: #ffffff;
        --app-border: #d9e0e8;
        --app-muted: #6c757d;
      }
      /* ---- Dark theme ---- */
      html[data-theme='dark'] {
        --app-bg: #0b1220;
        --app-text: #e3ebf4;
        --app-panel: #111b2e;
        --app-border: #2a3a54;
        --app-muted: #8899aa;
      }

      /* Base */
      body {
        background-color: var(--app-bg) !important;
        color: var(--app-text) !important;
      }
      .bslib-page-fill { background-color: var(--app-bg) !important; }

      /* Navbar */
      html[data-theme='dark'] .navbar {
        background-color: #0d1a2d !important;
        border-bottom: 1px solid var(--app-border);
      }
      html[data-theme='dark'] .navbar .navbar-brand,
      html[data-theme='dark'] .navbar .nav-link,
      html[data-theme='dark'] .navbar .navbar-toggler-icon {
        color: #e3ebf4 !important;
      }
      html[data-theme='dark'] .navbar .nav-link:hover,
      html[data-theme='dark'] .navbar .nav-link.active {
        color: #ffffff !important;
      }

      /* Cards and panels */
      .card, .sidebar, .bslib-sidebar-layout > .sidebar {
        background-color: var(--app-panel) !important;
        color: var(--app-text) !important;
        border-color: var(--app-border) !important;
      }
      .card-header {
        background-color: var(--app-panel) !important;
        color: var(--app-text) !important;
        border-color: var(--app-border) !important;
      }

      /* Form controls */
      .form-control, .selectize-input, .selectize-dropdown,
      .selectize-dropdown-content, .form-select {
        background-color: var(--app-panel) !important;
        color: var(--app-text) !important;
        border-color: var(--app-border) !important;
      }
      .selectize-input .item { color: var(--app-text) !important; }

      /* Labels, text, headings */
      .control-label, .form-label,
      label, .shiny-input-container label,
      .radio label, .checkbox label,
      .form-check-label {
        color: var(--app-text) !important;
      }
      .sidebar .control-label, .sidebar label,
      .sidebar .form-check-label, .sidebar .radio label {
        color: var(--app-text) !important;
      }

      /* Nav tabs (Affordability sub-tabs) */
      html[data-theme='dark'] .nav-tabs .nav-link,
      html[data-theme='dark'] .nav-pills .nav-link {
        color: var(--app-muted) !important;
      }
      html[data-theme='dark'] .nav-tabs .nav-link.active,
      html[data-theme='dark'] .nav-pills .nav-link.active {
        color: #ffffff !important;
        background-color: var(--app-panel) !important;
        border-color: var(--app-border) !important;
      }

      /* Selectize tag items in multi-select */
      html[data-theme='dark'] .selectize-input .item {
        background-color: #1e2d44 !important;
        color: #e3ebf4 !important;
        border-color: #3a4f6e !important;
      }

      /* Date range input */
      html[data-theme='dark'] .input-daterange .input-group-text {
        background-color: var(--app-panel) !important;
        color: var(--app-text) !important;
        border-color: var(--app-border) !important;
      }

      /* Radio & checkbox circles/checks */
      html[data-theme='dark'] .form-check-input {
        background-color: var(--app-border) !important;
        border-color: var(--app-muted) !important;
      }
      html[data-theme='dark'] .form-check-input:checked {
        background-color: #0E5A8A !important;
        border-color: #0E5A8A !important;
      }

      /* Slider (IRS) */
      html[data-theme='dark'] .irs--shiny .irs-line,
      html[data-theme='dark'] .irs--shiny .irs-grid-pol {
        background-color: var(--app-border) !important;
      }
      html[data-theme='dark'] .irs--shiny .irs-min,
      html[data-theme='dark'] .irs--shiny .irs-max,
      html[data-theme='dark'] .irs--shiny .irs-single,
      html[data-theme='dark'] .irs--shiny .irs-from,
      html[data-theme='dark'] .irs--shiny .irs-to,
      html[data-theme='dark'] .irs--shiny .irs-grid-text {
        color: var(--app-text) !important;
      }

      /* Value boxes */
      .value-box .value-box-value { font-size: 1.6rem; font-weight: 700; }
      .value-box .value-box-title { font-size: 0.85rem; opacity: 0.85; }
      .kpi-subtitle { font-size: 0.75rem; color: var(--app-muted); margin-top: 2px; margin-bottom: 0; }

      /* Misc */
      .shiny-output-error-validation { color: #A43D3D; font-weight: 600; }
      .calc-result { font-size: 1.3rem; font-weight: 600; color: var(--app-text); }
      .calc-label { font-size: 0.85rem; color: var(--app-muted); }

      /* ---- Responsive chart containers ---- */
      .chart-wide  { height: 380px; }
      .chart-square { height: 420px; }

      /* Plotly fills its container */
      .chart-wide .plotly, .chart-square .plotly,
      .chart-wide .js-plotly-plot, .chart-square .js-plotly-plot {
        width: 100% !important;
        height: 100% !important;
      }

      /* KPI change indicator colors */
      .kpi-change-up   { color: #2ecc71 !important; font-weight: 600; }
      .kpi-change-down { color: #e74c3c !important; font-weight: 600; }

      /* Mobile: stack sidebars, reduce value box text */
      @media (max-width: 768px) {
        .bslib-sidebar-layout { flex-direction: column !important; }
        .bslib-sidebar-layout > .sidebar { width: 100% !important; max-width: 100% !important; }
        .value-box .value-box-value { font-size: 1.2rem; }
        .value-box .value-box-title { font-size: 0.75rem; }
      }

      /* Tablet: tighten chart spacing */
      @media (max-width: 1024px) {
        .card-body { padding: 0.5rem !important; }
      }
    ")),
    tags$script(HTML("
      (function() {
        function setTheme(theme) {
          document.documentElement.setAttribute('data-theme', theme);
          document.documentElement.setAttribute('data-bs-theme', theme);
          var btn = document.getElementById('theme_toggle');
          if (btn) btn.textContent = theme === 'dark' ? 'Light mode' : 'Dark mode';
          // Send to Shiny only after it's ready
          if (window.Shiny && Shiny.setInputValue) {
            Shiny.setInputValue('theme_mode', theme, { priority: 'event' });
          }
        }

        // Apply saved theme immediately (before DOMContentLoaded) to avoid flash
        var saved = window.localStorage.getItem('afford_theme');
        var preferDark = window.matchMedia &&
          window.matchMedia('(prefers-color-scheme: dark)').matches;
        var initial = saved || (preferDark ? 'dark' : 'light');
        document.documentElement.setAttribute('data-theme', initial);
        document.documentElement.setAttribute('data-bs-theme', initial);

        // Once Shiny is connected, send the initial value
        $(document).on('shiny:connected', function() {
          var theme = document.documentElement.getAttribute('data-theme') || 'light';
          setTheme(theme);
        });

        // Toggle handler — use direct ID check instead of .closest()
        $(document).on('click', '#theme_toggle', function(e) {
          e.preventDefault();
          var cur = document.documentElement.getAttribute('data-theme') || 'light';
          var next = cur === 'dark' ? 'light' : 'dark';
          window.localStorage.setItem('afford_theme', next);
          setTheme(next);
        });
      })();
    "))
  ),
  nav_spacer(),
  nav_item(actionButton("theme_toggle", "Dark mode",
                         class = "btn-outline-light btn-sm")),

  # ============================================================================
  # PAGE 1: OVERVIEW
  # ============================================================================
  nav_panel(
    "Overview",
    div(
      class = "d-flex justify-content-between align-items-start mb-3 px-2",
      div(
        tags$h3("Housing Affordability", class = "mb-1",
                style = "font-weight: 700;"),
        tags$p("Analysing the state of the Australian market",
               style = "color: var(--app-muted); margin-bottom: 0;")
      )
    ),
    layout_column_wrap(
      width = 1/4,
      fill = FALSE,
      value_box(
        title = "National Median Price",
        value = textOutput("vb_nat_price"),
        p(class = "kpi-subtitle", textOutput("vb_nat_price_date")),
        uiOutput("vb_nat_price_change"),
        theme = value_box_theme(bg = "#0E5A8A", fg = "#fff")
      ),
      value_box(
        title = "Sydney Median Price",
        value = textOutput("vb_syd_price"),
        p(class = "kpi-subtitle", textOutput("vb_syd_price_date")),
        uiOutput("vb_syd_price_change"),
        theme = value_box_theme(bg = "#3B4C7A", fg = "#fff")
      ),
      value_box(
        title = "Modelled Serviceability",
        value = textOutput("vb_service"),
        p(class = "kpi-subtitle", "Stylised mortgage scenario"),
        uiOutput("vb_service_change"),
        theme = value_box_theme(bg = "#17415F", fg = "#fff")
      ),
      value_box(
        title = "Rental Affordability",
        value = textOutput("vb_rental"),
        p(class = "kpi-subtitle", textOutput("vb_rental_date")),
        uiOutput("vb_rental_change"),
        theme = value_box_theme(bg = "#984ea3", fg = "#fff")
      )
    ),
    layout_column_wrap(
      width = 1,
      fill = FALSE,
      card(
        fill = FALSE,
        card_header("Capital City Median House Prices"),
        uiOutput("overview_price_subtitle"),
        card_body(plotlyOutput("overview_median_prices", height = "480px")),
        card_footer(
          sliderInput("overview_price_dates", "Date Range",
                      min = min(median_prices_combined$date, na.rm = TRUE),
                      max = max(median_prices_combined$date, na.rm = TRUE),
                      value = c(as.Date("2010-01-01"),
                                max(median_prices_combined$date, na.rm = TRUE)),
                      width = "100%", timeFormat = "%b %Y"),
          radioButtons("overview_price_transform", NULL,
                       choices = c("Nominal ($)" = "nominal",
                                   "Index (start = 100)" = "index"),
                       selected = "nominal", inline = TRUE)
        )
      )
    ),
    layout_column_wrap(
      width = 1,
      fill = FALSE,
      card(
        fill = FALSE,
        card_header("Affordability Indices"),
        source_note("Cost-pressure indexes; higher = less affordable. Rent uses ABS CPI rents/WPI, mortgage uses price\u00d7rate/WPI, deposit uses price/income, and price-to-income uses national dwelling prices/WPI."),
        card_body(plotlyOutput("overview_afford_change", height = "380px"))
      )
    )
  ),

  # ============================================================================
  # PAGE 2: PRICE TRENDS
  # ============================================================================
  nav_panel(
    "Price Trends",
    navset_card_tab(
      # Tab 2a: Dwelling Price Index (existing content)
      nav_panel(
        "Dwelling Price Index",
        layout_sidebar(
          sidebar = sidebar(
            width = 300, open = "desktop",
            selectizeInput("price_cities", "Capital Cities",
                           choices = rppi_cities,
                           selected = c("Sydney", "Melbourne", "Brisbane",
                                        "Weighted average of eight capital cities"),
                           multiple = TRUE),
            radioButtons("price_dwelling", "Dwelling Type",
                         choices = c("Total", "Houses", "Units"),
                         selected = "Total"),
            dateRangeInput("price_dates", "Date Range",
                           start = as.Date("2003-01-01"),
                           end = Sys.Date(),
                           min = as.Date("1990-01-01")),
            radioButtons("price_transform", "Transform",
                         choices = c("Levels" = "levels",
                                     "YoY %" = "yoy",
                                     "Index (start=100)" = "index"),
                         selected = "levels")
          ),
          card(
            card_header("Dwelling Price Index by Capital City"),
            source_note("ABS dwelling price data. Price indexes describe market price movements, not household affordability or borrowing capacity."),
            card_body(div(class = "chart-wide", plotlyOutput("price_chart", height = "100%", width = "100%")))
          )
        )
      ),
      # Tab 2b: Rent CPI
      nav_panel(
        "Rent CPI",
        layout_sidebar(
          sidebar = sidebar(
            width = 300, open = "desktop",
            selectizeInput("rent_cpi_cities", "Capital Cities",
                           choices = rent_cpi_cities,
                           selected = rent_cpi_cities,
                           multiple = TRUE),
            radioButtons("rent_cpi_datatype", "Data Type",
                         choices = c("Index numbers" = "index",
                                     "Annual change (%)" = "yoy",
                                     "Quarterly change (%)" = "qoq"),
                         selected = "index")
          ),
          card(
            card_header("Rent Consumer Price Index (CPI) by Greater Capital City"),
            source_note("ABS CPI rents are price indexes. They measure rental price movements, not the housing cost burden of lower-income renters."),
            card_body(div(class = "chart-wide", plotlyOutput("rent_cpi_chart", height = "100%", width = "100%"))),
            card_footer(
              sliderInput("rent_cpi_dates", "Date Range",
                          min = min(rent_cpi_combined$date, na.rm = TRUE),
                          max = max(rent_cpi_combined$date, na.rm = TRUE),
                          value = c(as.Date("2012-01-01"),
                                    max(rent_cpi_combined$date, na.rm = TRUE)),
                          width = "100%", timeFormat = "%Y Q%q"),
              tags$p("Source: ABS — Consumer Price Index, Australia",
                     class = "text-muted small mt-2 mb-0")
            )
          )
        )
      )
    )
  ),

  # ============================================================================
  # PAGE 3: AFFORDABILITY DEEP DIVE
  # ============================================================================
  nav_panel(
    "Affordability",
    navset_card_tab(
      title = "Affordability Analysis",
      # Tab 3a: Indices over time
      nav_panel(
        "Indices",
        layout_sidebar(
          sidebar = sidebar(
            width = 280, open = "desktop",
            checkboxGroupInput("afford_indices", "Indicators",
                               choices = c("Price-to-Income Cost Pressure" = "Price-to-Income Ratio",
                                           "Modelled Mortgage Cost Pressure" = "Mortgage Serviceability Index",
                                           "Rent Cost Pressure" = "Rental Affordability Index",
                                           "Stylised Deposit Gap (Years)" = "Deposit Gap (Years)",
                                           "Modelled Serviceability" = "Housing Serviceability"),
                               selected = c("Price-to-Income Ratio",
                                           "Mortgage Serviceability Index",
                                           "Rental Affordability Index",
                                           "Deposit Gap (Years)",
                                           "Housing Serviceability")),
            sliderInput("afford_dates", "Date Range",
                        min = min(afford_idx$date, na.rm = TRUE),
                        max = max(afford_idx$date, na.rm = TRUE),
                        value = c(as.Date("2003-01-01"),
                                  max(afford_idx$date, na.rm = TRUE)),
                        width = "100%", timeFormat = "%b %Y")
          ),
          card(
            card_header("Affordability Indicators"),
            source_note("Cost-pressure indexes; higher = less affordable. Market-entry measures use wage, price and rate proxies, not official ABS stress definitions."),
            card_body(div(class = "chart-wide", plotlyOutput("afford_indices_chart", height = "100%", width = "100%")))
          ),
          conditionalPanel(
            condition = "input.afford_indices.indexOf('Housing Serviceability') >= 0",
            card(
              card_header("Modelled Serviceability"),
              source_note("Modelled annual repayment share using an 80% LVR, 30-year loan and RBA mortgage-rate inputs. ", stylised_scenario_note),
              card_body(plotlyOutput("afford_serviceability", height = "380px"))
            )
          )
        )
      ),
      # Tab 3b: Calculator
      nav_panel(
        "Calculator",
        layout_sidebar(
          sidebar = sidebar(
            width = 320, open = "desktop",
            source_note(stylised_scenario_note),
            numericInput("calc_price", "Dwelling Price ($)",
                         value = 800000, min = 100000, max = 5000000,
                         step = 50000),
            numericInput("calc_income", "Household Gross Income ($/yr)",
                         value = 120000, min = 20000, max = 1000000,
                         step = 5000),
            sliderInput("calc_rate", "Interest Rate (%)",
                        min = 1, max = 12, value = 6.0, step = 0.1),
            sliderInput("calc_deposit_pct", "Deposit (%)",
                        min = 5, max = 40, value = 20, step = 1),
            sliderInput("calc_term", "Loan Term (years)",
                        min = 10, max = 30, value = 30, step = 1),
            sliderInput("calc_savings_rate", "Savings Rate (%)",
                        min = 5, max = 40, value = 15, step = 1)
          ),
          layout_column_wrap(
            width = 1/2,
            fill = FALSE,
            value_box(
              title = "Monthly Repayment",
              value = textOutput("calc_repayment"),
              theme = value_box_theme(bg = "#0E5A8A", fg = "#fff")
            ),
            value_box(
              title = "Repayment / Income",
              value = textOutput("calc_ratio"),
              theme = value_box_theme(bg = "#1F9D8C", fg = "#fff")
            ),
            value_box(
              title = "Years to Save Deposit",
              value = textOutput("calc_years"),
              theme = value_box_theme(bg = "#3B4C7A", fg = "#fff")
            ),
            value_box(
              title = "Loan-to-Value Ratio",
              value = textOutput("calc_lvr"),
              theme = value_box_theme(bg = "#326273", fg = "#fff")
            ),
            value_box(
              title = "Total Interest Paid",
              value = textOutput("calc_total_interest"),
              theme = value_box_theme(bg = "#984ea3", fg = "#fff")
            ),
            value_box(
              title = "Deposit Amount",
              value = textOutput("calc_deposit_amt"),
              theme = value_box_theme(bg = "#17415F", fg = "#fff")
            )
          )
        )
      ),
      # Tab 3c: Who's in stress?
      nav_panel(
        "Housing Stress",
        layout_sidebar(
          sidebar = sidebar(
            width = 280, open = "desktop",
            selectInput("stress_breakdown", "Breakdown By",
                        choices = c("Age Group" = "age_group",
                                    "Family Type" = "family_type",
                                    "Income Quintile" = "equiv_income_quintile",
                                    "Dwelling Structure" = "dwelling_structure",
                                    "By Tenure (Owners)" = "owner",
                                    "By Tenure (Renters)" = "renter")),
            radioButtons("stress_population", "Population",
                         choices = c("All Households" = "all_households",
                                     "Lower Income (Bottom 40%)" = "lower_income"))
          ),
          card(
            card_header("Housing Cost Stress Bands (2019-20)"),
            source_note("ABS Survey of Income and Housing. Official survey-based housing cost burden bands by household group."),
            card_body(div(class = "chart-square", plotlyOutput("stress_chart", height = "100%", width = "100%")))
          )
        )
      ),
      # Tab 3d: Cost burden
      nav_panel(
        "Cost Burden",
        layout_sidebar(
          sidebar = sidebar(
            width = 280, open = "desktop",
            selectInput("burden_breakdown", "Breakdown By",
                        choices = c("Age Group" = "age_group",
                                    "Family Type" = "family_type",
                                    "Income Quintile" = "equiv_income_quintile")),
            radioButtons("burden_stat", "Statistic",
                         choices = c("Mean" = "mean", "Median" = "median"))
          ),
          card(
            card_header("Housing Cost-to-Income Ratio by Tenure & Demographics (2019-20)"),
            source_note("ABS Survey of Income and Housing. Gross-income housing cost ratios by tenure and demographic group."),
            card_body(div(class = "chart-square", plotlyOutput("burden_heatmap", height = "100%", width = "100%")))
          )
        )
      )
    )
  ),

  # ============================================================================
  # PAGE 4: MARKET CONTEXT (Labour & Demographics)
  # ============================================================================
  nav_panel(
    "Market Context",
    div(
      class = "d-flex justify-content-between align-items-start mb-3 px-2",
      div(
        tags$h3("Labour & Demographics", class = "mb-1",
                style = "font-weight: 700;"),
        tags$p("Analysing the state of the Australian market",
               style = "color: var(--app-muted); margin-bottom: 0;")
      )
    ),
    layout_column_wrap(
      width = 1/3,
      fill = FALSE,
      value_box(
        title = "Unemployment Rate",
        value = textOutput("vb_unemp"),
        p(class = "kpi-subtitle", "Trend estimate"),
        uiOutput("vb_unemp_change"),
        theme = value_box_theme(bg = "#0E5A8A", fg = "#fff")
      ),
      value_box(
        title = "Net Overseas Migration",
        value = textOutput("vb_nom"),
        p(class = "kpi-subtitle", "Annual estimate"),
        uiOutput("vb_nom_change"),
        theme = value_box_theme(bg = "#3B4C7A", fg = "#fff")
      ),
      value_box(
        title = "Participation Rate",
        value = textOutput("vb_participation"),
        p(class = "kpi-subtitle", "Trend estimate"),
        uiOutput("vb_participation_change"),
        theme = value_box_theme(bg = "#17415F", fg = "#fff")
      )
    ),
    sliderInput("context_dates", "Date Range",
                min = as.Date("1990-01-01"),
                max = Sys.Date(),
                value = c(as.Date("2000-01-01"), Sys.Date()),
                width = "100%", timeFormat = "%b %Y"),
    layout_column_wrap(
      width = 1,
      fill = FALSE,
      card(
        fill = FALSE,
        card_header("Interest Rates on Residential Mortgages"),
        source_note("RBA mortgage rates are market-rate inputs, not lender assessment outcomes or household serviceability approvals."),
        card_body(plotlyOutput("context_rates", height = "380px"))
      )
    ),
    layout_column_wrap(
      width = 1/2,
      card(
        fill = FALSE,
        card_header("Labour Market Spare Capacity"),
        source_note("ABS labour force rates. KPI changes are percentage-point changes, not relative percentage changes."),
        card_body(plotlyOutput("context_labour", height = "380px"))
      ),
      card(
        fill = FALSE,
        card_header("Population Demand"),
        source_note("ABS population data. Net overseas migration is shown as an annualised flow in thousands."),
        card_body(plotlyOutput("context_pop", height = "380px"))
      )
    )
  ),

  # ============================================================================
  # PAGE 5: HOUSING SUPPLY
  # ============================================================================
  nav_panel(
    "Housing Supply",
    div(
      class = "d-flex justify-content-between align-items-start mb-3 px-2",
      div(
        tags$h3("Housing Supply", class = "mb-1",
                style = "font-weight: 700;"),
        tags$p("Building activity and construction costs",
               style = "color: var(--app-muted); margin-bottom: 0;")
      )
    ),
    layout_column_wrap(
      width = 1/4,
      fill = FALSE,
      value_box(
        title = "NSW Approvals",
        value = textOutput("vb_approvals_nsw"),
        p(class = "kpi-subtitle", "Monthly dwelling units"),
        uiOutput("vb_approvals_nsw_change"),
        theme = value_box_theme(bg = "#0E5A8A", fg = "#fff")
      ),
      value_box(
        title = "VIC Approvals",
        value = textOutput("vb_approvals_vic"),
        p(class = "kpi-subtitle", "Monthly dwelling units"),
        uiOutput("vb_approvals_vic_change"),
        theme = value_box_theme(bg = "#3B4C7A", fg = "#fff")
      ),
      value_box(
        title = "Construction Costs",
        value = textOutput("vb_construction"),
        p(class = "kpi-subtitle", "CPI New Dwelling Index"),
        uiOutput("vb_construction_change"),
        theme = value_box_theme(bg = "#17415F", fg = "#fff")
      ),
      value_box(
        title = "Houses Share",
        value = textOutput("vb_houses_share"),
        p(class = "kpi-subtitle", "% of total approvals"),
        uiOutput("vb_houses_share_change"),
        theme = value_box_theme(bg = "#984ea3", fg = "#fff")
      )
    ),
    sliderInput("supply_dates", "Date Range",
                min = as.Date("1990-01-01"),
                max = Sys.Date(),
                value = c(as.Date("2000-01-01"), Sys.Date()),
                width = "100%", timeFormat = "%b %Y"),
    layout_column_wrap(
      width = 1,
      fill = FALSE,
      card(
        fill = FALSE,
        card_header("Building Approvals"),
        source_note("ABS building approvals. Approval counts are supply pipeline indicators, not completed dwellings."),
        card_body(plotlyOutput("supply_approvals", height = "420px"))
      ),
      card(
        card_header("CPI New Dwelling Purchase (Construction Cost)"),
        source_note("ABS CPI new dwelling purchase is a construction-cost price index, not a household burden measure."),
        card_body(div(class = "chart-wide", plotlyOutput("supply_cpi_construction", height = "100%", width = "100%")))
      )
    )
  ),

  # ============================================================================
  # PAGE 6: RENTAL MARKET
  # ============================================================================
  nav_panel(
    "Rental Market",
    layout_sidebar(
      sidebar = sidebar(
        width = 280, open = "desktop",
        selectInput("rental_year", "Survey Year (NHHA)",
                    choices = if (nrow(sih_nhha) > 0)
                      rev(sort(unique(sih_nhha$survey_year))) else "2019-20",
                    selected = "2019-20"),
        selectInput("rental_states", "States/Territories",
                    choices = c("All" = "all",
                                if (nrow(sih_nhha) > 0)
                                  sort(unique(sih_nhha$geography[
                                    sih_nhha$geography != "Aust."]))
                                else character(0)),
                    multiple = TRUE,
                    selected = "all"),
        selectInput("rental_cost_breakdown", "Rental Costs By",
                    choices = c("Age Group" = "age_group",
                                "Family Type" = "family_type",
                                "Income Quintile" = "equiv_income_quintile"))
      ),
      layout_column_wrap(
        width = 1/2,
        card(
          card_header("NHHA Rental Stress by State"),
          source_note("ABS Survey of Income and Housing, NHHA lower-income renter stress. Official survey burden/stress measure."),
          card_body(div(class = "chart-square", plotlyOutput("rental_stress_state", height = "100%", width = "100%")))
        ),
        card(
          card_header("NHHA Rental Stress Trends (Over Time)"),
          source_note("ABS Survey of Income and Housing, NHHA lower-income renter stress. Values are proportions of lower-income renter households."),
          card_body(div(class = "chart-wide", plotlyOutput("rental_stress_trend", height = "100%", width = "100%")))
        ),
        card(
          card_header("Rental Affordability Index"),
          source_note("Cost-pressure index using ABS CPI rents and WPI; higher = less affordable."),
          card_body(div(class = "chart-wide", plotlyOutput("rental_afford_index", height = "100%", width = "100%")))
        ),
        card(
          card_header("Weekly Rental Costs by Demographics (2019-20)"),
          source_note("ABS Survey of Income and Housing. Survey rental-cost estimates by household characteristic."),
          card_body(div(class = "chart-square", plotlyOutput("rental_costs_demo", height = "100%", width = "100%")))
        )
      )
    )
  )
)

# ==============================================================================
# SERVER
# ==============================================================================

server <- function(input, output, session) {

  # --- Dark mode reactive ---
  is_dark <- reactive({
    mode <- input$theme_mode
    if (is.null(mode)) return(FALSE)
    identical(mode, "dark")
  })

  # ============================================================================
  # PAGE 1: OVERVIEW
  # ============================================================================

  # Value boxes — National Median Price
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
    css_class <- if (!is.na(ch$change) && ch$change >= 0) "kpi-change-up" else "kpi-change-down"
    tags$p(class = paste("kpi-subtitle", css_class), ch$label)
  })

  # Sydney Median Price
  output$vb_syd_price <- renderText({
    v <- latest_val(median_house_prices, "city", "Sydney")
    if (is.na(v)) "N/A" else fmt_dollar_k(v * 1000)
  })
  output$vb_syd_price_date <- renderText({
    latest_date(median_house_prices, "city", "Sydney")
  })
  output$vb_syd_price_change <- renderUI({
    ch <- latest_change(median_house_prices, "city", "Sydney",
                        periods_back = 4, period_label = "YoY",
                        change_type = "relative_pct")
    css_class <- if (!is.na(ch$change) && ch$change >= 0) "kpi-change-up" else "kpi-change-down"
    tags$p(class = paste("kpi-subtitle", css_class), ch$label)
  })

  # Mortgage to Income (serviceability %)
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
    css_class <- if (diff_val >= 0) "kpi-change-up" else "kpi-change-down"
    tags$p(class = paste("kpi-subtitle", css_class), label)
  })

  # Rental Affordability Index
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
    css_class <- if (!is.na(ch$change) && ch$change >= 0) "kpi-change-up" else "kpi-change-down"
    tags$p(class = paste("kpi-subtitle", css_class), ch$label)
  })

  # Overview: dynamic subtitle for median price chart
  output$overview_price_subtitle <- renderUI({
    txt <- if (identical(input$overview_price_transform, "index")) {
      "Indexed to 100 at start of selected date range"
    } else {
      "Nominal values (in thousands AUD)"
    }
    tags$p(txt, class = "px-3",
           style = "color: var(--app-muted); font-size: 0.85rem; margin-bottom: 0;")
  })

  # Overview chart 1: Capital City Median House Prices
  output$overview_median_prices <- renderPlotly({
    show_cities <- c("Sydney", "Melbourne", "Brisbane", "Adelaide",
                     "Perth", "Hobart", "Darwin", "Canberra", "National Avg")
    d <- median_prices_combined %>%
      filter(city %in% show_cities,
             date >= input$overview_price_dates[1],
             date <= input$overview_price_dates[2])

    validate(need(nrow(d) > 0, "No median house price data available."))

    is_index <- identical(input$overview_price_transform, "index")

    if (is_index) {
      d <- d %>%
        group_by(city) %>%
        arrange(date) %>%
        mutate(plot_value = 100 * value / first(value)) %>%
        ungroup()
    } else {
      d <- d %>% mutate(plot_value = value * 1000)
    }

    price_colours <- c(
      "Sydney" = "#2196F3", "Melbourne" = "#7B1FA2", "Brisbane" = "#FF5722",
      "Adelaide" = "#984ea3", "Perth" = "#ff7f00", "Hobart" = "#a65628",
      "Darwin" = "#f781bf", "Canberra" = "#999999", "National Avg" = "#4CAF50"
    )

    y_scale <- if (is_index) {
      scale_y_continuous(labels = label_number(big.mark = ",", accuracy = 0.1))
    } else {
      scale_y_continuous(labels = label_dollar(prefix = "$", suffix = "k",
                                               scale = 1/1000, big.mark = ","))
    }

    p <- ggplot(d, aes(x = date, y = plot_value, color = city)) +
      geom_line(aes(linetype = city), linewidth = 1.1, alpha = 0.9) +
      scale_color_manual(values = price_colours) +
      scale_linetype_manual(
        values = setNames(
          ifelse(show_cities == "National Avg", "dashed", "solid"),
          show_cities
        )
      ) +
      scale_x_date(date_labels = "%Y", date_breaks = "3 years") +
      y_scale +
      labs(x = NULL, y = NULL, color = NULL, linetype = NULL) +
      theme_afford(is_dark()) +
      theme(legend.position = "none")

    # Build plotly, then add annotations outside the grid on the right
    label_data <- d %>%
      group_by(city) %>%
      filter(date == max(date)) %>%
      ungroup()

    # Repel label positions so they don't overlap
    y_range <- range(d$plot_value, na.rm = TRUE)
    min_gap <- diff(y_range) * 0.045
    label_data$y_repelled <- repel_labels(label_data$plot_value, min_gap)

    fig <- ggplotly(p, tooltip = c("x", "y", "color")) %>% plotly_layout(is_dark())

    # Add right-side annotations: xref="paper" x=1.01 places text just outside grid
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
  })

  # (Housing Serviceability chart moved to Affordability page)

  # Overview chart 3: Affordability Indices (level values)
  output$overview_afford_change <- renderPlotly({
    d <- afford_idx %>%
      filter(indicator %in% c("Rental Affordability Index",
                              "Mortgage Serviceability Index",
                              "Price-to-Income Ratio")) %>%
      mutate(indicator_label = case_when(
        indicator == "Rental Affordability Index" ~ "Rent Cost Pressure",
        indicator == "Mortgage Serviceability Index" ~ "Modelled Mortgage Cost Pressure",
        indicator == "Price-to-Income Ratio" ~ "Price-to-Income Cost Pressure"
      ))

    validate(need(nrow(d) > 0, "No affordability index data available."))

    idx_colours <- c("Rent Cost Pressure" = "#009688",
                     "Modelled Mortgage Cost Pressure" = "#FF9800",
                     "Price-to-Income Cost Pressure" = "#1565C0")

    p <- ggplot(d, aes(x = date, y = value, color = indicator_label)) +
      geom_line(linewidth = 1.1, alpha = 0.9) +
      scale_color_manual(values = idx_colours) +
      scale_x_date(date_labels = "%Y", date_breaks = "3 years") +
      scale_y_continuous(labels = label_number(big.mark = ",", accuracy = 0.1)) +
      labs(x = NULL, y = "Index value", color = NULL) +
      theme_afford(is_dark())

    # Add latest value annotations
    latest_vals <- d %>%
      group_by(indicator_label) %>%
      filter(date == max(date)) %>%
      ungroup()

    p <- p +
      geom_point(data = latest_vals, size = 3)

    ggplotly(p, tooltip = c("x", "y", "color")) %>% plotly_layout(is_dark())
  })

  # ============================================================================
  # PAGE 2: PRICE TRENDS
  # ============================================================================

  price_data <- reactive({
    req(input$price_cities)
    d <- rppi_combined %>%
      filter(city %in% input$price_cities,
             dwelling_type == input$price_dwelling,
             date >= input$price_dates[1],
             date <= input$price_dates[2])

    if (nrow(d) == 0) return(d)

    if (input$price_transform == "yoy") {
      d <- d %>%
        group_by(city) %>%
        arrange(date) %>%
        mutate(value = 100 * (value / lag(value, 4) - 1)) %>%
        filter(!is.na(value)) %>%
        ungroup()
    } else if (input$price_transform == "index") {
      d <- d %>%
        group_by(city) %>%
        arrange(date) %>%
        mutate(value = 100 * value / first(value)) %>%
        ungroup()
    }
    d
  })

  output$price_chart <- renderPlotly({
    d <- price_data()
    validate(need(nrow(d) > 0,
      "No data for selected cities/dwelling type. Try 'Total' or check dates."))

    y_lab <- switch(input$price_transform,
                    levels = "Index", yoy = "YoY %", index = "Index (start=100)")

    p <- ggplot(d, aes(x = date, y = value, color = city)) +
      geom_line(linewidth = 1, alpha = 0.9) +
      scale_color_manual(values = city_colours, na.value = "grey50") +
      scale_x_date(date_labels = "%Y", date_breaks = "3 years") +
      scale_y_continuous(labels = label_number(big.mark = ",", accuracy = 0.1)) +
      labs(x = NULL, y = y_lab, color = NULL) +
      theme_afford(is_dark())

    ggplotly(p, tooltip = c("x", "y", "color")) %>% plotly_layout(is_dark())
  })

  output$supply_cpi_construction <- renderPlotly({
    d <- abs_ts %>%
      filter(series == "CPI New Dwelling Purchase",
             date >= input$price_dates[1],
             date <= input$price_dates[2])
    validate(need(nrow(d) > 0, "No CPI construction cost data available."))

    p <- ggplot(d, aes(x = date, y = value)) +
      geom_line(linewidth = 1, color = "#0E5A8A") +
      scale_x_date(date_labels = "%Y", date_breaks = "5 years") +
      labs(x = NULL, y = "Index") +
      theme_afford(is_dark())

    ggplotly(p, tooltip = c("x", "y")) %>% plotly_layout(is_dark())
  })

  # Tab 2b: Rent CPI
  rent_cpi_data <- reactive({
    req(input$rent_cpi_cities)
    d <- rent_cpi_combined %>%
      filter(city %in% input$rent_cpi_cities,
             date >= input$rent_cpi_dates[1],
             date <= input$rent_cpi_dates[2])
    if (nrow(d) == 0) return(d)

    if (input$rent_cpi_datatype == "yoy") {
      d <- d %>%
        group_by(city) %>%
        arrange(date) %>%
        mutate(value = 100 * (value / lag(value, 4) - 1)) %>%
        filter(!is.na(value)) %>%
        ungroup()
    } else if (input$rent_cpi_datatype == "qoq") {
      d <- d %>%
        group_by(city) %>%
        arrange(date) %>%
        mutate(value = 100 * (value / lag(value, 1) - 1)) %>%
        filter(!is.na(value)) %>%
        ungroup()
    }
    d
  })

  output$rent_cpi_chart <- renderPlotly({
    d <- rent_cpi_data()
    validate(need(nrow(d) > 0, "No CPI Rents data for selected cities/dates."))

    y_lab <- switch(input$rent_cpi_datatype,
                    index = "Index",
                    yoy = "Annual change (%)",
                    qoq = "Quarterly change (%)")

    datatype_label <- switch(input$rent_cpi_datatype,
                             index = "index numbers",
                             yoy = "annual change (%)",
                             qoq = "quarterly change (%)")

    date_range_label <- paste(
      format(min(d$date), "%b %Y"), "to", format(max(d$date), "%b %Y"))

    p <- ggplot(d, aes(x = date, y = value, color = city)) +
      geom_line(linewidth = 1, alpha = 0.9) +
      scale_color_manual(values = city_colours, na.value = "grey50") +
      scale_x_date(date_labels = "%Y", date_breaks = "3 years") +
      scale_y_continuous(labels = label_number(big.mark = ",", accuracy = 0.1)) +
      labs(x = NULL, y = y_lab, color = NULL,
           title = paste0("Rent CPI, ", datatype_label,
                          ", by greater capital city, ", date_range_label)) +
      theme_afford(is_dark())

    ggplotly(p, tooltip = c("x", "y", "color")) %>% plotly_layout(is_dark())
  })

  # ============================================================================
  # PAGE 3: AFFORDABILITY
  # ============================================================================

  # Tab 3a: Indices
  output$afford_indices_chart <- renderPlotly({
    req(input$afford_indices)
    # Filter out the special "Housing Serviceability" — it has its own chart
    idx_selected <- setdiff(input$afford_indices, "Housing Serviceability")
    validate(need(length(idx_selected) > 0,
                  "Select at least one index indicator (or Housing Serviceability)."))
    d <- afford_idx %>%
      filter(indicator %in% idx_selected,
             date >= input$afford_dates[1],
             date <= input$afford_dates[2]) %>%
      mutate(indicator_label = case_when(
        indicator == "Price-to-Income Ratio" ~ "Price-to-Income Cost Pressure",
        indicator == "Mortgage Serviceability Index" ~ "Modelled Mortgage Cost Pressure",
        indicator == "Rental Affordability Index" ~ "Rent Cost Pressure",
        indicator == "Deposit Gap (Years)" ~ "Stylised Deposit Gap (Years)",
        TRUE ~ indicator
      ))
    validate(need(nrow(d) > 0, "No data for selected indicators in this date range."))

    p <- ggplot(d, aes(x = date, y = value, color = indicator_label)) +
      geom_line(linewidth = 1) +
      facet_wrap(~indicator_label, scales = "free_y") +
      scale_x_date(date_labels = "%Y", date_breaks = "5 years") +
      labs(x = NULL, y = NULL, color = NULL) +
      theme_afford(is_dark()) +
      theme(legend.position = "none")

    ggplotly(p, tooltip = c("x", "y")) %>% plotly_layout(is_dark())
  })

  # Tab 3a: Housing Serviceability chart (shown when selected)
  output$afford_serviceability <- renderPlotly({
    req("Housing Serviceability" %in% input$afford_indices)
    d <- serviceability_ts %>%
      filter(!is.na(serviceability_pct),
             date >= input$afford_dates[1],
             date <= input$afford_dates[2])

    validate(need(nrow(d) > 0, "No serviceability data in this date range."))

    p <- ggplot(d, aes(x = date, y = serviceability_pct)) +
      geom_ribbon(aes(ymin = pmin(serviceability_pct, 30), ymax = pmax(serviceability_pct, 30)),
                  fill = "#ffcdd2", alpha = 0.4) +
      geom_line(linewidth = 1.2, color = "#e53935") +
      geom_hline(yintercept = 30, linetype = "dashed", color = "#FF9800",
                 linewidth = 0.8) +
      annotate("text", x = max(d$date) - 2500, y = 31,
               label = "Housing Stress Threshold (30%)",
               color = "#FF9800", size = 3.5, hjust = 0, vjust = 0) +
      scale_x_date(date_labels = "%Y", date_breaks = "3 years") +
      scale_y_continuous(labels = label_percent(scale = 1, accuracy = 0.1)) +
      labs(x = NULL, y = NULL) +
      theme_afford(is_dark())

    ggplotly(p, tooltip = c("x", "y")) %>% plotly_layout(is_dark())
  })

  # Tab 3b: Calculator
  calc_vals <- reactive({
    price   <- input$calc_price
    income  <- input$calc_income
    rate    <- input$calc_rate / 100
    dep_pct <- input$calc_deposit_pct / 100
    term    <- input$calc_term
    save_rt <- input$calc_savings_rate / 100

    deposit     <- price * dep_pct
    loan        <- price - deposit
    monthly_r   <- rate / 12
    n_payments  <- term * 12
    monthly_pmt <- if (monthly_r == 0) loan / n_payments else
      loan * monthly_r / (1 - (1 + monthly_r)^(-n_payments))
    total_paid  <- monthly_pmt * n_payments
    total_int   <- total_paid - loan
    ratio_pct   <- (monthly_pmt * 12) / income * 100
    years_save  <- deposit / (income * save_rt)
    lvr         <- (1 - dep_pct) * 100

    list(
      repayment = monthly_pmt,
      ratio     = ratio_pct,
      years     = years_save,
      lvr       = lvr,
      total_int = total_int,
      deposit   = deposit
    )
  })

  output$calc_repayment     <- renderText(fmt_dollar(calc_vals()$repayment))
  output$calc_ratio         <- renderText(fmt_pct(calc_vals()$ratio, 0.1))
  output$calc_years         <- renderText(fmt_years(calc_vals()$years))
  output$calc_lvr           <- renderText(fmt_pct(calc_vals()$lvr, 1))
  output$calc_total_interest <- renderText(fmt_dollar(calc_vals()$total_int))
  output$calc_deposit_amt   <- renderText(fmt_dollar(calc_vals()$deposit))

  # Tab 3c: Stress bands
  output$stress_chart <- renderPlotly({
    bd <- input$stress_breakdown
    pop <- input$stress_population

    # Demographic breakdowns (age, family, etc.) use tenure="all".
    # Tenure breakdowns (owner, renter) use breakdown_var="owner"/"renter"
    # with tenure-specific rows.
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

    # Clean metric labels
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

    ggplotly(p, tooltip = c("x", "y", "fill")) %>% plotly_layout(is_dark())
  })

  # Tab 3d: Cost burden heatmap
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

    ggplotly(p, tooltip = c("x", "y", "fill")) %>% plotly_layout(is_dark())
  })

  # ============================================================================
  # PAGE 4: MARKET CONTEXT
  # ============================================================================

  # --- Value boxes ---
  output$vb_unemp <- renderText({
    v <- latest_val(abs_ts, "series", "Unemployment Rate")
    fmt_pct(v, 0.1)
  })
  output$vb_unemp_change <- renderUI({
    ch <- latest_change(abs_ts, "series", "Unemployment Rate",
                        periods_back = 12, period_label = "YoY",
                        change_type = "percentage_points")
    diff_val <- ch$change
    # For unemployment, down is good (green), up is bad (red)
    css_class <- if (!is.na(diff_val) && diff_val <= 0) "kpi-change-up" else "kpi-change-down"
    tags$p(class = paste("kpi-subtitle", css_class), ch$label)
  })

  output$vb_nom <- renderText({
    d <- supply_demand %>%
      filter(str_detect(series, "Net Overseas Migration"),
             !is.na(value)) %>%
      arrange(desc(date))
    if (nrow(d) < 4) return("N/A")
    # Sum last 4 quarters for annual estimate
    annual <- sum(d$value[1:4], na.rm = TRUE)
    paste0(round(annual), "k")
  })
  output$vb_nom_change <- renderUI({
    d <- supply_demand %>%
      filter(str_detect(series, "Net Overseas Migration"),
             !is.na(value)) %>%
      arrange(desc(date))
    if (nrow(d) < 8) return(tags$p(class = "kpi-subtitle", ""))
    current_annual <- sum(d$value[1:4], na.rm = TRUE)
    previous_annual <- sum(d$value[5:8], na.rm = TRUE)
    if (previous_annual == 0) return(tags$p(class = "kpi-subtitle", ""))
    pct <- (current_annual / previous_annual - 1) * 100
    direction <- if (pct >= 0) "\u2191" else "\u2193"
    label <- paste0(direction, " ", sprintf("%+.0f%%", pct), " YoY")
    css_class <- if (pct >= 0) "kpi-change-up" else "kpi-change-down"
    tags$p(class = paste("kpi-subtitle", css_class), label)
  })

  output$vb_participation <- renderText({
    v <- latest_val(abs_ts, "series", "Participation Rate")
    fmt_pct(v, 0.1)
  })
  output$vb_participation_change <- renderUI({
    ch <- latest_change(abs_ts, "series", "Participation Rate",
                        periods_back = 12, period_label = "YoY",
                        change_type = "percentage_points")
    diff_val <- ch$change
    css_class <- if (!is.na(diff_val) && diff_val >= 0) "kpi-change-up" else "kpi-change-down"
    lbl <- if (is.na(diff_val)) "" else if (abs(diff_val) < 0.3) "\u2192 Stable" else ch$label
    tags$p(class = paste("kpi-subtitle", css_class), lbl)
  })

  # --- Charts ---
  output$context_rates <- renderPlotly({
    d <- bind_rows(
      rba_cash_rate %>% mutate(series = "RBA Cash Rate"),
      rba_mortgage_var %>% mutate(series = "Owner-occ Variable (Discounted)"),
      rba_mortgage_fixed %>% mutate(series = "Owner-occ 3yr Fixed"),
      rba_investor_var %>% mutate(series = "Investor Variable (Discounted)"),
      rba_investor_fixed %>% mutate(series = "Investor 3yr Fixed")
    ) %>%
      distinct(date, series, .keep_all = TRUE) %>%
      filter(date >= input$context_dates[1],
             date <= input$context_dates[2])

    validate(need(nrow(d) > 0, "No rate data available."))

    rate_colours <- c(
      "RBA Cash Rate" = "#1B5E20",
      "Owner-occ Variable (Discounted)" = "#2196F3",
      "Owner-occ 3yr Fixed" = "#1565C0",
      "Investor Variable (Discounted)" = "#FF9800",
      "Investor 3yr Fixed" = "#E65100"
    )

    p <- ggplot(d, aes(x = date, y = value, color = series)) +
      geom_line(linewidth = 1, alpha = 0.9) +
      scale_color_manual(values = rate_colours) +
      scale_x_date(date_labels = "%Y", date_breaks = "5 years") +
      scale_y_continuous(labels = label_number(big.mark = ",", accuracy = 0.1)) +
      labs(x = NULL, y = "%", color = NULL) +
      theme_afford(is_dark())

    ggplotly(p, tooltip = c("x", "y", "color")) %>% plotly_layout(is_dark())
  })

  output$context_labour <- renderPlotly({
    d <- abs_ts %>%
      filter(series %in% c("Unemployment Rate", "Underemployment Rate",
                            "Labour Underutilisation Rate"),
             date >= input$context_dates[1],
             date <= input$context_dates[2]) %>%
      mutate(series = factor(series,
        levels = c("Labour Underutilisation Rate", "Underemployment Rate",
                   "Unemployment Rate")))
    validate(need(nrow(d) > 0, "No labour market data available."))

    labour_fills <- c("Unemployment Rate" = "#2196F3",
                      "Underemployment Rate" = "#AB47BC",
                      "Labour Underutilisation Rate" = "#78909C")

    p <- ggplot(d, aes(x = date, y = value, fill = series)) +
      geom_area(alpha = 0.6, linewidth = 0.5, colour = "white") +
      scale_fill_manual(values = labour_fills) +
      scale_x_date(date_labels = "%Y", date_breaks = "5 years") +
      scale_y_continuous(labels = label_percent(scale = 1, accuracy = 0.1)) +
      labs(x = NULL, y = NULL, fill = NULL) +
      theme_afford(is_dark())

    ggplotly(p, tooltip = c("x", "y", "fill")) %>% plotly_layout(is_dark())
  })

  output$context_pop <- renderPlotly({
    d <- supply_demand %>%
      filter(str_detect(series, "Net Overseas Migration"),
             date >= input$context_dates[1],
             date <= input$context_dates[2])
    validate(need(nrow(d) > 0,
      "Run pipeline/05_driver.R to fetch population data (ABS 3101.0)"))

    p <- ggplot(d, aes(x = date, y = value)) +
      geom_col(fill = "#29B6F6", alpha = 0.85, width = 60) +
      scale_x_date(date_labels = "%Y", date_breaks = "5 years") +
      scale_y_continuous(labels = label_number(big.mark = ",", accuracy = 1)) +
      labs(x = NULL, y = "Thousands") +
      theme_afford(is_dark())

    ggplotly(p, tooltip = c("x", "y")) %>% plotly_layout(is_dark())
  })

  # ============================================================================
  # PAGE 5: HOUSING SUPPLY
  # ============================================================================

  # --- Value boxes ---

  # Helper: get latest total approvals for a state
  approvals_latest <- function(state_name) {
    supply_demand %>%
      filter(category == "Building Approvals",
             str_detect(series, state_name),
             str_detect(series, "Total \\(Type of Building\\)"),
             str_detect(series, "Total Sectors")) %>%
      filter(!is.na(value)) %>%
      arrange(desc(date)) %>%
      slice(1)
  }

  output$vb_approvals_nsw <- renderText({
    d <- approvals_latest("New South Wales")
    if (nrow(d) == 0) return("N/A")
    fmt_number(d$value[1])
  })
  output$vb_approvals_nsw_change <- renderUI({
    d <- supply_demand %>%
      filter(category == "Building Approvals",
             str_detect(series, "New South Wales"),
             str_detect(series, "Total \\(Type of Building\\)"),
             str_detect(series, "Total Sectors"),
             !is.na(value)) %>%
      arrange(desc(date))
    if (nrow(d) < 13) return(tags$p(class = "kpi-subtitle", ""))
    current <- d$value[1]; previous <- d$value[13]
    if (is.na(previous) || previous == 0) return(tags$p(class = "kpi-subtitle", ""))
    pct <- (current / previous - 1) * 100
    direction <- if (pct >= 0) "\u2191" else "\u2193"
    label <- paste0(direction, " ", sprintf("%+.1f%%", pct), " YoY")
    css_class <- if (pct >= 0) "kpi-change-up" else "kpi-change-down"
    tags$p(class = paste("kpi-subtitle", css_class), label)
  })

  output$vb_approvals_vic <- renderText({
    d <- approvals_latest("Victoria")
    if (nrow(d) == 0) return("N/A")
    fmt_number(d$value[1])
  })
  output$vb_approvals_vic_change <- renderUI({
    d <- supply_demand %>%
      filter(category == "Building Approvals",
             str_detect(series, "Victoria"),
             str_detect(series, "Total \\(Type of Building\\)"),
             str_detect(series, "Total Sectors"),
             !is.na(value)) %>%
      arrange(desc(date))
    if (nrow(d) < 13) return(tags$p(class = "kpi-subtitle", ""))
    current <- d$value[1]; previous <- d$value[13]
    if (is.na(previous) || previous == 0) return(tags$p(class = "kpi-subtitle", ""))
    pct <- (current / previous - 1) * 100
    direction <- if (pct >= 0) "\u2191" else "\u2193"
    label <- paste0(direction, " ", sprintf("%+.1f%%", pct), " YoY")
    css_class <- if (pct >= 0) "kpi-change-up" else "kpi-change-down"
    tags$p(class = paste("kpi-subtitle", css_class), label)
  })

  output$vb_construction <- renderText({
    v <- latest_val(abs_ts, "series", "CPI New Dwelling Purchase")
    fmt_index(v)
  })
  output$vb_construction_change <- renderUI({
    ch <- latest_change(abs_ts, "series", "CPI New Dwelling Purchase",
                        periods_back = 12, period_label = "YoY",
                        change_type = "relative_pct")
    diff_val <- ch$change
    css_class <- if (!is.na(diff_val) && diff_val >= 0) "kpi-change-up" else "kpi-change-down"
    lbl <- if (is.na(diff_val)) "" else ch$label
    tags$p(class = paste("kpi-subtitle", css_class), lbl)
  })

  output$vb_houses_share <- renderText({
    # Houses as % of total approvals (NSW + VIC combined), latest month
    d_total <- supply_demand %>%
      filter(category == "Building Approvals",
             str_detect(series, "Total \\(Type of Building\\)"),
             str_detect(series, "Total Sectors"),
             !is.na(value)) %>%
      arrange(desc(date))
    if (nrow(d_total) == 0) return("N/A")
    latest_month <- d_total$date[1]

    total_val <- d_total %>% filter(date == latest_month) %>% summarise(s = sum(value)) %>% pull(s)
    houses_val <- supply_demand %>%
      filter(category == "Building Approvals",
             str_detect(series, "Houses"),
             str_detect(series, "Total Sectors"),
             !is.na(value),
             date == latest_month) %>%
      summarise(s = sum(value)) %>% pull(s)

    if (total_val == 0) return("N/A")
    fmt_pct(houses_val / total_val * 100, 0.1)
  })
  output$vb_houses_share_change <- renderUI({
    # Compare houses share: latest month vs 12 months prior
    calc_share <- function(target_date) {
      total_val <- supply_demand %>%
        filter(category == "Building Approvals",
               str_detect(series, "Total \\(Type of Building\\)"),
               str_detect(series, "Total Sectors"),
               !is.na(value), date == target_date) %>%
        summarise(s = sum(value)) %>% pull(s)
      houses_val <- supply_demand %>%
        filter(category == "Building Approvals",
               str_detect(series, "Houses"),
               str_detect(series, "Total Sectors"),
               !is.na(value), date == target_date) %>%
        summarise(s = sum(value)) %>% pull(s)
      if (total_val == 0) return(NA_real_)
      houses_val / total_val * 100
    }

    dates <- supply_demand %>%
      filter(category == "Building Approvals",
             str_detect(series, "Total \\(Type of Building\\)"),
             str_detect(series, "Total Sectors"),
             !is.na(value)) %>%
      distinct(date) %>% arrange(desc(date)) %>% pull(date)

    if (length(dates) < 13) return(tags$p(class = "kpi-subtitle", ""))
    current_share <- calc_share(dates[1])
    previous_share <- calc_share(dates[13])
    if (is.na(current_share) || is.na(previous_share)) return(tags$p(class = "kpi-subtitle", ""))

    diff_pp <- current_share - previous_share
    direction <- if (diff_pp >= 0) "\u2191" else "\u2193"
    label <- paste0(direction, " ", sprintf("%+.1f pp", diff_pp), " YoY")
    css_class <- if (diff_pp >= 0) "kpi-change-up" else "kpi-change-down"
    tags$p(class = paste("kpi-subtitle", css_class), label)
  })

  # --- Chart ---
  output$supply_approvals <- renderPlotly({
    d <- supply_demand %>%
      filter(category == "Building Approvals",
             date >= input$supply_dates[1],
             date <= input$supply_dates[2]) %>%
      # Shorten long series names for readable legends
      mutate(series_short = series %>%
        str_remove("Total number of dwelling units ;\\s*") %>%
        str_remove("\\s*;\\s*$") %>%
        str_replace(";\\s*", " - ") %>%
        str_replace(";\\s*", " - "))

    validate(need(nrow(d) > 0,
      "Run pipeline/05_driver.R to fetch building approvals data (ABS 8731.0)"))

    p <- ggplot(d, aes(x = date, y = value, color = series_short)) +
      geom_line(linewidth = 0.8, alpha = 0.85) +
      scale_x_date(date_labels = "%Y", date_breaks = "5 years") +
      scale_y_continuous(labels = label_number(big.mark = ",")) +
      labs(x = NULL, y = "Number of Dwellings", color = NULL) +
      theme_afford(is_dark()) +
      theme(legend.text = element_text(size = 8))

    ggplotly(p, tooltip = c("x", "y", "color")) %>% plotly_layout(is_dark())
  })

  # ============================================================================
  # PAGE 6: RENTAL MARKET
  # ============================================================================

  # Rental stress by state (bar chart for selected year)
  output$rental_stress_state <- renderPlotly({
    yr <- input$rental_year

    d <- sih_nhha %>%
      filter(survey_year == yr,
             metric == "pct_rental_stress_over_30",
             geography != "Aust.")

    # Apply state filter
    if (!is.null(input$rental_states) && !"all" %in% input$rental_states) {
      d <- d %>% filter(geography %in% input$rental_states)
    }

    validate(need(nrow(d) > 0, "No NHHA rental stress data for selected year."))

    # Add national average line
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

    ggplotly(p, tooltip = c("x", "y")) %>% plotly_layout(is_dark())
  })

  # Rental stress trends over time
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
        # Order: Aust. first, then states alphabetically
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

    pl <- ggplotly(p, tooltip = "fill") %>% plotly_layout(dark)
    # Remove hover from the text trace to avoid doubling
    for (i in seq_along(pl$x$data)) {
      if (!is.null(pl$x$data[[i]]$mode) && grepl("text", pl$x$data[[i]]$mode)) {
        pl$x$data[[i]]$hoverinfo <- "skip"
      }
    }
    pl
  })

  # Rental affordability index
  output$rental_afford_index <- renderPlotly({
    d <- afford_idx %>%
      filter(indicator == "Rental Affordability Index")
    validate(need(nrow(d) > 0, "No rental affordability index data."))

    p <- ggplot(d, aes(x = date, y = value)) +
      geom_line(linewidth = 1, color = "#e74c3c") +
      scale_x_date(date_labels = "%Y", date_breaks = "5 years") +
      labs(x = NULL, y = "Index (CPI Rents / WPI)") +
      theme_afford(is_dark())

    ggplotly(p, tooltip = c("x", "y")) %>% plotly_layout(is_dark())
  })

  # Rental costs by demographics
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

    ggplotly(p, tooltip = c("x", "y", "fill")) %>% plotly_layout(is_dark())
  })
}

# ==============================================================================
# RUN
# ==============================================================================
shinyApp(ui, server)

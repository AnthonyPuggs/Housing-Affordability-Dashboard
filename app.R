# ==============================================================================
# Australian Housing Affordability Dashboard
# ==============================================================================
# Shiny app shell — reads pre-processed CSVs from data/ pipeline
# 7 pages: Overview, Price Trends, Affordability, Market Context, Housing Supply, Rental Market, Methodology
# ==============================================================================

library(shiny)
library(bslib)
library(plotly)

# Shared project paths, data loading, helpers, theme, and pre-computed datasets
.load_app_project_paths <- function(envir = parent.frame()) {
  source_file <- NULL
  frames <- sys.frames()
  for (i in rev(seq_along(frames))) {
    frame <- frames[[i]]
    if (exists("ofile", envir = frame, inherits = FALSE)) {
      source_file <- get("ofile", envir = frame, inherits = FALSE)
      break
    }
  }

  starts <- unique(c(
    if (!is.null(source_file)) dirname(normalizePath(source_file, winslash = "/", mustWork = TRUE)),
    getwd()
  ))
  candidates <- unique(c(
    file.path(starts, "R", "project_paths.R"),
    file.path(dirname(starts), "R", "project_paths.R")
  ))
  candidates <- candidates[file.exists(candidates)]
  if (length(candidates) == 0) {
    stop("Could not locate R/project_paths.R for dashboard startup.", call. = FALSE)
  }
  source(candidates[[1]], local = envir)
}

.load_app_project_paths()
source(project_path("plot_setup.R"), local = TRUE)
source(project_path("R", "app_ui_helpers.R"), local = TRUE)
source(project_path("R", "plotly_helpers.R"), local = TRUE)
source(project_path("R", "provenance_report.R"), local = TRUE)
source(project_path("R", "methodology_module.R"), local = TRUE)
source(project_path("R", "affordability_module.R"), local = TRUE)
source(project_path("R", "rental_market_module.R"), local = TRUE)
source(project_path("R", "housing_supply_module.R"), local = TRUE)
rm(.load_app_project_paths)

overview_cost_pressure_indicators <- c(
  "Rental Affordability Index",
  "Mortgage Serviceability Index",
  "Price-to-Income Ratio"
)
overview_cost_pressure_colours <- stats::setNames(
  c("#009688", "#FF9800", "#1565C0"),
  indicator_chart_label(overview_cost_pressure_indicators)
)

# ==============================================================================
# UI
# ==============================================================================

ui <- page_navbar(
  title = "Australian Housing Affordability",
  id = "main_nav",
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

      .methodology-table-wrap {
        overflow-x: auto;
        width: 100%;
      }
      .methodology-table-wrap table {
        min-width: 1100px;
        font-size: 0.85rem;
      }
      html[data-theme='dark'] .methodology-table-wrap table,
      html[data-theme='dark'] .methodology-table-wrap th,
      html[data-theme='dark'] .methodology-table-wrap td {
        color: var(--app-text) !important;
        background-color: var(--app-panel) !important;
        border-color: var(--app-border) !important;
      }

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
        .navbar-collapse.show {
          max-height: 70vh;
          overflow-y: auto;
        }
        .navbar-collapse.show .nav-link {
          padding-top: 0.65rem;
          padding-bottom: 0.65rem;
        }
        .bslib-sidebar-layout { flex-direction: column !important; }
        .bslib-sidebar-layout > .sidebar { width: 100% !important; max-width: 100% !important; }
        .chart-wide,
        .chart-square {
          height: 340px;
          min-height: 320px;
        }
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

        $(document).on('click', '.navbar-collapse.show .nav-link', function(e) {
          if (window.innerWidth < 992) {
            var mainNav = document.getElementById('main_nav');
            var collapseEl = e.currentTarget.closest('.navbar-collapse.show');
            if (!collapseEl) return;
            if (mainNav && !collapseEl.contains(mainNav) && !e.currentTarget.closest('#main_nav')) {
              return;
            }
            if (window.bootstrap && bootstrap.Collapse) {
              bootstrap.Collapse.getOrCreateInstance(collapseEl, { toggle: false }).hide();
            }
          }
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
  affordabilityPageUI("affordability"),

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
  housingSupplyPageUI("housing_supply"),

  # ============================================================================
  # PAGE 6: RENTAL MARKET
  # ============================================================================
  rentalMarketPageUI("rental_market"),

  # ============================================================================
  # PAGE 7: METHODOLOGY
  # ============================================================================
  methodologyPageUI("methodology")
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

  methodologyPageServer("methodology")
  affordabilityPageServer("affordability", is_dark = is_dark)
  rentalMarketPageServer("rental_market", is_dark = is_dark)
  housingSupplyPageServer("housing_supply", is_dark = is_dark)

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

    fig <- dashboard_ggplotly(p, dark = is_dark(),
                              tooltip = c("x", "y", "color"))

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
  }) %>%
    bindCache(input$overview_price_dates, input$overview_price_transform,
              is_dark())

  # (Housing Serviceability chart moved to Affordability page)

  # Overview chart 3: Affordability Indices (level values)
  output$overview_afford_change <- renderPlotly({
    d <- afford_idx %>%
      filter(indicator %in% overview_cost_pressure_indicators) %>%
      mutate(indicator_label = indicator_chart_label(indicator))

    validate(need(nrow(d) > 0, "No affordability index data available."))

    p <- ggplot(d, aes(x = date, y = value, color = indicator_label)) +
      geom_line(linewidth = 1.1, alpha = 0.9) +
      scale_color_manual(values = overview_cost_pressure_colours) +
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

    dashboard_ggplotly(p, dark = is_dark(), tooltip = c("x", "y", "color"))
  }) %>%
    bindCache(is_dark())

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

    dashboard_ggplotly(p, dark = is_dark(), tooltip = c("x", "y", "color"))
  }) %>%
    bindCache(input$price_cities, input$price_dwelling, input$price_dates,
              input$price_transform, is_dark())

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

    dashboard_ggplotly(p, dark = is_dark(), tooltip = c("x", "y", "color"))
  }) %>%
    bindCache(input$rent_cpi_cities, input$rent_cpi_dates,
              input$rent_cpi_datatype, is_dark())

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

    dashboard_ggplotly(p, dark = is_dark(), tooltip = c("x", "y", "color"))
  }) %>%
    bindCache(input$context_dates, is_dark())

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

    dashboard_ggplotly(p, dark = is_dark(), tooltip = c("x", "y", "fill"))
  }) %>%
    bindCache(input$context_dates, is_dark())

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

    dashboard_ggplotly(p, dark = is_dark(), tooltip = c("x", "y"))
  }) %>%
    bindCache(input$context_dates, is_dark())

}

# ==============================================================================
# RUN
# ==============================================================================
shinyApp(ui, server)

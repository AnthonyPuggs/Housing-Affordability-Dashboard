# ==============================================================================
# Australian Housing Affordability Dashboard
# ==============================================================================
# Shiny app shell — reads pre-processed CSVs from data/ pipeline
# 8 pages: Overview, Price Trends, Affordability, Geographic Affordability, Market Context, Housing Supply, Rental Market, Methodology
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
source(project_path("R", "market_entry_scenarios.R"), local = TRUE)
source(project_path("R", "provenance_report.R"), local = TRUE)
source(project_path("R", "methodology_module.R"), local = TRUE)
source(project_path("R", "affordability_module.R"), local = TRUE)
source(project_path("R", "rental_market_module.R"), local = TRUE)
source(project_path("R", "housing_supply_module.R"), local = TRUE)
source(project_path("R", "price_trends_module.R"), local = TRUE)
source(project_path("R", "geographic_affordability_module.R"), local = TRUE)
source(project_path("R", "market_context_module.R"), local = TRUE)
source(project_path("R", "overview_module.R"), local = TRUE)
rm(.load_app_project_paths)

# ==============================================================================
# UI
# ==============================================================================

ui <- page_navbar(
  title = "Australian Housing Affordability",
  id = "main_nav",
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    base_font = font_collection(
      "-apple-system",
      "BlinkMacSystemFont",
      "Segoe UI",
      "Roboto",
      "Helvetica Neue",
      "Arial",
      "sans-serif"
    ),
    heading_font = font_collection(
      "Segoe UI",
      "Roboto",
      "Helvetica Neue",
      "Arial",
      "sans-serif"
    ),
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
      html[data-bs-theme='dark'] {
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
      html[data-bs-theme='dark'] .navbar {
        background-color: #0d1a2d !important;
        border-bottom: 1px solid var(--app-border);
      }
      html[data-bs-theme='dark'] .navbar .navbar-brand,
      html[data-bs-theme='dark'] .navbar .nav-link,
      html[data-bs-theme='dark'] .navbar .navbar-toggler-icon {
        color: #e3ebf4 !important;
      }
      html[data-bs-theme='dark'] .navbar .nav-link:hover,
      html[data-bs-theme='dark'] .navbar .nav-link.active {
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
      html[data-bs-theme='dark'] .nav-tabs .nav-link,
      html[data-bs-theme='dark'] .nav-pills .nav-link {
        color: var(--app-muted) !important;
      }
      html[data-bs-theme='dark'] .nav-tabs .nav-link.active,
      html[data-bs-theme='dark'] .nav-pills .nav-link.active {
        color: #ffffff !important;
        background-color: var(--app-panel) !important;
        border-color: var(--app-border) !important;
      }

      /* Selectize tag items in multi-select */
      html[data-bs-theme='dark'] .selectize-input .item {
        background-color: #1e2d44 !important;
        color: #e3ebf4 !important;
        border-color: #3a4f6e !important;
      }

      /* Date range input */
      html[data-bs-theme='dark'] .input-daterange .input-group-text {
        background-color: var(--app-panel) !important;
        color: var(--app-text) !important;
        border-color: var(--app-border) !important;
      }

      /* Radio & checkbox circles/checks */
      html[data-bs-theme='dark'] .form-check-input {
        background-color: var(--app-border) !important;
        border-color: var(--app-muted) !important;
      }
      html[data-bs-theme='dark'] .form-check-input:checked {
        background-color: #0E5A8A !important;
        border-color: #0E5A8A !important;
      }

      /* Slider (IRS) */
      html[data-bs-theme='dark'] .irs--shiny .irs-line,
      html[data-bs-theme='dark'] .irs--shiny .irs-grid-pol {
        background-color: var(--app-border) !important;
      }
      html[data-bs-theme='dark'] .irs--shiny .irs-min,
      html[data-bs-theme='dark'] .irs--shiny .irs-max,
      html[data-bs-theme='dark'] .irs--shiny .irs-single,
      html[data-bs-theme='dark'] .irs--shiny .irs-from,
      html[data-bs-theme='dark'] .irs--shiny .irs-to,
      html[data-bs-theme='dark'] .irs--shiny .irs-grid-text {
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
      html[data-bs-theme='dark'] .methodology-table-wrap table,
      html[data-bs-theme='dark'] .methodology-table-wrap th,
      html[data-bs-theme='dark'] .methodology-table-wrap td {
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
          max-height: 52vh;
          overflow-y: auto;
        }
        .navbar-collapse.show .nav-link {
          padding-top: 0.48rem;
          padding-bottom: 0.48rem;
        }
        .bslib-sidebar-layout { flex-direction: column !important; }
        .bslib-sidebar-layout > .sidebar { width: 100% !important; max-width: 100% !important; }
        .rental-market-page .rental-market-grid {
          width: 100%;
        }
        .rental-market-page .rental-market-grid > .bslib-grid {
          grid-template-columns: minmax(0, 1fr) !important;
          grid-auto-rows: auto !important;
        }
        .rental-market-page .rental-market-grid .bslib-grid-item,
        .rental-market-page .rental-market-grid .card {
          min-width: 0;
          width: 100% !important;
        }
        .rental-market-page .rental-market-chart {
          height: 360px;
          min-height: 360px;
        }
        .rental-market-page .rental-market-chart-square {
          height: 390px;
          min-height: 390px;
        }
        .rental-market-page .rental-market-chart-trend {
          height: 460px;
          min-height: 460px;
        }
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
  nav_item(input_dark_mode(id = "theme_mode")),

  # ============================================================================
  # PAGE 1: OVERVIEW
  # ============================================================================
  overviewPageUI("overview"),

  # ============================================================================
  # PAGE 2: PRICE TRENDS
  # ============================================================================
  priceTrendsPageUI("price_trends"),

  # ============================================================================
  # PAGE 3: AFFORDABILITY DEEP DIVE
  # ============================================================================
  affordabilityPageUI("affordability"),

  # ============================================================================
  # PAGE 4: GEOGRAPHIC AFFORDABILITY
  # ============================================================================
  geographicAffordabilityPageUI("geographic_affordability"),

  # ============================================================================
  # PAGE 5: MARKET CONTEXT (Labour & Demographics)
  # ============================================================================
  marketContextPageUI("market_context"),

  # ============================================================================
  # PAGE 6: HOUSING SUPPLY
  # ============================================================================
  housingSupplyPageUI("housing_supply"),

  # ============================================================================
  # PAGE 7: RENTAL MARKET
  # ============================================================================
  rentalMarketPageUI("rental_market"),

  # ============================================================================
  # PAGE 8: METHODOLOGY
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
    isTRUE(mode) || identical(mode, "dark")
  })

  methodologyPageServer("methodology")
  affordabilityPageServer("affordability", is_dark = is_dark)
  rentalMarketPageServer("rental_market", is_dark = is_dark)
  housingSupplyPageServer("housing_supply", is_dark = is_dark)
  priceTrendsPageServer("price_trends", is_dark = is_dark)
  geographicAffordabilityPageServer("geographic_affordability", is_dark = is_dark)
  marketContextPageServer("market_context", is_dark = is_dark)
  overviewPageServer("overview", is_dark = is_dark)

}

# ==============================================================================
# RUN
# ==============================================================================
shinyApp(ui, server)

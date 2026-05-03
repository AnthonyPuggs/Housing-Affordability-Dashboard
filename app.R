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
source(project_path("R", "visual_semantics.R"), local = TRUE)
source(project_path("R", "ui_style_system.R"), local = TRUE)
source(project_path("R", "app_ui_helpers.R"), local = TRUE)
source(project_path("R", "plotly_helpers.R"), local = TRUE)
source(project_path("R", "chart_builders.R"), local = TRUE)
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
        --app-bg: #f7f9fc;
        --app-text: #182231;
        --app-panel: #ffffff;
        --app-border: #d8e0ea;
        --app-muted: #5f6f82;
        --policy-surface: #ffffff;
        --policy-surface-subtle: #f2f6fa;
        --policy-accent: #0E5A8A;
        --policy-accent-soft: #E6F1F7;
        --policy-border-strong: #bdc9d7;
        --policy-kpi-bg: #ffffff;
        --policy-kpi-fg: #182231;
        --policy-shadow: 0 1px 2px rgba(15, 23, 42, 0.06);
      }
      /* ---- Dark theme ---- */
      html[data-bs-theme='dark'] {
        --app-bg: #0b1220;
        --app-text: #e3ebf4;
        --app-panel: #111b2e;
        --app-border: #2a3a54;
        --app-muted: #8899aa;
        --policy-surface: #111b2e;
        --policy-surface-subtle: #0f172a;
        --policy-accent: #7dbce5;
        --policy-accent-soft: #17304a;
        --policy-border-strong: #3a4f6e;
        --policy-kpi-bg: #111b2e;
        --policy-kpi-fg: #e3ebf4;
        --policy-shadow: 0 1px 2px rgba(0, 0, 0, 0.28);
      }

      /* Base */
      body {
        background-color: var(--app-bg) !important;
        color: var(--app-text) !important;
        font-size: 15px;
        line-height: 1.45;
      }
      .bslib-page-fill { background-color: var(--app-bg) !important; }

      /* Navbar */
      .navbar {
        background-color: var(--policy-surface) !important;
        border-bottom: 1px solid var(--app-border);
        box-shadow: var(--policy-shadow);
      }
      .navbar .navbar-brand {
        font-weight: 700;
        letter-spacing: 0;
        color: var(--app-text) !important;
      }
      .navbar .nav-link {
        color: var(--app-muted) !important;
        font-weight: 600;
        border-bottom: 2px solid transparent;
      }
      .navbar .nav-link:hover,
      .navbar .nav-link.active {
        color: var(--app-text) !important;
        border-bottom-color: var(--policy-accent);
      }
      .navbar .navbar-toggler,
      .navbar .navbar-toggle {
        min-width: 44px;
        min-height: 40px;
        background-color: var(--policy-surface-subtle) !important;
        border: 1px solid var(--app-border) !important;
        color: var(--app-text);
      }
      .navbar .navbar-toggle .icon-bar {
        background-color: var(--app-text) !important;
      }
      .navbar .navbar-toggler-icon {
        background-image: url(\"data:image/svg+xml,%3csvg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 30 30'%3e%3cpath stroke='rgba%2824%2C34%2C49%2C0.88%29' stroke-linecap='round' stroke-miterlimit='10' stroke-width='2' d='M4 7h22M4 15h22M4 23h22'/%3e%3c/svg%3e\") !important;
      }
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
      html[data-bs-theme='dark'] .navbar .navbar-toggler-icon {
        background-image: url(\"data:image/svg+xml,%3csvg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 30 30'%3e%3cpath stroke='rgba%28227%2C235%2C244%2C0.9%29' stroke-linecap='round' stroke-miterlimit='10' stroke-width='2' d='M4 7h22M4 15h22M4 23h22'/%3e%3c/svg%3e\") !important;
      }

      /* Cards and panels */
      .card, .sidebar, .bslib-sidebar-layout > .sidebar {
        background-color: var(--app-panel) !important;
        color: var(--app-text) !important;
        border-color: var(--app-border) !important;
        border-radius: 8px !important;
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
        border-radius: 6px !important;
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

      /* Public-policy report UI system */
      .policy-page-header {
        display: flex;
        justify-content: space-between;
        align-items: flex-end;
        gap: 1rem;
        margin: 0.35rem 0 1.15rem;
        padding: 0.15rem 0.25rem 0.9rem;
        border-bottom: 1px solid var(--app-border);
      }
      .policy-page-title {
        font-size: clamp(1.55rem, 2.6vw, 2.15rem);
        line-height: 1.12;
        font-weight: 750;
        color: var(--app-text);
        margin: 0;
      }
      .policy-page-subtitle {
        max-width: 760px;
        color: var(--app-muted);
        margin: 0.35rem 0 0;
        font-size: 0.96rem;
      }
      .policy-page-actions {
        display: flex;
        align-items: center;
        gap: 0.5rem;
      }
      .policy-card {
        border: 1px solid var(--app-border) !important;
        border-radius: 8px !important;
        box-shadow: var(--policy-shadow);
      }
      .policy-card-header {
        padding: 0.72rem 1rem !important;
        border-bottom: 1px solid var(--app-border) !important;
      }
      .policy-card-title {
        font-size: 0.98rem;
        font-weight: 700;
        letter-spacing: 0;
      }
      .policy-card-body {
        padding: 0.95rem 1rem !important;
      }
      .policy-chart-card .policy-card-body {
        padding: 0.75rem 0.9rem 0.9rem !important;
      }
      .policy-card-body-with-note {
        display: flex;
        flex-direction: column;
        gap: 0.75rem;
      }
      .policy-card-body-with-note > .policy-source-note {
        padding: 0 !important;
      }
      .policy-source-note {
        color: var(--app-muted);
        font-size: 0.78rem;
        line-height: 1.42;
        margin: 0;
        padding: 0.65rem 1rem 0 !important;
      }
      .policy-kpi {
        background-color: var(--policy-kpi-bg) !important;
        color: var(--policy-kpi-fg) !important;
        border: 1px solid var(--app-border) !important;
        border-left: 4px solid var(--policy-accent) !important;
        border-radius: 8px !important;
        box-shadow: var(--policy-shadow);
      }
      .policy-kpi-blue { border-left-color: #0E5A8A !important; }
      .policy-kpi-teal { border-left-color: #1F9D8C !important; }
      .policy-kpi-navy { border-left-color: #17415F !important; }
      .policy-kpi-purple { border-left-color: #7B5AA6 !important; }
      .value-box .value-box-value { font-size: 1.55rem; font-weight: 750; }
      .value-box .value-box-title { font-size: 0.82rem; opacity: 0.9; font-weight: 700; }
      .kpi-subtitle { font-size: 0.76rem; color: var(--app-muted); margin-top: 2px; margin-bottom: 0; }
      .bslib-sidebar-layout > .sidebar {
        background-color: var(--policy-surface-subtle) !important;
      }
      .bslib-sidebar-layout > .sidebar .shiny-input-container {
        margin-bottom: 1rem;
      }
      .bslib-sidebar-layout > .sidebar .control-label,
      .bslib-sidebar-layout > .sidebar label {
        font-size: 0.78rem;
        font-weight: 700;
      }

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
      .geographic-affordability-page .geo-chart {
        height: 440px;
        min-height: 440px;
      }
      .affordability-indices-page .bslib-sidebar-layout > .main {
        overflow-y: auto;
        align-content: flex-start;
      }
      .affordability-indices-page .policy-chart-card,
      .affordability-indices-page .shiny-panel-conditional {
        flex: 0 0 auto;
        width: 100%;
      }

      /* KPI change indicator colors */
      .kpi-change-better { color: #0072B2 !important; font-weight: 600; }
      .kpi-change-worse { color: #D55E00 !important; font-weight: 600; }
      .kpi-change-neutral { color: #6C757D !important; font-weight: 600; }
      .kpi-change-up { color: #0072B2 !important; font-weight: 600; }
      .kpi-change-down { color: #D55E00 !important; font-weight: 600; }

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
        .geographic-affordability-page .geo-chart {
          height: 380px;
          min-height: 380px;
        }
        .policy-page-header {
          display: block;
          margin-bottom: 0.85rem;
        }
        .policy-page-actions {
          margin-top: 0.75rem;
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
        document.addEventListener('click', function(e) {
          var navLink = e.target.closest('.navbar-collapse.show .nav-link');
          if (!navLink) return;

          var mainNav = document.getElementById('main_nav');
          var collapseEl = navLink.closest('.navbar-collapse.show');
          if (!collapseEl) return;
          if (mainNav && !navLink.closest('#main_nav')) return;

          var toggle = mainNav ? mainNav.querySelector('.navbar-toggle, .navbar-toggler') : null;
          var toggleVisible = toggle &&
            window.getComputedStyle(toggle).display !== 'none' &&
            window.getComputedStyle(toggle).visibility !== 'hidden';
          if (!toggleVisible && window.innerWidth >= 992) return;

          var forceHideNavbar = function() {
            collapseEl.classList.remove('show');
            collapseEl.classList.remove('collapsing');
            collapseEl.classList.add('collapse');
            if (toggle) {
              toggle.classList.add('collapsed');
              toggle.setAttribute('aria-expanded', 'false');
            }
          };

          if (window.bootstrap && bootstrap.Collapse) {
            bootstrap.Collapse.getOrCreateInstance(collapseEl, { toggle: false }).hide();
          } else if (window.jQuery && typeof $(collapseEl).collapse === 'function') {
            $(collapseEl).collapse('hide');
          } else {
            forceHideNavbar();
          }
          window.setTimeout(forceHideNavbar, 150);
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

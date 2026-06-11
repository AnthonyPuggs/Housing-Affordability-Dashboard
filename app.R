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
source(project_path("R", "data_vintage.R"), local = TRUE)
source(project_path("R", "visual_semantics.R"), local = TRUE)
source(project_path("R", "ui_style_system.R"), local = TRUE)
source(project_path("R", "app_ui_helpers.R"), local = TRUE)
source(project_path("R", "indicator_context.R"), local = TRUE)
source(project_path("R", "plotly_helpers.R"), local = TRUE)
source(project_path("R", "official_burden_summary.R"), local = TRUE)
source(project_path("R", "recent_buyers_helpers.R"), local = TRUE)
source(project_path("R", "source_audit_registry.R"), local = TRUE)
source(project_path("R", "feature_metadata.R"), local = TRUE)
source(project_path("R", "contextual_kpi_helpers.R"), local = TRUE)
source(project_path("R", "rental_market_helpers.R"), local = TRUE)
source(project_path("R", "chart_builders.R"), local = TRUE)
source(project_path("R", "market_entry_scenarios.R"), local = TRUE)
source(project_path("R", "provenance_report.R"), local = TRUE)
source(project_path("R", "release_checklist.R"), local = TRUE)
source(project_path("R", "methodology_module.R"), local = TRUE)
source(project_path("R", "affordability_module.R"), local = TRUE)
source(project_path("R", "recent_buyers_module.R"), local = TRUE)
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
  header = tagList(
    useBusyIndicators(),
    tags$head(
      # Theme CSS lives in www/dashboard.css (SHINY-08); the JS companion
      # www/dashboard.js is loaded after the splash div below.
      tags$link(rel = "stylesheet", href = "dashboard.css")
    ),
    div(
      id = "app-loading-splash",
      role = "status",
      `aria-label` = "Loading dashboard",
      div(
        class = "splash-inner",
        div(class = "splash-spinner"),
        p("Loading the Australian Housing Affordability dashboard…")
      )
    ),
    tags$script(src = "dashboard.js")
  ),
  nav_spacer(),
  nav_item(data_vintage_badge()),
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
  # PAGE 4: RECENT BUYERS (official SIH File 9 evidence)
  # ============================================================================
  recentBuyersPageUI("recent_buyers"),

  # ============================================================================
  # PAGE 5: GEOGRAPHIC AFFORDABILITY
  # ============================================================================
  geographicAffordabilityPageUI("geographic_affordability"),

  # ============================================================================
  # PAGE 6: MARKET CONTEXT (Labour & Demographics)
  # ============================================================================
  marketContextPageUI("market_context"),

  # ============================================================================
  # PAGE 7: HOUSING SUPPLY
  # ============================================================================
  housingSupplyPageUI("housing_supply"),

  # ============================================================================
  # PAGE 8: RENTAL MARKET
  # ============================================================================
  rentalMarketPageUI("rental_market"),

  # ============================================================================
  # PAGE 9: METHODOLOGY
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
  recentBuyersPageServer("recent_buyers", is_dark = is_dark)
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

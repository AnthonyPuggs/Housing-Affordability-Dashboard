library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(scales)
library(plotly)
library(readabs)
library(watcher)

options(shiny.autoreload = TRUE)

safe_read <- function(expr, label, warn = TRUE) {
  tryCatch(
    expr,
    error = function(e) {
      if (isTRUE(warn)) {
        warning(paste0("Failed to load ", label, ": ", conditionMessage(e)))
      }
      tibble()
    }
  )
}

normalize_abs <- function(df, label = NULL, category = "Other", units = NA_character_, freq_hint = NA_character_) {
  if (nrow(df) == 0) {
    return(tibble())
  }

  if (!"date" %in% names(df)) {
    return(tibble())
  }

  if (!"value" %in% names(df)) {
    return(tibble())
  }

  out <- df %>%
    mutate(
      date = as.Date(date),
      value = as.numeric(value),
      series = if ("series" %in% names(df)) as.character(series) else ifelse(is.null(label), "Series", label),
      series_id = if ("series_id" %in% names(df)) as.character(series_id) else NA_character_,
      unit = if ("unit" %in% names(df)) as.character(unit) else units,
      frequency = if ("frequency" %in% names(df)) as.character(frequency) else freq_hint
    ) %>%
    filter(!is.na(date), !is.na(value))

  if (!is.null(label)) {
    out$series <- label
  }

  out %>%
    mutate(category = category) %>%
    select(date, value, series, series_id, category, unit, frequency)
}

select_series <- function(df, pattern, label, category, units = NA_character_) {
  if (nrow(df) == 0 || !"series" %in% names(df)) {
    return(tibble())
  }

  matched <- df %>%
    filter(str_detect(series, regex(pattern, ignore_case = TRUE)))

  if (nrow(matched) == 0) {
    return(tibble())
  }

  normalize_abs(matched, label = label, category = category, units = units)
}

infer_lag <- function(freq_label) {
  case_when(
    str_detect(freq_label %||% "", regex("quarter", ignore_case = TRUE)) ~ 4,
    str_detect(freq_label %||% "", regex("month", ignore_case = TRUE)) ~ 12,
    str_detect(freq_label %||% "", regex("year", ignore_case = TRUE)) ~ 1,
    TRUE ~ 12
  )
}


infer_lag_from_dates <- function(dates, fallback = 12) {
  d <- sort(unique(as.Date(dates)))
  if (length(d) < 3) {
    return(fallback)
  }

  median_gap <- median(as.numeric(diff(d)), na.rm = TRUE)

  if (is.na(median_gap)) {
    return(fallback)
  }

  if (median_gap <= 40) {
    return(12) # monthly
  }
  if (median_gap <= 120) {
    return(4) # quarterly
  }
  1 # annual
}

get_macro_data <- function(custom_ids = character(), nominal_rate_id = "") {
  # National accounts: GDP and consumption (real + nominal)
  na_real <- safe_read(read_abs(cat_no = "5206.0", tables = "1"), "National accounts table 1")
  na_nom <- safe_read(read_abs(cat_no = "5206.0", tables = "2"), "National accounts table 2")

  gdp_real <- select_series(
    na_real,
    "Gross domestic product.*Chain volume",
    "Real GDP (chain volume)",
    "Output"
  )

  gdp_nominal <- select_series(
    na_nom,
    "Gross domestic product.*Current prices",
    "Nominal GDP (current prices)",
    "Output"
  )

  consumption_real <- select_series(
    na_real,
    "Household final consumption expenditure.*Chain volume",
    "Real Household Consumption",
    "Consumption"
  )

  consumption_nominal <- select_series(
    na_nom,
    "Household final consumption expenditure.*Current prices",
    "Nominal Household Consumption",
    "Consumption"
  )

  # CPI helper from readabs
  cpi <- safe_read(read_cpi(), "CPI") %>%
    transmute(date, value = as.numeric(cpi)) %>%
    normalize_abs(label = "CPI (All groups, Australia)", category = "Prices", units = "Index", freq_hint = "Quarter")

  # Labour force indicators
  labour <- safe_read(read_abs(cat_no = "6202.0", tables = "1"), "Labour force")

  employment <- select_series(
    labour,
    "Employed.*Total.*Persons",
    "Employment (Persons)",
    "Labour Market"
  )

  unemployment_rate <- select_series(
    labour,
    "Unemployment rate.*Persons",
    "Unemployment Rate",
    "Labour Market",
    units = "Per cent"
  )

  participation_rate <- select_series(
    labour,
    "Participation rate.*Persons",
    "Participation Rate",
    "Labour Market",
    units = "Per cent"
  )

  # Wage indicator
  wages <- safe_read(read_awe(wage_measure = "awote", sex = "persons", sector = "total"), "Average weekly ordinary time earnings") %>%
    transmute(date, value = as.numeric(value)) %>%
    normalize_abs(label = "AWE (AWOTE, Persons)", category = "Income", units = "AUD", freq_hint = "Quarter")

  # Broad macro indicators table often includes cash rate target
  indicators <- safe_read(
    read_abs(cat_no = "1350.0", tables = "1"),
    "Key macro indicators",
    warn = FALSE
  )

  nominal_rate <- tibble()

  if (nzchar(nominal_rate_id)) {
    nominal_rate <- safe_read(read_abs(series_id = nominal_rate_id), paste0("Nominal rate series ", nominal_rate_id)) %>%
      normalize_abs(label = paste0("Nominal Interest Rate (", nominal_rate_id, ")"), category = "Interest Rates", units = "Per cent")
  }

  if (nrow(nominal_rate) == 0) {
    nominal_rate <- select_series(
      indicators,
      "Cash rate target|Cash rate|Interbank overnight cash rate",
      "Nominal Cash Rate",
      "Interest Rates",
      units = "Per cent"
    )
  }

  # Derived inflation and real policy rate
  cpi_lag <- infer_lag_from_dates(cpi$date, fallback = 4)
  inflation_yoy <- cpi %>%
    arrange(date) %>%
    mutate(value = 100 * (value / lag(value, cpi_lag) - 1)) %>%
    filter(!is.na(value)) %>%
    mutate(series = "CPI Inflation (YoY %)", category = "Prices", unit = "Per cent")

  real_rate <- tibble()
  if (nrow(nominal_rate) > 0 && nrow(inflation_yoy) > 0) {
    inflation_for_join <- inflation_yoy %>%
      select(date, inflation_yoy = value)

    real_rate <- nominal_rate %>%
      arrange(date) %>%
      left_join(inflation_for_join, by = "date") %>%
      tidyr::fill(inflation_yoy, .direction = "down") %>%
      mutate(
        value = value - inflation_yoy,
        series = "Real Cash Rate (Nominal - CPI YoY)",
        category = "Interest Rates",
        unit = "Per cent"
      ) %>%
      filter(!is.na(value)) %>%
      select(date, value, series, series_id, category, unit, frequency)
  }

  custom_series <- map_dfr(custom_ids, function(id) {
    id <- str_trim(id)
    if (!nzchar(id)) {
      return(tibble())
    }

    safe_read(read_abs(series_id = id), paste0("Custom series ", id)) %>%
      normalize_abs(label = paste0("Custom: ", id), category = "Custom")
  })

  bind_rows(
    gdp_nominal,
    gdp_real,
    consumption_nominal,
    consumption_real,
    cpi,
    inflation_yoy,
    employment,
    unemployment_rate,
    participation_rate,
    wages,
    nominal_rate,
    real_rate,
    custom_series
  ) %>%
    distinct(date, series, .keep_all = TRUE) %>%
    arrange(category, series, date)
}

transform_series <- function(df, transform_mode) {
  if (nrow(df) == 0) {
    return(df)
  }

  out <- df %>%
    group_by(series) %>%
    arrange(date, .by_group = TRUE) %>%
    mutate(
      lag_meta = infer_lag(first(frequency)),
      lag_data = infer_lag_from_dates(date, fallback = lag_meta),
      lag_n = coalesce(lag_data, lag_meta)
    )

  if (transform_mode == "levels") {
    out <- out %>% mutate(plot_value = value, plot_label = "Level")
  } else if (transform_mode == "yoy") {
    out <- out %>%
      mutate(plot_value = 100 * (value / lag(value, lag_n) - 1), plot_label = "YoY %")
  } else if (transform_mode == "period") {
    out <- out %>%
      mutate(plot_value = 100 * (value / lag(value, 1) - 1), plot_label = "Period-over-period %")
  } else if (transform_mode == "index") {
    out <- out %>%
      mutate(plot_value = 100 * value / first(value), plot_label = "Index (start = 100)")
  }

  out %>%
    filter(!is.na(plot_value)) %>%
    ungroup()
}

plot_macro <- function(df, chart_type, show_points = FALSE, dark_mode = FALSE) {
  axis_col <- if (isTRUE(dark_mode)) "#D7E0EA" else "#3B3F4A"
  strip_col <- if (isTRUE(dark_mode)) "#E8EEF6" else "#1E2C4A"
  panel_bg <- if (isTRUE(dark_mode)) "#0F172A" else "#FFFFFF"
  grid_col <- if (isTRUE(dark_mode)) "#334155" else "#D9DDE3"

  p <- ggplot(df, aes(x = date, y = plot_value, color = series, fill = series))

  if (chart_type == "line") {
    p <- p + geom_line(linewidth = 1.05, alpha = 0.9)
    if (show_points) {
      p <- p + geom_point(size = 1.6, alpha = 0.9)
    }
  }

  if (chart_type == "area") {
    p <- p + geom_area(alpha = 0.25, position = "identity") + geom_line(linewidth = 0.8)
  }

  if (chart_type == "bar") {
    p <- p + geom_col(position = "dodge", alpha = 0.85)
  }

  if (chart_type == "facet") {
    p <- p +
      geom_line(linewidth = 0.95, show.legend = FALSE) +
      facet_wrap(~series, scales = "free_y", ncol = 2)
  }

  if (chart_type == "seasonal") {
    df <- df %>%
      mutate(
        year = format(date, "%Y"),
        month = factor(format(date, "%b"), levels = month.abb)
      )

    p <- ggplot(df, aes(x = month, y = plot_value, group = year, color = year)) +
      geom_line(linewidth = 0.9, alpha = 0.85) +
      geom_point(size = 1.2, alpha = 0.85) +
      facet_wrap(~series, scales = "free_y")

    return(
      p +
        scale_y_continuous(labels = label_number(big.mark = ",", accuracy = 0.1)) +
        labs(x = NULL, y = NULL, color = NULL) +
        theme_minimal(base_size = 13) +
        theme(
          panel.background = element_rect(fill = panel_bg, color = NA),
          plot.background = element_rect(fill = panel_bg, color = NA),
          panel.grid.major = element_line(color = grid_col),
          panel.grid.minor = element_blank(),
          legend.position = "bottom",
          legend.box = "vertical",
          axis.text.x = element_text(color = axis_col),
          axis.text.y = element_text(color = axis_col),
          axis.title.x = element_text(color = axis_col),
          axis.title.y = element_text(color = axis_col),
          legend.text = element_text(color = axis_col),
          strip.text = element_text(face = "bold", color = strip_col)
        )
    )
  }

  p +
    scale_x_date(date_labels = "%Y", date_breaks = "4 years") +
    scale_y_continuous(labels = label_number(big.mark = ",", accuracy = 0.1)) +
    scale_color_brewer(palette = "Dark2") +
    scale_fill_brewer(palette = "Set2") +
    labs(x = NULL, y = NULL, color = NULL, fill = NULL) +
    theme_minimal(base_size = 13) +
    theme(
      panel.background = element_rect(fill = panel_bg, color = NA),
      plot.background = element_rect(fill = panel_bg, color = NA),
      panel.grid.major = element_line(color = grid_col),
      panel.grid.minor = element_blank(),
      legend.position = "bottom",
      legend.box = "vertical",
      axis.text.x = element_text(color = axis_col),
      axis.text.y = element_text(color = axis_col),
      axis.title.x = element_text(color = axis_col),
      axis.title.y = element_text(color = axis_col),
      legend.text = element_text(color = axis_col),
      strip.text = element_text(face = "bold", color = strip_col)
    )
}

default_rate_series <- function(series_names) {
  preferred <- series_names[str_detect(
    series_names,
    regex("rate|inflation|yoy|growth", ignore_case = TRUE)
  )]

  if (length(preferred) > 0) {
    return(preferred)
  }

  series_names[seq_len(min(8, length(series_names)))]
}

ui <- page_navbar(
  title = "Australia Macro Dashboard",
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
      :root {
        --app-bg: #f4f7fb;
        --app-text: #1f2d3d;
        --app-panel: #ffffff;
        --app-border: #d9e0e8;
      }
      html[data-theme='dark'] {
        --app-bg: #0b1220;
        --app-text: #e3ebf4;
        --app-panel: #111b2e;
        --app-border: #2a3a54;
      }
      body {
        background-color: var(--app-bg);
        color: var(--app-text);
      }
      .bslib-page-fill {
        background-color: var(--app-bg);
      }
      .top-note {
        background: linear-gradient(135deg, #0E5A8A 0%, #1F9D8C 100%);
        color: #ffffff;
        border-radius: 14px;
        padding: 14px 18px;
        margin-bottom: 14px;
      }
      .card,
      .sidebar,
      .form-control,
      .selectize-input,
      .selectize-dropdown,
      .irs--shiny .irs-line,
      .irs--shiny .irs-grid-pol {
        background-color: var(--app-panel);
        color: var(--app-text);
        border-color: var(--app-border);
      }
      .shiny-output-error-validation {
        color: #A43D3D;
        font-weight: 600;
      }
    ")),
    tags$script(HTML("
      (function() {
        function setTheme(theme) {
          document.documentElement.setAttribute('data-theme', theme);
          Shiny.setInputValue('theme_mode', theme, { priority: 'event' });
          var btn = document.getElementById('theme_toggle');
          if (btn) {
            btn.textContent = theme === 'dark' ? 'Light mode' : 'Dark mode';
          }
        }

        function initTheme() {
          var saved = window.localStorage.getItem('macro_theme');
          var preferredDark = window.matchMedia && window.matchMedia('(prefers-color-scheme: dark)').matches;
          var initial = saved || (preferredDark ? 'dark' : 'light');
          setTheme(initial);
        }

        document.addEventListener('DOMContentLoaded', function() {
          initTheme();
          document.addEventListener('click', function(evt) {
            var toggle = evt.target && evt.target.closest ? evt.target.closest('#theme_toggle') : null;
            if (toggle) {
              var current = document.documentElement.getAttribute('data-theme') || 'light';
              var next = current === 'dark' ? 'light' : 'dark';
              window.localStorage.setItem('macro_theme', next);
              setTheme(next);
            }
          });
        });
      })();
    "))
  ),
  nav_spacer(),
  nav_item(actionButton("theme_toggle", "Dark mode", class = "btn-outline-light btn-sm")),
  nav_panel(
    "Dashboard",
    layout_sidebar(
      sidebar = sidebar(
        width = 360,
        tags$div(
          class = "top-note",
          tags$strong("Data source:"),
          tags$span(" ABS via readabs. Use internet access to refresh data.")
        ),
        actionButton("refresh", "Refresh Data", class = "btn-primary"),
        textInput(
          "nominal_rate_id",
          "Nominal Interest Rate Series ID (optional)",
          placeholder = "e.g., ABS series ID"
        ),
        textAreaInput(
          "custom_ids",
          "Custom ABS Series IDs (comma-separated)",
          placeholder = "A84423086A, A2302476C",
          rows = 2
        ),
        checkboxGroupInput("categories", "Categories", choices = NULL),
        selectizeInput("series", "Series", choices = NULL, multiple = TRUE),
        dateRangeInput(
          "date_range",
          "Date Range",
          start = as.Date("1990-01-01"),
          end = Sys.Date(),
          min = as.Date("1970-01-01"),
          separator = "to"
        ),
        radioButtons(
          "transform_mode",
          "Transform",
          choices = c(
            "Levels" = "levels",
            "Year-over-Year %" = "yoy",
            "Period-over-Period %" = "period",
            "Rebased Index" = "index"
          ),
          selected = "levels"
        ),
        radioButtons(
          "chart_type",
          "Chart Type",
          choices = c(
            "Line" = "line",
            "Area" = "area",
            "Bar" = "bar",
            "Small Multiples" = "facet",
            "Seasonality" = "seasonal"
          ),
          selected = "line"
        ),
        checkboxInput("show_points", "Show Points", value = FALSE)
      ),
      card(
        card_header("Australian Macroeconomic Time Series"),
        card_body(
          plotlyOutput("macro_plot", height = "640px")
        )
      )
    )
  ),
  nav_panel(
    "Latest Snapshot",
    layout_column_wrap(
      width = 1/3,
      value_box(title = "Nominal GDP", value = textOutput("vb_gdp_nom"), theme = value_box_theme(bg = "#0E5A8A", fg = "#fff")),
      value_box(title = "Real GDP", value = textOutput("vb_gdp_real"), theme = value_box_theme(bg = "#1F9D8C", fg = "#fff")),
      value_box(title = "CPI YoY", value = textOutput("vb_inflation"), theme = value_box_theme(bg = "#3B4C7A", fg = "#fff")),
      value_box(title = "Unemployment Rate", value = textOutput("vb_ur"), theme = value_box_theme(bg = "#326273", fg = "#fff")),
      value_box(title = "Nominal Cash Rate", value = textOutput("vb_nominal_rate"), theme = value_box_theme(bg = "#17415F", fg = "#fff")),
      value_box(title = "Real Cash Rate", value = textOutput("vb_real_rate"), theme = value_box_theme(bg = "#2A6F97", fg = "#fff"))
    )
  )
)

server <- function(input, output, session) {
  macro_data <- reactiveVal(tibble())
  theme_mode <- reactive({
    mode <- input$theme_mode %||% "light"
    if (identical(mode, "dark")) "dark" else "light"
  })

  load_data <- function() {
    custom_ids <- str_split(input$custom_ids %||% "", pattern = ",")[[1]]
    custom_ids <- custom_ids[nzchar(str_trim(custom_ids))]

    withProgress(message = "Loading ABS macro data", value = 0, {
      incProgress(0.2, detail = "Fetching macro series")
      dat <- get_macro_data(
        custom_ids = custom_ids,
        nominal_rate_id = str_trim(input$nominal_rate_id %||% "")
      )

      incProgress(0.8, detail = "Finalizing")
      macro_data(dat)
    })
  }

  observeEvent(TRUE, {
    load_data()
  }, once = TRUE)

  observeEvent(input$refresh, {
    load_data()
  })

  observeEvent(macro_data(), {
    dat <- macro_data()

    req(nrow(dat) > 0)

    cats <- sort(unique(dat$category))
    updateCheckboxGroupInput(session, "categories", choices = cats, selected = cats)

    srs <- dat %>% distinct(series) %>% arrange(series) %>% pull(series)
    default_selected <- default_rate_series(srs)
    updateSelectizeInput(session, "series", choices = srs, selected = default_selected, server = TRUE)

    updateDateRangeInput(
      session,
      "date_range",
      min = min(dat$date, na.rm = TRUE),
      max = max(dat$date, na.rm = TRUE),
      start = max(min(dat$date, na.rm = TRUE), as.Date("2000-01-01")),
      end = max(dat$date, na.rm = TRUE)
    )
  })

  filtered <- reactive({
    dat <- macro_data()
    req(nrow(dat) > 0)
    req(length(input$categories) > 0)
    req(length(input$series) > 0)

    dat %>%
      filter(category %in% input$categories, series %in% input$series) %>%
      filter(date >= input$date_range[1], date <= input$date_range[2])
  })

  transformed <- reactive({
    transform_series(filtered(), input$transform_mode)
  })

  output$macro_plot <- renderPlotly({
    dat <- transformed()
    validate(need(nrow(dat) > 0, "No data available for your current filters."))

    is_dark <- identical(theme_mode(), "dark")
    p <- plot_macro(
      dat,
      chart_type = input$chart_type,
      show_points = isTRUE(input$show_points),
      dark_mode = is_dark
    )

    ggplotly(p, tooltip = c("x", "y", "color")) %>%
      layout(
        paper_bgcolor = if (is_dark) "#0F172A" else "#FFFFFF",
        plot_bgcolor = if (is_dark) "#0F172A" else "#FFFFFF",
        font = list(color = if (is_dark) "#D7E0EA" else "#3B3F4A"),
        legend = list(orientation = "h", y = -0.2),
        xaxis = list(
          title = "",
          tickformat = "%Y",
          nticks = 7,
          tickangle = 0,
          gridcolor = if (is_dark) "#334155" else "#D9DDE3",
          automargin = TRUE
        ),
        yaxis = list(
          gridcolor = if (is_dark) "#334155" else "#D9DDE3"
        )
      )
  })

  latest_value <- function(series_name, percent = FALSE, prefix = "", suffix = "") {
    dat <- macro_data() %>%
      filter(series == series_name) %>%
      arrange(date)

    if (nrow(dat) == 0) {
      return("N/A")
    }

    val <- tail(dat$value, 1)

    if (isTRUE(percent)) {
      return(paste0(number(val, accuracy = 0.01), "%"))
    }

    paste0(prefix, comma(round(val, 2)), suffix)
  }

  output$vb_gdp_nom <- renderText({ latest_value("Nominal GDP (current prices)", prefix = "$") })
  output$vb_gdp_real <- renderText({ latest_value("Real GDP (chain volume)", prefix = "$") })
  output$vb_inflation <- renderText({ latest_value("CPI Inflation (YoY %)", percent = TRUE) })
  output$vb_ur <- renderText({ latest_value("Unemployment Rate", percent = TRUE) })
  output$vb_nominal_rate <- renderText({ latest_value("Nominal Cash Rate", percent = TRUE) })
  output$vb_real_rate <- renderText({ latest_value("Real Cash Rate (Nominal - CPI YoY)", percent = TRUE) })
}

shinyApp(ui, server)

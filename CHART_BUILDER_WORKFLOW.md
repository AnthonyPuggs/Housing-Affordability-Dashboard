# Chart Builder Workflow

This dashboard now separates chart construction from Shiny page wiring.
The practical result is that most visual changes can be made and tested in
`R/chart_builders.R` without editing the Shiny modules directly.

## Architecture

The chart path is:

1. `app.R` sources setup files, including `R/chart_builders.R`.
2. Page modules prepare data from Shiny inputs and preloaded dashboard objects.
3. A chart builder returns a plain `ggplot` object.
4. The module converts that `ggplot` to Plotly with `dashboard_ggplotly()`.
5. The module applies Shiny-specific caching and any Plotly-only adjustments.

In simplified form:

```r
d <- filtered_data_from_inputs()
p <- build_affordability_indices_plot(d, dark = is_dark())
dashboard_ggplotly(p, dark = is_dark(), tooltip = c("x", "y"))
```

## File Responsibilities

`R/chart_builders.R`

- Owns page-level `ggplot2` construction.
- Contains chart-specific layers, geoms, scales, axis labels and legends.
- Includes small chart-specific transform helpers, such as index and change transforms.
- Should remain free of Shiny inputs, outputs, reactivity and `renderPlotly()`.

`R/*_module.R`

- Owns Shiny UI, inputs, filtering, validation and output IDs.
- Calls chart builders after preparing data.
- Converts builder output to Plotly.
- Owns `bindCache()` keys.
- Keeps Plotly-specific post-processing, such as right-side Overview annotations and trace tweaks.

`R/plotly_helpers.R`

- Owns the shared `dashboard_ggplotly()` conversion wrapper.
- Applies the common Plotly layout wrapper.

`R/dashboard_theme.R`

- Owns `theme_afford()` and `plotly_layout()`.
- Use this file for broad theme changes rather than repeating theme code inside individual builders.

`R/visual_semantics.R`

- Owns semantic colours and palettes.
- Use this file when changing the meaning of colours, for example better, worse, neutral, caution or reference.

`plot_setup.R`

- Loads dashboard CSVs and prepares shared app-ready objects.
- Keeps global object names available to page modules.
- It is not the preferred place for chart styling changes.

## When To Edit Each Surface

Edit `R/chart_builders.R` when changing:

- chart layers, such as `geom_line()`, `geom_col()`, `geom_tile()` or `geom_text()`;
- axis labels, scales and date breaks;
- legend position and labels;
- chart titles produced inside plots;
- error bars or uncertainty interval display;
- whether a chart is faceted, stacked or grouped;
- chart-specific data transformations used only for plotting.

Edit the relevant module when changing:

- controls, filters or input choices;
- output IDs or tab/card layout;
- source notes and explanatory UI text;
- Shiny validation or empty-state handling;
- Plotly tooltips passed to `dashboard_ggplotly()`;
- Plotly `bindCache()` keys;
- Plotly-only post-processing that cannot be represented in `ggplot2`.

Edit `R/visual_semantics.R` when changing:

- KPI colour interpretation;
- stress-band colours;
- cost-pressure palettes;
- caution/reference colours;
- burden heatmap colour gradients.

Edit `R/dashboard_theme.R` when changing:

- common chart background;
- gridlines;
- base text colour and size;
- dark/light chart theme behaviour;
- shared Plotly layout settings.

## Current Builder Coverage

`R/chart_builders.R` covers the page-level chart construction for:

- Overview;
- Price Trends;
- Affordability;
- Geographic Affordability;
- Market Context;
- Housing Supply;
- Rental Market.

The modules still retain a few responsibilities by design:

- Overview keeps right-side city annotations in Plotly because those are Plotly layout annotations.
- Rental Market keeps the NHHA heatmap text/hover trace behaviour around Plotly conversion.
- Affordability keeps market-entry scenario generation and SIH quality joins before calling builders.
- Survey modules keep SIH reliability and uncertainty metadata preparation before plotting.

## Testing Chart Builder Changes

For isolated builder checks, run:

```bash
Rscript tests/test_chart_builders.R
```

For a source-level app check, run:

```bash
Rscript -e "source('plot_setup.R'); source('app.R'); cat('APP_SOURCE_OK\n')"
```

For a browser check, start the app:

```bash
Rscript -e "shiny::runApp('.')"
```

Then inspect the affected page in the browser, including dark/light mode and any relevant controls.

## Quick Local Plot Test

You can test a builder outside Shiny with an ordinary R session:

```r
source("plot_setup.R")
source("R/chart_builders.R")

d <- median_prices_combined |>
  dplyr::filter(city %in% c("Sydney", "Melbourne"))

p <- build_overview_median_prices_plot(
  data = d,
  is_index = FALSE,
  price_colours = city_colours,
  show_cities = unique(d$city),
  dark = FALSE
)

print(p)
```

For dark mode, pass `dark = TRUE`.

## Cautions

- Restart the Shiny app after changing `R/chart_builders.R`; sourced helper changes are read at app launch.
- Do not use Shiny inputs directly inside chart builders.
- Do not call `dashboard_ggplotly()` inside chart builders.
- Keep formulas and analytics separate from purely visual changes. If a change alters an affordability definition or scenario formula, update the relevant methodology tests and documentation as well.
- If a chart uses SIH reliability or uncertainty metadata, keep the join and data preparation in the module unless the same preparation becomes shared across several modules.


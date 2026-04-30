# Australian Housing Affordability Dashboard

Interactive R/Shiny dashboard for Australian housing affordability and cost-pressure analysis. The app combines saved ABS, SIH and RBA-derived CSV outputs with Plotly charts for prices, rents, official housing burden measures, labour-market context, housing supply and stylised market-entry scenarios.

## Run The App

Restore the pinned R package environment from `renv.lock`:

```bash
Rscript -e "renv::restore()"
```

`renv.lock` pins package versions for reproducible dashboard, pipeline and test runs. In plain terms, renv.lock pins package versions so another machine can restore the same R package set. The project activation file `.Rprofile` loads `renv` automatically when R starts from the repository root.

If `renv` is unavailable, install it first:

```r
install.packages("renv")
```

As a manual fallback, install the direct runtime and pipeline packages:

```r
install.packages(c(
  "shiny", "bslib", "ggplot2", "plotly", "dplyr", "tidyr", "purrr",
  "stringr", "scales", "readr", "readxl", "readabs", "lubridate",
  "httr", "rlang", "watcher"
))
```

From the repository root, start the dashboard with:

```bash
Rscript -e "shiny::runApp('.')"
```

The app reads pre-generated CSV files from `data/`, so it can run without refreshing live ABS or RBA inputs.

## Refresh The Data

Run the full data pipeline from the repository root:

```bash
Rscript pipeline/05_driver.R
```

The pipeline parses local ABS Survey of Income and Housing workbooks under `resources/ABS_data/`, retrieves public ABS/RBA time series, derives affordability and cost-pressure indicators, and validates the generated outputs.

## Data Model

Main dashboard CSVs live in `data/`:

- `abs_timeseries.csv`: long-format ABS macro, CPI, price, labour and supply series using `date | value | series | series_id | category | unit | frequency`.
- `rba_rates.csv`: long-format RBA cash and mortgage-rate inputs using the same time-series schema.
- `rba_*_raw.csv`: normalised RBA source-cache artefacts kept rectangular for reproducible parsing; `rba_rates.csv` is the dashboard-ready RBA output.
- `affordability_indices.csv`: derived cost-pressure indicators using `date | value | indicator | geography | unit | frequency`.
- `sih_*.csv`: parsed ABS Survey of Income and Housing tables for official housing cost, burden and NHHA rental-stress measures.

Official SIH/NHHA measures should be interpreted separately from modelled market-entry indicators. Mortgage serviceability, deposit-gap and calculator outputs are stylised scenarios, not official ABS measures or lender assessments.

## Methodology Metadata

`R/indicator_registry.R` is the source of truth for derived indicator formulas, source series, units, interpretation direction and caveats. The registry documents the current formulas used by the pipeline and dashboard; it does not make stylised market-entry measures official ABS measures or lender assessments.

The Shiny app includes a Methodology page backed by this registry. It shows the formula, source series, interpretation direction and official/stylised status for each derived affordability indicator.

## Verification

The project uses lightweight base-R tests. Useful checks from the repository root are:

```bash
Rscript tests/test_pipeline_outputs.R
Rscript tests/test_app_output_ids.R
Rscript tests/test_kpi_change_labels.R
Rscript tests/test_app_method_text.R
Rscript tests/test_methodology_page.R
Rscript tests/test_rba_raw_cache_hygiene.R
Rscript tests/test_public_release_hygiene.R
Rscript tests/test_project_root_paths.R
```

For a quick source check:

```bash
Rscript -e "source('plot_setup.R'); source('app.R'); cat('APP_SOURCE_OK\n')"
```

## Notes

- `pipeline/05_driver.R` is the canonical data-refresh entrypoint.
- `app_old.R`, `_check_cpi.R` and `save_plots.R` are legacy or manual-support scripts and are not part of the production app launch path.
- `HOUSING_DASHBOARD_ROOT` can be set to the repository path when launching scripts from unusual working directories.

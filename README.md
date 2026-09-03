<div align="center">

<img src="assets/banner.svg" alt="Australian Housing Affordability Dashboard" width="100%" />

<br/>

[![Live dashboard](https://img.shields.io/badge/▶_live_dashboard-open-1f6feb?labelColor=0b2a52)](https://anthonypuggs-housing-affordability-dashboard.share.connect.posit.cloud/)
[![Language](https://img.shields.io/badge/R-100%25-6639ba?labelColor=41464d)](https://github.com/AnthonyPuggs/Housing-Affordability-Dashboard/search?l=r)
[![Framework](https://img.shields.io/badge/Shiny-bslib-0b2a52?labelColor=41464d)](https://shiny.posit.co/)
[![Data](https://img.shields.io/badge/data-ABS_·_SIH_·_RBA-0b2a52?labelColor=41464d)](#where-the-data-comes-from)
[![Reproducible](https://img.shields.io/badge/reproducible-renv-0969da?labelColor=41464d)](https://rstudio.github.io/renv/)
[![License](https://img.shields.io/badge/license-MIT-6a737d?labelColor=41464d)](LICENSE)

</div>

An R/Shiny dashboard for Australian housing affordability. It puts the official ABS survey
measures of housing costs next to a set of modelled market-entry indicators, and it keeps the two
visibly apart on every page. That separation is the main design rule of the project: a number is
either something the ABS observed in a household survey, or something this dashboard computed from
public series under stated assumptions, and the labels never let you confuse one for the other.

**[Open the live dashboard](https://anthonypuggs-housing-affordability-dashboard.share.connect.posit.cloud/)** &nbsp;·&nbsp; [Methodology](#methodology-and-provenance) &nbsp;·&nbsp; [Run it locally](#run-it)

---

## Two kinds of number

| | Official (SIH / NHHA) | Modelled (stylised) |
| --- | --- | --- |
| What it is | ABS Survey of Income and Housing estimates of observed household housing costs, cost-to-income ratios and lower-income renter stress. | Serviceability, deposit-gap and calculator outputs for a stylised household under fixed assumptions. |
| How the app treats it | Passed through as official survey burden and stress measures, with relative standard error and 95% margin-of-error metadata shown as reliability markers (`†`). | Useful for scenarios, but not official ABS measures or lender assessments. The app says so wherever they appear. |

The headline National Market-Entry Affordability Score belongs to the second column. It is a
relative index scored against 2012 to 2025 history, so a higher value means market entry is easier
than it has been, not that more households can afford housing.

One thing worth knowing up front: the ABS cancelled the 2023-24 Survey of Income and Housing
because renter households were under-represented in the sample. The latest survey cross-section
is therefore still 2019-20, and it will stay that way until SIH 2025-26 is released, probably in
2027. The market-entry indicators exist partly to give the dashboard something timelier to say in
the meantime.

---

## The pages

| # | Page | What it shows |
| :-: | --- | --- |
| 1 | Overview | The headline score, its component contributions and a snapshot of official SIH burden. |
| 2 | Price Trends | Capital-city dwelling price indexes and ABS rent CPI. |
| 3 | Affordability | Official SIH burden bands, market-entry scenarios and a serviceability calculator. |
| 4 | Recent Buyers | SIH File 9: dwelling values, mortgages, equity and household profiles of recent buyers. |
| 5 | Geographic Affordability | SIH-only cost-to-income comparisons across states and capital cities. |
| 6 | Market Context | Labour spare capacity, mortgage rates and population growth. |
| 7 | Housing Supply | Building approvals by state, type and sector, plus construction cost pressure. |
| 8 | Rental Market | NHHA rental stress, rent price pressure and SIH rental cost estimates. |
| 9 | Methodology | Formulas, source series and caveats for every derived indicator, generated from the registry. |

### The score

The current score is version 2. It combines three component scores at fixed weights:

| Component | Weight | What it captures |
| --- | :-: | --- |
| Mortgage serviceability | 40% | Indexed repayment burden on a 30-year principal-and-interest loan at actual RBA new-loan rates |
| Rental entry | 35% | Rent pressure relative to wages |
| Deposit barrier | 25% | Time needed to save a deposit |

Each component is normalised against a frozen 2012 to 2025 reference window, so new data does not
shift the meaning of past scores. The weights are unchanged from version 1. The rental component
uses public index-style inputs, which means it can understate stress in new leases. On the Overview
page you can click any historical date on the chart and the headline and component display update
to that point, with the weights held fixed.

Official SIH and NHHA stress measures are never folded into the score, because they answer a
different question: they describe the housing burden households actually reported, while the score
describes modelled conditions for entering the market. The dashboard shows both, side by side,
with their labels intact.

---

## Where the data comes from

- ABS: dwelling prices, CPI, labour market and building approval series, plus the Survey of Income and Housing (SIH and NHHA tables).
- RBA: cash rate and mortgage-rate inputs from the F-series tables, and household debt-to-income from table E2.
- Derived: affordability indices and the National Market-Entry Affordability Score, computed by the pipeline in this repository.

A scheduled GitHub Actions job refreshes the ABS and RBA inputs on weekday mornings (Brisbane
time), runs the full test suite against the new data, and commits the updated CSVs only when
something other than the vintage timestamp has changed. Large revisions go through an automatically
merged pull request so the diff is kept for later review.

The app is built with R, Shiny and bslib on Bootstrap 5, with charts in Plotly. It uses local
system font stacks rather than hosted web fonts, so nothing is fetched from a third party at
launch.

---

## Run it

Packages are pinned with [`renv`](https://rstudio.github.io/renv/). The app reads the committed
CSVs in `data/`, so it starts without any network access:

```bash
Rscript -e "renv::restore()"      # restore the pinned packages
Rscript -e "shiny::runApp('.')"   # launch the dashboard
```

<details>
<summary>No <code>renv</code>? Install the runtime packages by hand</summary>

```r
install.packages("renv")   # the easier option

# or install the direct runtime and pipeline packages:
install.packages(c(
  "shiny", "bslib", "ggplot2", "plotly", "dplyr", "tidyr", "purrr",
  "stringr", "scales", "readr", "readxl", "readabs", "lubridate",
  "httr", "rlang", "watcher"
))
```
</details>

### Refresh the data

The pipeline parses the local ABS SIH workbooks, downloads the public ABS and RBA series, derives
the indicators and checks every output against its stage contract. Run it from the repository
root:

```bash
Rscript pipeline/05_driver.R
```

### Verify

The testthat suite covers the pipeline, every page module, the methodology text and the visual
semantics. Before publishing, run it together with the release checklist:

```bash
Rscript -e "testthat::test_dir('tests', stop_on_failure = TRUE)"
Rscript -e "source('R/release_checklist.R'); validate_release_checklist()"
```

---

## Methodology and provenance

Every number on the dashboard can be traced back through the same chain:

```
pipeline/05_driver.R  →  06_validate_outputs.R  →  data/*.csv  →  R/indicator_registry.R  →  dashboard labels
```

`R/indicator_registry.R` is the source of truth for derived indicator formulas, source series, units, interpretation direction and caveats. The in-app Methodology page is generated from it, and so is the downloadable methodology summary. The registry documents what the pipeline currently computes; it does not turn a stylised market-entry measure into an official ABS statistic.

Caveats that apply throughout:

- Average weekly earnings (AWE) is individual earnings, not household disposable income. The wage price index (WPI) measures wage prices, not the income distribution.
- CPI rents and CPI new-dwelling indexes are price indexes, not measures of household burden.
- Assessment buffer, deposit, LVR, loan term and expense inputs are sensitivity assumptions, not a lender assessment.
- KPI colours encode economic interpretation (better, worse or neutral), not raw up or down movement.
- SIH figures are survey estimates. Where the relative standard error is high, interpret with caution.

---

## Technical notes

### Dependencies

renv.lock pins package versions so another machine can restore the same package set for the dashboard, the pipeline and the tests. The `.Rprofile` in the repository root activates renv automatically when R starts there.

### UI and theming

The interface uses bslib-native dark/light mode through Bootstrap 5, with local system font stacks in place of Google-hosted fonts.

Page headers, KPI tiles, chart cards and source notes come from a shared public-policy report UI system in `R/ui_style_system.R`, so every module gets the same hierarchy, spacing and typography without re-implementing it.

KPI colours encode economic interpretation as better, worse or neutral/contextual rather than raw up/down movement. `R/visual_semantics.R` holds those classes and the chart palettes, which is what stops a rise in an affordability-worsening measure from being coloured as good news.

### Pipeline gates

Each pipeline stage has to pass per-stage output gates from `R/pipeline_contracts.R` before the next stage runs. The same file records the fixed external-source manifest: the ABS catalogue and table calls, the ABS SDMX CPI endpoints and the RBA F-series tables the pipeline is allowed to touch.

### Data model

The dashboard reads these files from `data/`:

- `abs_timeseries.csv`: long-format ABS macro, CPI, price, labour and supply series, using `date | value | series | series_id | category | unit | frequency`.
- `rba_rates.csv`: RBA cash rate, mortgage rates and household debt-to-income (E2) in the same time-series schema.
- `rba_*_raw.csv`: normalised RBA download caches, kept rectangular so parsing is reproducible. These are gitignored artefacts of a pipeline run, not committed outputs. `rba_rates.csv` is the dashboard-ready RBA file.
- `affordability_indices.csv`: derived cost-pressure indicators, the National Housing Affordability Score, and timely official market-entry context (ABS 5601.0 first home buyer lending, the monthly CPI rents signal and the RBA E2 debt-to-income ratio), using `date | value | indicator | geography | unit | frequency`.
- `sih_*.csv`: parsed ABS Survey of Income and Housing tables for official housing cost, burden and NHHA rental stress measures.
- `sih_estimate_quality.csv`: sampling-error metadata for selected SIH tables, including 95% margin of error values and relative standard error flags. Estimates with an RSE between 25% and 50% should be interpreted with caution; above 50% the ABS considers them too unreliable for general use.

SIH outputs are checked against workbook benchmark rows in `R/sih_benchmarks.R` (key cells from Files 4, 5, 8 and 13), which keeps the sampling-error sections of the workbooks from leaking into the main estimate CSVs.

Official SIH and NHHA measures are interpreted separately from the modelled indicators. Mortgage serviceability, deposit-gap and calculator outputs are stylised scenarios, not official ABS measures or lender assessments. The National Housing Affordability Score is the underlying v2 composite; the Overview page presents it as the National Market-Entry Affordability Score, which is a labelling layer over the same series rather than a new one.

`R/market_entry_scenarios.R` holds the app-only scenario calculations: mortgage repayments, assessed-rate sensitivity, deposit saving time and expense-adjusted serviceability ratios. The serviceability chart uses AWE individual earnings as its income proxy. Savings-rate assumptions live in the calculator only, since they affect saving time rather than repayment serviceability.

### Modules and helpers

The page modules are `R/overview_module.R`, `R/price_trends_module.R`, `R/affordability_module.R`, `R/recent_buyers_module.R`, `R/geographic_affordability_module.R`, `R/market_context_module.R`, `R/housing_supply_module.R`, `R/rental_market_module.R` and `R/methodology_module.R`. Each one owns its page's inputs, validation, caching, SIH quality joins and Plotly conversion.

`R/chart_builders.R` holds the ggplot builders the modules call, so charts can be tested outside Shiny. `plot_setup.R` is a thin compatibility entrypoint over the CSV loading, formatting, theme and precomputed-series helpers; it keeps the old global object names available to the modules. `CHART_BUILDER_WORKFLOW.md` walks through the practical chart-editing loop.

A few page-specific notes:

- The Geographic Affordability page only shows SIH estimates where the housing-cost numerator and the income or household denominator were measured in the same geography (states, lower-income households by state, and greater capital city versus rest of state). It does not build state or city market-entry indexes from national wage, price or CPI-rent proxies.
- The Housing Supply page filters the ABS approvals series by state, building type and sector. The default view compares total approvals for New South Wales and Victoria.
- Survey charts with matching estimate-quality metadata show a `†` reliability marker and put the relative standard error or margin of error in the hover text. Where 95% margin-of-error metadata exists, error bars are drawn from `data/sih_estimate_quality.csv`.
- The Methodology page offers a Markdown download built by `R/provenance_report.R`. It combines the registry table with an inventory of the saved `data/*.csv` files and does not write anything into the repository.

### Testing

The suite runs on testthat. `tests/helper-contracts.R` is the shared harness. Unit and module tests that boot the app data layer read frozen fixtures from `tests/fixtures/data` (regenerate them deliberately with `Rscript tests/fixtures/generate_fixtures.R`), so a scheduled data refresh cannot break a code test. Run everything from the repository root with:

```bash
Rscript -e "testthat::test_dir('tests', stop_on_failure = TRUE)"
```

For release smoke testing of the live interface, see `docs/ui_smoke_checklist.md`. The project combines static UI smoke contracts, that manual browser checklist, and one automated `shinytest2::AppDriver` test (`tests/test_app_smoke.R`) that boots the app on fixture data and visits every nav panel under headless Chrome. It skips, rather than fails, when no Chromium-based browser is available.

Every test file also runs standalone. Useful checks from the repository root:

```bash
Rscript tests/test_pipeline_outputs.R
Rscript tests/test_app_output_ids.R
Rscript tests/test_kpi_change_labels.R
Rscript tests/test_app_method_text.R
Rscript tests/test_methodology_page.R
Rscript tests/test_methodology_module.R
Rscript tests/test_affordability_module.R
Rscript tests/test_recent_buyers_module.R
Rscript tests/test_rental_market_module.R
Rscript tests/test_housing_supply_module.R
Rscript tests/test_housing_supply_legend_contracts.R
Rscript tests/test_price_trends_module.R
Rscript tests/test_geographic_affordability_module.R
Rscript tests/test_geographic_affordability_data_contracts.R
Rscript tests/test_market_context_module.R
Rscript tests/test_overview_module.R
Rscript tests/test_provenance_report.R
Rscript tests/test_sih_quality_helpers.R
Rscript tests/test_sih_uncertainty_intervals.R
Rscript tests/test_sih_estimate_quality.R
Rscript tests/test_sih_workbook_benchmarks.R
Rscript tests/test_market_entry_scenarios.R
Rscript tests/test_serviceability_scenario_controls.R
Rscript tests/test_visual_semantics.R
Rscript tests/test_spliced_rate.R
Rscript tests/test_chart_builders.R
Rscript tests/test_plot_setup_extraction.R
Rscript tests/test_plotly_helpers.R
Rscript tests/test_app_plotly_cache_contracts.R
Rscript tests/test_ui_style_system.R
Rscript tests/test_ui_smoke_contracts.R
Rscript tests/test_responsive_ui_contracts.R
Rscript tests/test_rental_market_mobile_contracts.R
Rscript tests/test_rental_market_interaction_contracts.R
Rscript tests/test_theme_infrastructure.R
Rscript tests/test_rba_raw_cache_hygiene.R
Rscript tests/test_public_release_hygiene.R
Rscript tests/test_project_root_paths.R
```

Before publishing, run the release-readiness checklist:

```bash
Rscript -e "source('R/release_checklist.R'); validate_release_checklist()"
```

Checklist warnings can be acceptable for known data vintage. Checklist failures block public release until fixed.

For a quick check that the app still sources cleanly:

```bash
Rscript -e "source('plot_setup.R'); source('app.R'); cat('APP_SOURCE_OK\n')"
```

### Notes

`pipeline/05_driver.R` is the only supported data-refresh entrypoint. Older scripts (`app_old.R`, `_check_cpi.R`, `save_plots.R` and the exported `plots/`) live under `archive/` and are not part of the app. If you launch scripts from an unusual working directory, set `HOUSING_DASHBOARD_ROOT` to the repository path.

---

<div align="center">
<sub>

[Live dashboard](https://anthonypuggs-housing-affordability-dashboard.share.connect.posit.cloud/) &nbsp;·&nbsp;
Data: ABS · SIH · RBA &nbsp;·&nbsp; Built in R with Shiny and Plotly &nbsp;·&nbsp; Brisbane, Australia

</sub>
</div>

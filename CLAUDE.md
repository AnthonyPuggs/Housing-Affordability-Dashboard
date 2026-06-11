# CLAUDE.md — Housing Affordability Dashboard

## Project Overview

Production R/Shiny dashboard for Australian housing affordability analysis, deployed to Posit Connect Cloud. It combines official ABS Survey of Income and Housing (SIH/NHHA) burden measures with modelled market-entry indicators (serviceability, deposit gap, National Housing Affordability Score), keeping the two strictly separated in labelling.

**Architecture in one line:** `pipeline/` stages fetch/parse source data into committed `data/*.csv` → `app.R` + `R/` page modules read only those CSVs at runtime (no live API calls in the app).

## Repository Layout

| Path | Role |
|------|------|
| `app.R` | Shiny entrypoint: UI shell (`bslib::page_navbar`), theme CSS/JS, server wiring for 9 page modules |
| `plot_setup.R` | Compatibility entrypoint sourcing data loading, formatting, theme and precomputed-series helpers |
| `R/` | ~36 modules: page modules (`*_module.R`), chart builders, indicator registry, score, helpers, release checklist |
| `pipeline/` | Data pipeline stages `00_config` → `01_process_sih` → `02/02b_fetch_abs` → `03_fetch_rba` → `04_derive_indicators` → `05_driver` (runner) → `06_validate_outputs` → `07_write_data_vintage` |
| `data/` | Committed pipeline outputs the app reads (`abs_timeseries.csv`, `rba_rates.csv`, `affordability_indices.csv`, `sih_*.csv`, `data_vintage.csv`). `data/rba_*_raw.*` are gitignored download caches, not outputs |
| `tests/` | 56 standalone base-R test scripts (`test_*.R`), each with a small `check()` harness; run with `Rscript tests/test_X.R` from the repo root |
| `resources/` | ABS SIH workbooks + methodology PDFs (see below); inputs to stage 01, never read by the app |
| `docs/` | Reviews, roadmap (`docs/roadmap.md`), UI smoke checklist |
| `app_old.R` | Legacy pre-pipeline macro dashboard. Not part of the app; archival candidate (roadmap Track 3) |

## Key Conventions

- **Dependencies via renv**: `renv.lock` pins packages; `.Rprofile` auto-activates. Use `Rscript -e "renv::restore()"`, never ad-hoc `install.packages()` for project dependencies.
- **Indicator registry is the source of truth**: `R/indicator_registry.R` defines formula text, units, interpretation direction, official/stylised class and public caveats for every derived indicator. The Methodology page renders it. Update the registry whenever a derivation changes.
- **Schema contracts**: time series use `date | value | series | series_id | category | unit | frequency`; derived indices use `date | value | indicator | geography | unit | frequency`; SIH outputs use `survey_year | value | metric | tenure | breakdown_var | breakdown_val | geography | stat_type`. Preserve these when adding sources.
- **Official vs stylised separation**: SIH/NHHA estimates are official pass-through cells; serviceability/deposit/score outputs are stylised scenarios and must always be labelled as such (never as official ABS measures or lender assessments).
- **Fail-loud selection**: stage 04 selects required series via `get_series_exact()`; name-regex selections must be followed by `assert_selection_nonempty()`, and stage combine steps use `combine_series_unique()` (errors on cross-source name collisions) — never `distinct(date, series)`. Prefer exact series IDs with loud errors over regex fallbacks when adding pipeline series.
- **Strict pipeline mode**: the driver (and CI) set `PIPELINE_STRICT <- TRUE`, which promotes parser/write-lock warnings to errors via `pipeline_problem()`; the driver also asserts every stage output was rewritten by the current run (`validate_pipeline_stage_freshness()`).
- **App data loading**: `load_dashboard_csvs()` returns typed empty tibbles with a warning for broken/missing CSVs and `assert_dashboard_data()` refuses startup with a message naming every problem file.

## Commands

```bash
# Run the dashboard (reads committed data/*.csv; no network needed)
Rscript -e "shiny::runApp('.')"

# Refresh all data (network: ABS via readabs/SDMX, RBA CSV endpoints)
Rscript pipeline/05_driver.R

# Run one test / the full suite (from repo root)
Rscript tests/test_pipeline_outputs.R
# PowerShell: foreach ($f in Get-ChildItem tests -Filter "test_*.R") { Rscript "tests/$($f.Name)" }

# Release-readiness checklist (data, methodology, hygiene, deployment checks)
Rscript -e "source('R/release_checklist.R'); validate_release_checklist()"
```

## CI / Automation

- `.github/workflows/ci.yml` — push (main) + PR: runs all 56 test scripts plus the release checklist.
- `.github/workflows/data-refresh.yml` — scheduled weekdays 07:00 AEST (`cron: '0 21 * * 0-4'` UTC; Actions schedules are UTC-only — do not add a `timezone:` key). Runs the pipeline, refresh contract tests, then commits `data/*.csv` only when something other than `data_vintage.csv` changed.
- Byte-literal contract tests pin exact source strings (workflow text, module text, README text). When changing pinned code or docs, update the corresponding `tests/test_*.R` contract in the same commit.

## Deployment (Posit Connect Cloud)

Git-backed deployment driven by `manifest.json` + `.rscignore`. Rules:

- After adding/removing files that `app.R` sources, or after `renv::snapshot()`, regenerate the manifest: `Rscript -e "rsconnect::writeManifest(appDir = '.')"` and commit it.
- `.rscignore` keeps tests/docs/resources/legacy scripts and the gitignored RBA caches out of the bundle.
- The release checklist asserts every sourced file appears in the manifest and no `.rscignore` path leaks in — CI fails if the manifest goes stale.

## Domain Expertise

When working on this project, apply expertise in:

- **Property economics** — housing tenure (owners, mortgagees, renters), dwelling types, housing supply/demand dynamics in Australia
- **Microeconometrics (housing)** — household survey analysis (ABS SIH), income distribution by tenure, housing cost ratios by quintile, equivalised disposable income
- **Macroeconometrics** — price indices (CPI housing components, mean dwelling prices), time series alignment, interest rate transmission to mortgage costs
- **Affordability indicators** — housing cost-to-income ratios (30/40 rule), deposit-to-income ratios, mortgage serviceability, rental stress thresholds
- **Australian institutional context** — ABS catalogue numbering (4130.0 Housing Occupancy and Costs, 6432.0 dwelling prices, 6401.0 CPI, 5601.0 lending), SIH methodology, NHHA definitions. Note: ABS cancelled SIH 2023-24 (renter under-representation), so SIH 2019-20 stays the latest cross-section until SIH 2025-26 (~2027).

## Resources Directory

### `resources/ABS_data/housing_occupancy_and_costs_SIH/`

14 Excel workbooks from ABS Cat. 4130.0 (Housing Occupancy and Costs, 2019-20), based on the Survey of Income and Housing (SIH). Stage `01_process_sih.R` parses these into `data/sih_*.csv`:

| File | Contents |
|------|----------|
| 1. Housing occupancy and costs, 1994-95 to 2019-20 | Long-run time series of key housing indicators |
| 2. Housing occupancy | Tenure type, landlord type, dwelling structure |
| 3. Housing costs | Weekly housing costs by tenure and income |
| 4. Housing costs as a proportion of income | Cost-to-income ratios by tenure type |
| 5. Housing costs as a proportion of income ranges | Distribution across cost burden ranges |
| 6. Age of household reference person | Housing indicators by age group |
| 7. Housing utilisation | Bedrooms needed vs available, overcrowding |
| 8. Lower income households, state and territory | Affordability for bottom 40% by state |
| 9. Recent home buyer households | Purchase price, deposit, loan characteristics |
| 10. Residential property ownership | Property *other than the own home* (excluding selected dwelling) — cannot source owner-occupied dwelling values |
| 11. Greater capital city statistical areas | Capital city vs rest-of-state breakdowns |
| 12. Housing occupancy costs, states and territories | State-level housing cost comparisons |
| 13. Rental affordability, lower income renters (NHHA) | NHHA-basis rental stress indicators |
| Table A3 | Comparison of rental affordability measures across editions |

### `resources/` — CSV and PDFs

- **Housing Occupancy and Costs, 2019-20.csv** — Flat extract of housing occupancy and cost data
- **housing_afford_stats_guide_ABS.pdf** — ABS statistical guide to housing affordability concepts and definitions
- **housing_afford_indic_methods_NZ.pdf** — NZ methodology for housing affordability indicators (useful comparative framework)
- **household-living-costs-price-indexes-backgrd_NZ.pdf** — NZ background on household living cost price indexes

**Rule:** Always consult these PDFs and the relevant workbooks before implementing or changing affordability calculations — they define the exact indicator methodologies. Cross-reference workbook definitions (especially files 4, 5, 8, 13) with the ABS statistical guide.

## Analytical Methodology

Key affordability concepts (per ABS and NZ reference documents):

- **Housing cost burden** — housing costs as % of gross/disposable household income
- **30/40 rule** — households in the bottom 40% of equivalised income distribution paying >30% of income on housing are in "housing stress"
- **Rental affordability (NHHA)** — lower-income renter households paying >30% of gross income on rent (excludes Rent Assistance from income)
- **Deposit gap** — years to save a 20% deposit on the national mean dwelling price at an assumed savings rate (stylised; assumptions disclosed in the registry)
- **Mortgage serviceability** — repayments as % of income at prevailing interest rates (AWE individual-earnings proxy; not household income)
- **Equivalised disposable income** — ABS-applied (modified OECD scale); the dashboard passes SIH cells through and computes no local equivalisation

## Rules

- Always use **Context7 MCP** for library/API documentation and code generation without being explicitly asked
- Consult `resources/` PDFs and `resources/ABS_data/` workbooks before implementing any affordability metric — do not rely on general knowledge alone
- Preserve the schema contracts above when adding new data sources
- Australian data context: ABS catalogue numbers, Australian dollars, Australian fiscal year (July–June) where relevant
- After changing module/workflow/README text, run the matching contract tests — many assert byte-literal strings

## graphify

This project has a graphify knowledge graph at graphify-out/.

Rules:
- Before answering architecture or codebase questions, read graphify-out/GRAPH_REPORT.md for god nodes and community structure
- If graphify-out/wiki/index.md exists, navigate it instead of reading raw files
- For cross-module "how does X relate to Y" questions, prefer `graphify query "<question>"`, `graphify path "<A>" "<B>"`, or `graphify explain "<concept>"` over grep — these traverse the graph's EXTRACTED + INFERRED edges instead of scanning files
- After modifying code files in this session, run `graphify update .` to keep the graph current (AST-only, no API cost)

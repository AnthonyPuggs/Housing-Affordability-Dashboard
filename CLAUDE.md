# CLAUDE.md — Household Affordability Dashboard

## Project Overview

Australian household affordability analysis project. Currently early-stage: contains a working macro dashboard (`app_old.R`, ~680 lines) that pulls live ABS data, plus local ABS housing microdata and methodology reference PDFs. The goal is to build a comprehensive housing affordability dashboard combining macro indicators with household-level cost burden analysis.

**Current state:** Exploration and data assembly. No production app yet — `app_old.R` is the inherited macro dashboard to extend or replace.

## Domain Expertise

When working on this project, apply expertise in:

- **Property economics** — housing tenure (owners, mortgagees, renters), dwelling types, housing supply/demand dynamics in Australia
- **Microeconometrics (housing)** — household survey analysis (ABS SIH), income distribution by tenure, housing cost ratios by quintile, equivalised disposable income
- **Macroeconometrics** — price indices (CPI housing components, RPPI), time series decomposition, interest rate transmission to mortgage costs
- **Affordability indicators** — housing cost-to-income ratios (30/40 rule), deposit-to-income ratios, mortgage serviceability, rental stress thresholds
- **Australian institutional context** — ABS catalogue numbering (e.g. 4130.0 Housing Occupancy and Costs), SIH methodology, NHHA (National Housing and Homelessness Agreement) definitions

## Resources Directory

### `resources/ABS_data/housing_occupancy_and_costs_SIH/`

14 Excel workbooks from ABS Cat. 4130.0 (Housing Occupancy and Costs, 2019-20), based on the Survey of Income and Housing (SIH):

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
| 10. Residential property ownership | Property values, equity, multiple ownership |
| 11. Greater capital city statistical areas | Capital city vs rest-of-state breakdowns |
| 12. Housing occupancy costs, states and territories | State-level housing cost comparisons |
| 13. Rental affordability, lower income renters (NHHA) | NHHA-basis rental stress indicators |
| Table A3 | Comparison of rental affordability measures across editions |

### `resources/` — CSV and PDFs

- **Housing Occupancy and Costs, 2019-20.csv** — Flat extract of housing occupancy and cost data
- **housing_afford_stats_guide_ABS.pdf** — ABS statistical guide to housing affordability concepts and definitions
- **housing_afford_indic_methods_NZ.pdf** — NZ methodology for housing affordability indicators (useful comparative framework)
- **household-living-costs-price-indexes-backgrd_NZ.pdf** — NZ background on household living cost price indexes

**Rule:** Always consult these PDFs before implementing affordability calculations — they define the exact indicator methodologies.

## Technical Stack

| Layer | Technology |
|-------|-----------|
| Dashboard | R / Shiny with `bslib` (Bootstrap 5) |
| Visualization | `ggplot2` + `plotly` |
| ABS data access | `readabs` (live API to ABS.Stat) |
| Data wrangling | `dplyr`, `tidyr`, `purrr`, `stringr` |
| Formatting | `scales` |
| Future additions | Python, SQL, Excel as needed |

### Commands

```bash
# Run the existing macro dashboard
Rscript -e "shiny::runApp('app_old.R')"

# Install R dependencies
Rscript -e "install.packages(c('shiny', 'bslib', 'ggplot2', 'plotly', 'readabs', 'dplyr', 'tidyr', 'purrr', 'stringr', 'scales', 'watcher'))"
```

## Architecture of `app_old.R`

Single-file Shiny app (UI + server). Key components:

### Data Layer (lines 1-253)
- `safe_read()` — error-tolerant wrapper for ABS data fetching
- `normalize_abs()` — standardises any ABS tibble to schema: `date | value | series | series_id | category | unit | frequency`
- `select_series()` — regex-matches series names from ABS tables and normalises
- `infer_lag()` / `infer_lag_from_dates()` — detects data frequency for YoY calculations
- `get_macro_data()` — master fetch function pulling GDP (5206.0), CPI, labour force (6202.0), wages (AWE), cash rate (1350.0), plus custom series IDs; derives CPI inflation YoY and real cash rate

### Transform & Plot (lines 255-384)
- `transform_series()` — applies levels/YoY/period/index transforms with frequency-aware lagging
- `plot_macro()` — builds ggplot with chart type dispatch (line/area/bar/facet/seasonal) and dark mode support

### UI (lines 386-552)
- `bslib::page_navbar` with two tabs: Dashboard (sidebar + plotly chart) and Latest Snapshot (value boxes)
- Dark/light theme toggle with localStorage persistence
- Sidebar: refresh button, custom series ID inputs, category/series filters, date range, transform and chart type selectors

### Server (lines 554-678)
- Reactive data flow: `load_data()` → `macro_data()` reactiveVal → `filtered()` → `transformed()` → `renderPlotly`
- Value boxes show latest GDP, CPI, unemployment, cash rate

### ABS Integration Pattern
The `readabs` package is the sole data source. Pattern: `read_abs(cat_no, tables)` → pipe through `select_series()` → `normalize_abs()` → `bind_rows()` into unified long-format tibble. This pattern should be extended for housing-specific ABS catalogues.

## Analytical Methodology

Key affordability concepts (per ABS and NZ reference documents):

- **Housing cost burden** — housing costs as % of gross/disposable household income
- **30/40 rule** — households in the bottom 40% of income distribution paying >30% of income on housing are in "housing stress"
- **Rental affordability (NHHA)** — lower-income renter households paying >30% of gross income on rent
- **Deposit gap** — median dwelling price vs median household income (years to save a deposit)
- **Mortgage serviceability** — repayments as % of income at prevailing interest rates
- **Equivalised disposable income** — income adjusted for household size/composition using modified OECD scale

When building indicators, cross-reference workbook definitions (especially files 4, 5, 8, 13) with the ABS statistical guide PDF.

## Rules

- Always use **Context7 MCP** for library/API documentation and code generation without being explicitly asked
- Consult `resources/` PDFs before implementing any affordability metric — do not rely on general knowledge alone
- Consult `resources/ABS_data/` workbooks for variable definitions, category breakdowns, and historical benchmarks
- Preserve the `normalize_abs()` schema pattern (`date | value | series | series_id | category | unit | frequency`) when adding new data sources
- Australian data context: use ABS catalogue numbers, Australian dollar amounts, Australian fiscal year conventions (July-June) where relevant

# HousingAffordabilityDashboard news

Versioning: `DESCRIPTION` carries the version under development; each Posit
Connect deploy gets a matching git tag `vX.Y.Z` (see `docs/deploying.md`).

## 0.4.0 (in development — Track 3 hygiene)

- Legacy scripts (`app_old.R`, `save_plots.R`, `_check_cpi.R`, `README_old.md`,
  exported `plots/`) archived under `archive/`.
- Overview score chart no longer mutates plotly private session state for
  click events (SHINY-06).
- Houses Share KPI subtitle labels the measure as national (SHINY-07).
- ABS SDMX endpoints pinned to explicit dataflow versions with response
  dataflow/column assertions (PIPE-09).
- Dwelling price state indexes re-based to a fixed base quarter (2011-09);
  the national 6432.0 mean price series renamed from the misleading "RPPI"
  to "Mean Dwelling Price ; Australia ;", and Table 2 medians renamed from
  "RPPI Established Houses/Attached Dwellings" to "Median Price ..." (PIPE-11).
- `align_quarterly()`/`quarterly_mean()` only use complete quarters of
  monthly inputs, so partial latest quarters no longer wobble derived
  indicators (STAT-07).
- Methodology page states the RSE > 50% flag-not-suppress policy and that no
  sampling-error propagation is performed on derived indices (STAT-06).
- Deploy discipline: versioned releases (`DESCRIPTION` + git tags), this
  NEWS file and `docs/deploying.md` (TEST-08).

## 0.3.0 (2026-06-12) — roadmap Track 2

- Serviceability v2 / National Affordability Score v2: annuity P&I input,
  RBA F6 actual new-loan rates spliced onto level-adjusted F5 history,
  frozen percentile reference window.
- Fail-loud data layer across app startup and pipeline selection/combine
  steps (caught a state-as-national-price defect).
- Recent Buyers page (SIH File 9) as ninth nav panel.
- Honest geography: state means labelled as states, eight-capital-city rent
  labels, common-window indexing.
- testthat migration with frozen fixtures, AppDriver smoke test and stage-04
  unit tests.
- Timely market-entry data: FHB lending (5601.0), monthly CPI indicator
  rents, RBA E2 household debt-to-income.

## 0.2.0 (2026-06-11) — roadmap Track 1

- Quick wins: manifest regeneration with release-checklist enforcement,
  RBA cache unfreeze + freshness gate, push/PR CI, scheduled refresh
  no-op-commit fix, Deposit Gap anchor fix, YoY label tolerance, dark-mode
  contrast batch, busy indicators and loading splash, CLAUDE.md rewrite,
  SIH 2023-24 cancellation disclosure.

## 0.1.0 — baseline

- Initial public dashboard: pipeline (SIH/ABS/RBA) → committed `data/*.csv`
  → Shiny app with eight pages, indicator registry and release checklist.

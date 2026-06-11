# Roadmap — Housing Affordability Dashboard

**Date:** 2026-06-10 · Derived from `docs/code_review_2026-06-10.md` (finding IDs referenced below), the open items of `docs/feature_review_2026-05-31.md`, and a domain gap analysis. Each item carries: value rationale, effort (S ≤ half-day, M ≤ ~3 days, L = week+), dependencies, and acceptance criteria. Items were checked against the feature review's **Do Not Overbuild** list; rejected candidates are listed at the end so they aren't re-litigated.

**Prioritization:** Value = decision-usefulness × credibility impact × audience reach. Four tracks: **Now** (do this week — high value, small effort, or outage-preventing), **Next** (strategic, M/L), **Hygiene** (debt that compounds), **Later/Watch**.

**Strategic context that shapes everything below:** ABS cancelled the SIH 2023-24 release, so 2019-20 SIH remains the latest official cross-section until **SIH 2025-26 lands (~2027)**. That means: (a) the dashboard's comparative advantage near-term is *credibility and timely market-entry indicators*, not newer survey data; (b) the pipeline should be hardened **now** so the 2025-26 release can be ingested quickly when it arrives (PIPE-12).

---

## Track 1 — NOW (quick wins, ~1–2 days total, all S effort)

| # | Item | Why first | Source | Acceptance criteria |
|---|---|---|---|---|
| 1 | **Regenerate `manifest.json`** (`rsconnect::writeManifest()`) and add a release-checklist assertion that every file `app.R` sources appears in it and no `.rscignore` path leaks in | The next Connect redeploy from git is a likely **outage** (12 missing files) and currently publishes internal docs + 15 MB dead weight | TEST-02 / SHINY-02 | Fresh manifest committed; checklist fails if a sourced file is absent |
| 2 | **Unfreeze RBA data**: gitignore `rba_*_raw.csv` (they're caches, not outputs) or key cache validity on content date; add a freshness gate (e.g. rba_rates max date ≤ 45 days) to `06_validate_outputs.R` | Rates have been silently frozen >1 month while labelled "refreshed today" — this is live wrong-staleness on the deployed app | PIPE-01, PIPE-06 | Next CI run commits new RBA observations; freshness gate fails if it ever recurs |
| 3 | **Add push/PR CI** (`ci.yml`): renv restore + run all 56 test scripts (bash loop interim; testthat later) | 89% of the test suite currently never runs; every code change lands unvalidated | TEST-01 | Red X on a PR that breaks any test |
| 4 | **Stop the daily no-op commits**: exclude `data_vintage.csv` timestamps from the change gate; fix the cron (`timezone:` is not a GitHub Actions key → run at `0 21 * * 0-4` UTC = 07:00 Brisbane Mon–Fri); update the workflow test | Restores a meaningful commit log and the intended morning refresh slot | PIPE-02, PIPE-07 | No commit on a no-change day; run appears at 07:00 AEST |
| 5 | **Fix the Deposit Gap anchor**: drop `SIH_BASE_PRICE <- 575000` (mis-cited) — use the 6432.0 mean dwelling price level directly and relabel; disclose the 15% savings-rate and AWE-proxy assumptions in the registry caveat | Strongest credibility issue in the modelled layer; fix already exists in the codebase pattern (`precomputed_series.R` uses the price level) | ECON-01 | No hard-coded price constant; methodology page states assumptions; level change explained in commit message |
| 6 | **Fix the "YoY" fallback label**: require the t−12 observation within tolerance (`add_with_rollback`), blank label otherwise; mirror in `overview_score_change` | A KPI can currently display a 1-month change captioned YoY — wrong-number class | STAT-02 | Unit test: missing t−12 → empty label |
| 7 | **Dark-mode/contrast batch**: national rent CPI #000000 → theme-aware (UX-01); better/worse KPI colours per-theme (UX-03); validation-error red dark override (UX-09); Rental Market sidebar `open="desktop"` (FR-P2-5 leftover) | Four one-line-ish CSS/colour edits; UX-01 makes the headline national series literally invisible in dark mode today | UX-01/03/09 | Contrast ≥ 3:1 (graphics) / 4.5:1 (text) in both themes |
| 8 | **Add `bslib::useBusyIndicators()` + static loading splash** | Cold start is currently a blank tab; one-line + small CSS | UX-05 | Spinner/splash visible during load |
| 9 | **Rewrite `CLAUDE.md`** to describe the real architecture (pipeline → data/*.csv → app.R; renv; test runner; CI behavior; app_old.R = legacy) | Currently misdirects every contributor and AI tool that reads it | META-01 | Doc matches repo reality |
| 10 | **Disclose the SIH 2023-24 non-release** (one methodology sentence) + label NAS score as-at date | Pre-empts the "stale dashboard" misread; free credibility | STAT-12, STAT-05 | Sentence on methodology page citing ABS decision |

## Track 2 — NEXT (strategic, M/L effort, ordered)

### 2.1 Serviceability v2 (the highest-value methodology upgrade) — M
Replace the interest-only MSI score input with the **existing** annuity-based P&I series (`precomputed_series.R:110-127`); switch the rate source from RBA F5 advertised-discounted to **F6 actual new-loan owner-occupier rates (already fetched, unused)**; bump NAS to v2 with a methodology version note; freeze (or explicitly disclose as revisable) the score's percentile reference distribution; render the "judgement weights" sentence in-app.
*Sources: ECON-02, ECON-04, ECON-06, ECON-07, STAT-04. Dependencies: none — all inputs already in the repo. Acceptance: score component matches the doc's own P&I definition; published score history stops mutating silently (or the chart says it revises).*
This also progresses FR-P1-6 (income denominator) — evaluate ABS household disposable income per household (national accounts) as the denominator while in here.

### 2.2 Fail-loud data layer — M
App side: typed empty tibbles + warning-with-filename in `data_loader.R`, startup assertion naming any missing/column-broken CSV, `validate()` moved above filters in renders (SHINY-01, SHINY-03). Pipeline side: run manifest (per-stage output sha256 asserted newer than pipeline start), promote parser/write warnings to errors under CI, range/dup/freshness gates for the unvalidated files, series-ID-based selection replacing name regexes, explicit `series_type` filter before dedup (PIPE-03/04/05/06).
*Acceptance: deleting any data CSV produces a named, friendly failure at startup and in CI; renaming an ABS series fails the pipeline loudly instead of shipping a silently different number.*

### 2.3 Recent Buyers page (the one genuinely new feature ready to ship) — M
The data (`data/sih_recent_buyers_2020.csv`, 982 rows) and helpers (`R/recent_buyers_helpers.R`) already exist; only the page module and app wiring are missing (FR-P1-3, open since May). First-home-buyer entry conditions are the dashboard's core narrative and this is official SIH data, not modelled.
*Acceptance: ninth nav page with deposit/price/loan characteristics of recent buyers, quality markers wired, module test added.*

### 2.4 Honest geography — M
Fix the state-mean-as-capital-city relabelling (STAT-01): either fetch genuine GCCSA price series or relabel as states, and rename "national" CPI rents to eight-capital-city (ECON-05); footnote the mean-vs-median mix on the Overview price chart and index over the common window (STAT-09). Add the "Bottom 40%" definition notes (ECON-03).
*Acceptance: every geographic label matches what the series measures; cross-tile populations defined at point of use.*

### 2.5 Test architecture migration — M/L
testthat conversion (mechanical: `check()` → `expect_true()`, ~2-3 days), frozen fixtures under `tests/fixtures/` for unit/module tests vs live-data contract tests kept in the refresh job, one `shinytest2::AppDriver` smoke test over the 8 panels + `testServer()` tests for the calculator and score reactives, unit tests for `pipeline/04_derive_indicators.R` (currently zero coverage) with hand-computed expected values.
*Sources: TEST-03/04/05/10. Dependency: Track 1 #3 (CI exists to run them). Acceptance: `testthat::test_dir("tests")` green locally and in CI; a data refresh can no longer fail code tests.*

### 2.6 Timelier market-entry data (post-survey-drought strategy) — M/L
With no new SIH until ~2027, add timely *flow* indicators that strengthen the existing modelled layer (all free ABS/RBA sources, all fit the current pipeline pattern):
- **ABS Lending Indicators 5601.0** — FHB loan counts and average loan sizes: directly evidences the deposit/serviceability components with *actual borrower* data (strong complement to ECON-01/02 fixes).
- **Monthly CPI indicator rents** — monthly rather than quarterly rent signal.
- **RBA E2 household finances** — debt-to-income context for the methodology page.
*Acceptance: new indicators enter via the registry with class/caveat metadata, validation gates, and vintage rows — same standards as existing series.*

## Track 3 — HYGIENE (compounding debt, schedule opportunistically)

| Item | Source | Effort |
|---|---|---|
| Archive `app_old.R`, `README_old.md`, `_check_cpi.R`, `project_plan.md`, `save_plots.R`+`plots/` (confirm export workflow unused) and update the hygiene test that currently pins them | SHINY-09 | S |
| Move the 680-line CSS blob + JS to `www/` (browser-cacheable, lintable); replace CSS-pseudo tooltips with `bslib::tooltip()` | SHINY-08, UX-12 | S/M |
| Deduplicate module scaffolding: `require_helper()`, `sih_quality_hover_frame()`, `policy_kpi_row()` (~290 lines) | SHINY-05 | M |
| Remove the plotly private-state mutation (`plotlyShinyEventIDs`) before a plotly upgrade breaks it | SHINY-06 | S |
| Refresh-workflow hardening: rebase-before-push, queue instead of cancel-in-progress, `if: failure()` issue creation, diff-threshold → auto-PR for large revisions, pin actions to SHAs | TEST-06/07, PIPE-08 | S/M |
| **SIH 2025-26 readiness**: header-anchored Excel parsing (replace positional `skip=`/column maps), ≥1 benchmark cell per output file, loud parse failures — do this *before* the release lands, not during | PIPE-12 | M |
| Pin SDMX dataflow versions + assert returned dimension labels; fixed base quarter for the price index (and stop calling it "RPPI") | PIPE-09, PIPE-11 | S |
| Hover formatting consistency (formatted text aes on line charts); conditional date-axis config; shared modebar config | UX-06/07 | M |
| Chart accessibility: shared colour-blind-safe city palette across pages, role="img" + aria-label wrapper, official-vs-stylised visual marker on KPI boxes, mobile-safe KPI grids, heatmap text-colour port | UX-02/04/08/10/11 | M |
| Deploy discipline: document the deploy procedure, bump DESCRIPTION version + git tag per deploy, NEWS.md | TEST-08 | S |
| Replace byte-literal contract tests with structural assertions (YAML parse; httptest2 mocks) | TEST-09 | M |
| Houses Share KPI: precompute + honour filters or retitle | SHINY-07 | S |
| RSE >50% suppression path + "no error propagation on derived indices" methodology sentence; complete-quarter rule in `align_quarterly` | STAT-06/07 | S |

## Track 4 — LATER / WATCH

- **SA3/SA4 regional affordability** (Census 2026 TableBuilder + ABS regional statistics): high value, **L** effort, and Census 2026 outputs won't flow until 2027 — design the geographic module schema for it when doing 2.4, build later.
- **Persona-based calculators / shareable state**: extend `market_entry_scenarios.R` presets into named personas (single FHB, couple+children renter, etc.) with URL-encoded state and CSV/PNG export. Genuine value but audience-dependent — do after 2.3 proves the FHB narrative draws usage.
- **Advertised/new-tenancy rents**: CoreLogic/PropTrack are license-gated; ABS is developing new-dwelling rent indicators from bond data — watch and ingest when public. (Open source-audit item; would materially improve the RAI's known stock-rents caveat.)
- **SIH 2025-26 ingestion** (~2027): the payoff for Track 3's parser hardening; plan a vintage-comparison view (2019-20 vs 2025-26) when the release date is announced.

## Rejected / out of scope (per the existing Do-Not-Overbuild list — unchanged)

Forecasting models; credit-constraint/borrowing-capacity structural models; CGE/macro-structural modelling; real-wage/real-rate headline features; migration/unemployment as causal explanations; price-to-rent/investor-yield as core affordability metrics; full lender-assessment claims; a single pass/fail score. The review found nothing that justifies reopening these.

## Reconciliation with the 2026-05-31 recommended order

That backlog is substantially **delivered** (burden strip, KPI label, methodology layering, metadata classes, NHHA/CPI pairing — see the ledger in `docs/code_review_2026-06-10.md` §2). What carries forward: FR-P1-3 → Track 2.3; FR-P1-4 (renter preset rename) → fold into 2.3/2.1; FR-P1-6 → 2.1; FR-P2-5 leftover → Track 1 #7; FR-P2-6 (participation KPI swap) → fold into 2.6 (replace with an income-security KPI when RBA E2/HDI lands); UIR-P2-1 (heatmap grid warnings) → Track 3 hover/chart batch. This roadmap supersedes the 2026-05-31 implementation order; the Do-Not-Overbuild list remains in force.

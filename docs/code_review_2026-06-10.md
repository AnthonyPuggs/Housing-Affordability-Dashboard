# Code Review — Housing Affordability Dashboard

**Date:** 2026-06-10 (session 2026-06-11 AEST)
**Scope:** `app.R`, `R/` (36 modules), `pipeline/` (stages 00–07), `tests/` (56 scripts), `.github/workflows/`, deployment artifacts. `app_old.R` treated as legacy (archival candidate), not deeply reviewed.
**Method:** Six parallel expert review lenses — R/Shiny engineering (SHINY), data pipeline (PIPE), economics/methodology (ECON), statistics (STAT), UX/accessibility (UX, delta-only vs prior internal reviews), testing/CI/release (TEST) — run against the codebase, the ABS/NZ methodology PDFs in `resources/`, and the SIH 4130.0 workbooks. All P1 file:line citations were independently re-verified before publication. Prior internal reviews (`docs/feature_review_2026-05-31.md`, `docs/ui_review_2026-05-21.md`, `docs/national_affordability_score_review.md`) were read first; this review reports deltas and new ground only.

---

## 1. Executive summary

**Overall verdict: a genuinely well-engineered dashboard whose weakest surfaces are its failure paths and its deployment/CI plumbing, not its analytics.**

| Lens | Verdict | P1 | P2 | P3 |
|---|---|---|---|---|
| Economics/methodology (ECON) | Official SIH/NHHA layer reproduces ABS published cells **exactly**; modelled layer well-caveated but two components misrepresent what they measure | 1 | 3 | 3 |
| Statistics (STAT) | Above-average hygiene (RSE conventions correct, CPI rebase clean, vintage labelling good); city-label and normalisation issues | 2 | 4 | 5 |
| Pipeline (PIPE) | Loud-failure spine is good (`required=TRUE`, `get_series_exact`); but staleness-blind caching and silent regex selection undermine it | 5 | 7 | 1 |
| Shiny engineering (SHINY) | Strong: full bindCache coverage with correct keys, idiomatic modules; silent-empty data loader is the main debt | 1 | 5 | 2 |
| Testing/CI (TEST) | Large, disciplined test corpus that is structurally inverted: 89% never runs automatically; deployment manifest dangerously stale | 2 | 3 | 5 |
| UX/accessibility (UX) | Structure good and prior backlog mostly delivered; consistent accessibility gap (contrast, colour-blindness, screen readers) | 3 | 5 | 4 |

**Headline positives (verified, not assumed):**
- NHHA rental stress, stress bands, cost-ratio and lower-income state figures reproduce ABS workbook cells **exactly** (e.g. NHHA 2019-20 Australia: 42.0% / 618,846 / 1,473,599 — all exact matches to workbook 13 Table 13.1).
- The classic 30/40-rule error (mixing equivalised selection with gross ratios) **does not occur** — all official figures are ABS pass-through cells.
- All 25 `renderPlotly` outputs across 7 modules have `bindCache` with correct keys (traced individually; no stale-render risk found).
- ABS confirmed **SIH 2023-24 will not be released** (renter under-representation), so the dashboard's "2019-20 is latest" framing remains factually correct. Next cycle: SIH 2025-26 (in field).
- "Not an official statistic" / "historical-relative" guardrails for the National Affordability Score are consistently enforced in UI strings.

**Top 5 issues by urgency:**
1. **TEST-02** Posit Connect `manifest.json` is 5 weeks stale and missing 12 files `app.R` sources — the next git-backed redeploy is a likely outage. It also bundles ~15 MB of internal files (tests/, resources/, CLAUDE.md, app_old.R) that `.rscignore` is supposed to exclude.
2. **PIPE-01** RBA rates data has been silently frozen for >1 month: committed `rba_*_raw.csv` caches + an mtime-based 24h check (checkout resets mtimes → cache always looks fresh) mean CI never re-downloads, while `data_vintage.csv` stamps the files "refreshed today."
3. **TEST-01** No push/PR CI exists — 50 of 56 test scripts never run automatically; regressions reach main unvalidated.
4. **ECON-01** The Deposit Gap's $575,000 anchor is hard-coded, "approx", and **mis-cited** (SIH File 10 covers property *other than* the own home), splicing a median-level anchor onto mean-price growth, undisclosed.
5. **STAT-01** State-level mean dwelling prices are relabelled as capital-city series ("Sydney" actually shows all-of-NSW dynamics), disclosed only in a code comment.

---

## 2. Prior-findings ledger (status of internal review backlogs)

| ID | Item | Status | Evidence |
|---|---|---|---|
| FR-P1-1 | Official SIH/NHHA burden snapshot on Overview | DONE | `R/overview_module.R:428-459`, UI 206-214 |
| FR-P1-2 | "National Mean Dwelling Price" KPI label fix | DONE (label only) | `R/overview_module.R:221`; component semantics → ECON-01 |
| FR-P1-3 | Recent Buyers / FHB feature from SIH File 9 | PARTIAL | helpers + data exist (`R/recent_buyers_helpers.R`, `data/sih_recent_buyers_2020.csv`); no page module wired into `app.R` |
| FR-P1-4 | Rename/convert "Renter entry" calculator preset | PARTIAL | still mortgage/deposit-based |
| FR-P1-6 | Income denominator (household disposable income) | OPEN | still AWE/WPI proxy; see ECON-02/06, STAT-03 |
| FR-P1-7 | Pair CPI rent proxy with official NHHA stress | DONE | rental market module |
| FR-P1-9 | Feature-level metadata classes | DONE | `R/feature_metadata.R` |
| FR-P2-3 | Mobile rental/geographic charts | Source-fixed, runtime unverified | builder + CSS present; no browser QA available this session |
| FR-P2-4 | Methodology page density layering | DONE | `navset_tab` Public Summary / Technical / Source Audit (`R/methodology_module.R:61-188`) |
| FR-P2-5 | Mobile sidebar defaults | PARTIAL | all `open="desktop"` **except Rental Market** (`open="open"`, `R/rental_market_module.R:66`) — the exact page flagged for mobile density |
| FR-P2-6 | Replace participation-rate KPI | OPEN | unchanged |
| UIR-P1-1..4 | Methodology vectorisation, tablet breakpoint, mobile state chart, plotly click | DONE (all four) | spot-verified, no regressions |
| UIR-P2-1 | Heatmap ggplotly matrix warnings | OPEN | no `tidyr::complete` grid regularisation; `hoverinfo="skip"` masks, not fixes |
| UIR-P2-2 | hovermode wiring | DONE | `R/dashboard_theme.R:48`, `R/plotly_helpers.R:3-6` |
| NASR-1..4 | NAS guardrails | DONE | but see ECON-04/07 (normalisation drift, weight disclosure in-app) |

**Meta-finding (META-01, P2):** `CLAUDE.md` is badly stale — it claims "no production app yet", documents `app_old.R` as the dashboard, recommends `install.packages()` despite renv, and says `readabs` live API is the sole data source (the app is CSV-only at runtime). It misdirects every tool and contributor that reads it. Rewrite needed (see roadmap quick-win).

---

## 3. Findings by lens

Severity rubric: **P0** user-visible wrong numbers / methodology error / data corruption · **P1** broken behavior, fragile contract, major debt · **P2** quality/perf/maintainability · **P3** polish. Effort: S/M/L. No P0s were found.

### 3.1 Economics / methodology (ECON)

| ID | Sev | Finding | Location | Fix (effort) |
|---|---|---|---|---|
| ECON-01 | P1 | Deposit Gap anchor `SIH_BASE_PRICE <- 575000` is hard-coded, "(approx)", and **mis-cited**: SIH File 10 is "Residential property ownership *(excluding selected dwelling)*" — it cannot source an owner-occupied dwelling value; File 10 isn't even parsed by the pipeline. The series splices a median-level anchor onto **mean**-price growth (6432.0), with neither the dollar value nor the median/mean mix disclosed in registry or methodology. Also flagged independently by PIPE-10 and STAT-03 (which adds: AWOTE per *employee* used as *household* income, `SAVINGS_RATE <- 0.15` assumed). | `pipeline/04_derive_indicators.R:229-257` | Use the 6432.0 mean price level directly and relabel honestly, or source/benchmark a real anchor; disclose savings-rate and income-proxy assumptions at point of use (S) |
| ECON-02 | P2 | Mortgage Serviceability Index is **interest-only** (`price_idx × rate / wage_idx`) but the score component is captioned "Monthly repayment burden", and the methodology doc's own spec says P&I. Interest-only overstates swings at low rates (3%→6% doubles MSI; raises true P&I ~40%). A **correct annuity-based series already exists** (`R/precomputed_series.R:110-127`). | `pipeline/04_derive_indicators.R:191-201`; `R/overview_module.R:48` | Feed the P&I series into the score (NAS v2 version bump) or recaption as "interest-cost pressure" (M) |
| ECON-03 | P2 | UI flattens two different ABS "lower income" populations into one "Bottom 40%" label: File 5/8 use the 3rd–40th percentile band of equivalised disposable income; NHHA uses bottom 40% excluding Rent Assistance. Cross-tile comparisons aren't like-for-like; equivalised-selection basis never stated. Values themselves are correct ABS pass-throughs. | `R/affordability_module.R:170`; `R/official_burden_summary.R:140-168` | One-line definition note per surface; rename radio; restore "Equivalised" to quintile labels (S) |
| ECON-04 | P2 | NAS percentile normalisation re-ranks and re-winsorises the **full growing sample** every refresh — previously published scores mutate without a version change, contradicting the methodology doc's own fixed-window rule (line 160). Also look-ahead: a quarter's score depends on later data (STAT-04). | `R/national_affordability_score.R:83-105,159-204` | Freeze a versioned reference distribution, or disclose revision behavior at the chart (M) |
| ECON-05 | P3 | "National CPI rents" is actually the weighted average of eight **capital cities** (rest-of-state excluded; SIH shows materially lower stress there). | `R/indicator_registry.R:12,96` | Say "eight-capital-city CPI rents" (S) |
| ECON-06 | P3 | Serviceability rate source is RBA F5 *advertised discounted* variable rate, which has sat well above rates actually paid since mid-2010s; F6 (actual new-loan rates) is already fetched but unused. No caveat anywhere. | `R/indicator_registry.R:15`; `R/precomputed_series.R:102-108` | Switch to F6 new owner-occupier rate (methodology version note) or caveat (S) |
| ECON-07 | P3 | The doc's "judgement weights, not causal estimates" disclosure for the 40/35/25 weights is never rendered in-app. | `R/methodology_module.R:90` | Add the sentence to the in-app score card (S) |

**Per-indicator methodology verdict table**

| Indicator | Official definition source | Implementation | Verdict |
|---|---|---|---|
| NHHA rental stress | Wb 13 T13.1 + Table A3 | `pipeline/01_process_sih.R:1119-1197` | **CORRECT** (exact reproduction) |
| Stress bands >30% (gross) | Wb 5 T5.1/5.2 footnotes; ABS guide | `01_process_sih.R:684-748` | **CORRECT** (values); label → ECON-03 |
| Cost-to-income ratios | Wb 4 T4.1/4.2 (gross income) | `01_process_sih.R:662-679` | **CORRECT**; gross basis labeled |
| Lower-income by state | Wb 8 T8.1 (3rd–40th pct EDHI) | `01_process_sih.R:837-917` | **CORRECT** (values); definition undisclosed → ECON-03 |
| Mortgage Serviceability Index | modelled; doc spec = P&I/income | `04_derive_indicators.R:191-201` | DEVIATION-DISCLOSED (formula honest; caption misleading → ECON-02) |
| Serviceability KPI/calculator | NAR HAI-style P&I | `precomputed_series.R:110-127`; `market_entry_scenarios.R:22-40` (annuity math **correct**) | DEVIATION-DISCLOSED (AWE proxy open FR-P1-6; F5 caveat missing → ECON-06) |
| Deposit Gap (years) | CHAI deposit indicator | `04_derive_indicators.R:229-257` | **DEVIATION-UNDISCLOSED** → ECON-01 |
| Rental Affordability Index | CHAI rental (new-tenancy rents) | `04_derive_indicators.R:212-222` | DEVIATION-DISCLOSED (CPI stock rents + WPI proxy caveated; "national" mislabel → ECON-05) |
| National Affordability Score | own construct | `R/national_affordability_score.R` | DEVIATION-DISCLOSED (guardrails enforced; ECON-04/07 residual) |
| Equivalisation | ABS-applied (pass-through) | no local OECD scale computed or claimed | **CORRECT** |

### 3.2 Statistics (STAT)

| ID | Sev | Finding | Location | Fix (effort) |
|---|---|---|---|---|
| STAT-01 | P1 | Whole-of-state mean dwelling prices ("Mean price of residential dwellings", 6432.0 T1, state level) are relabelled as capital-city "Dwelling Price Index ; Sydney" etc. Sydney-labelled growth is diluted by regional NSW; users can't detect this — disclosure exists only as a code comment. | `pipeline/02_fetch_abs_timeseries.R:23-70`; consumed by `R/precomputed_series.R:53-58` | Relabel as states, fetch genuine GCCSA series, or add point-of-use proxy disclosure (M) |
| STAT-02 | P1 | KPI YoY helper: if the exact t−12 observation is missing it silently falls back to the **immediately preceding observation** but still labels the figure "YoY" — a 1-month change can be published as year-on-year. Found independently by SHINY-04, which adds: `overview_score_change` handles the same miss differently (inconsistent), and both constructions break on a Feb-29 latest date. | `R/contextual_kpi_helpers.R:196-216`; `R/overview_module.R:74-90` | Require t−12 within tolerance (`lubridate::add_with_rollback`); blank label otherwise (S) |
| STAT-05 | P2 | Deposit Gap & NAS are semiannual (AWE is biannual) but carry frequency metadata "Quarter"; NAS complete-case merge truncates the score at 2025-10 while components run to 2026-01 — the headline silently excludes the latest two quarters. | `data/affordability_indices.csv`; `R/national_affordability_score.R:140-143,213` | Correct frequency metadata; label score as-at date or carry AWE forward (disclosed) (S) |
| STAT-06 | P2 | `align_quarterly()` averages **partial** current quarters (no `n()==3` guard, unlike stage 02); rate data to 2026-05-05 means 2026-Q2 points are 1–2-month provisional averages, undisclosed. | `pipeline/04_derive_indicators.R:95-111,185-189` | Complete-quarter rule or "provisional" flag on last point (S) |
| STAT-09 | P2 | Overview price chart mixes city **median established-house** prices with the national **mean all-dwellings** series on one $ axis; index mode rebases each series at its own first in-window date while the subtitle claims a common base. | `R/precomputed_series.R:74-84`; `R/chart_builders.R:410-424` | Index over common-coverage window (pattern exists in `rent_cpi_city_common_range`); footnote the mean/median mix (S) |
| STAT-07 | P3 | RSE thresholds match ABS convention (25/50), but >50% estimates would *display* with the same dagger as 25–50% caution ones (no suppression path; latent — current data has none). No disclosure that derived indices carry no error propagation. | `R/sih_quality_helpers.R:15-19`; `pipeline/01_process_sih.R:251-270` | Distinct marker/suppression; one methodology sentence (S) |
| STAT-08 | P3 | Comment calls monthly→quarterly averaging "ABS-recommended seasonal adjustment" — it's temporal aggregation, not SA. (CPI rebase to 2024-25=100 verified clean — no splice breaks; the CPI conversion-factors workbook in resources/ is unused.) | `pipeline/02_fetch_abs_timeseries.R:120-121` | Fix comment; delete or wire the workbook (S) |
| STAT-10 | P3 | Two differently-shaped "serviceability" series coexist (linear-in-rate MSI vs correct annuity) — see ECON-02. | `04_derive_indicators.R:191-201` | Compute MSI from annuity factor (S) |
| STAT-11 | P3 | Index base dates are data-availability artefacts (RAI 1997-Q3, PTI/MSI 2011-Q3) that shift if upstream history changes; NAS is rank-based so insensitive (verified — no cross-base arithmetic errors anywhere). | `04_derive_indicators.R:114-118` | Pin an explicit common base quarter; state on axes (S) |
| STAT-12 | P3 | "2019-20 is latest" is **correct** (ABS cancelled SIH 2023-24 — renter under-representation; SIH 2025-26 in field) but the dashboard never says why, inviting the "stale dashboard" misread. | `R/methodology_module.R` | One methodology sentence citing the ABS non-release decision (S) |

### 3.3 Data pipeline (PIPE)

| ID | Sev | Finding | Location | Fix (effort) |
|---|---|---|---|---|
| PIPE-01 | P1 | **RBA data permanently stale in CI:** `rba_*_raw.csv` caches are committed, and the 24h cache check uses file mtime — `actions/checkout` resets mtimes, so the download branch never runs. Empirically: rba_rates.csv last truly updated 2026-05-07; bot commits touch only ABS files; latest RBA obs >1 month old despite daily runs — while `data_vintage.csv` stamps RBA files "refreshed today". Derived indicators (MSI, real mortgage rate) have stopped advancing. | `pipeline/00_config.R:276-277` | Gitignore the raw caches (they're caches, not outputs), or key validity on content date, or force-refresh env flag in CI (S) |
| PIPE-02 | P1 | Guaranteed diff noise: `data_vintage.csv` embeds run timestamps, so the bot commits **every** weekday even when nothing changed (verified: commit 6343d79 = pure timestamp churn). Real revisions are buried; the pipeline is not byte-idempotent by construction. | `R/data_vintage.R:100-101`; workflow:49 | Exclude data_vintage.csv from the change gate, or derive freshness from git metadata (S) |
| PIPE-03 | P1 | Stage gates validate **checked-out files**, not this run's outputs (all 19 outputs are committed, so gates pass on a fresh clone even if a stage wrote nothing); SIH parser errors and CSV write-lock errors are downgraded to warnings. A failed stage + green gate + auto-commit = stale/mixed-vintage indicators shipped silently. | `R/pipeline_contracts.R:159-216`; `pipeline/01_process_sih.R:576-582` | Run manifest (sha256/mtime per stage, asserted newer than pipeline start); promote warnings to errors under driver/CI (M) |
| PIPE-04 | P1 | Silent regex/name-based series selection: 02b filters by name regex with no required-series assertion (renamed ABS series → 0 rows, file still written, gate passes); 03 falls back to **the entire F5/F6 table** when the mortgage-rate filter misses; WPI fallback regex `"index|total"` can match a wrong series. Only stage 4's `get_series_exact()` (verified loud) protects 7 series. | `pipeline/02b_fetch_abs_supply.R:84-128`; `pipeline/03_fetch_rba.R:240,268,296` | Select by series IDs; assert each expected sub-series non-empty; delete broadening fallbacks (M) |
| PIPE-05 | P1 | `distinct(date, series)` arbitrarily drops series_type variants — Trend/SA/Original share identical names and the keeper is whichever `bind_rows` ordered first. The dashboard plots an adjustment type chosen by accident; a readabs reorder would shift values with a plausible-looking diff. | `pipeline/02b_fetch_abs_supply.R:133-135`; same pattern 02:493-495, 03:307-309 | Filter `series_type` explicitly; dedup on (date, series_id); fail on residual duplicates (S) |
| PIPE-06 | P2 | No range/sanity/freshness checks anywhere (negative prices, percent ∈ [0,100], date bounds, max-staleness — a 7-day freshness gate would have caught PIPE-01); `abs_supply_demand.csv` never read by validation; 4 SIH files get structure-only checks. See coverage table below. | `pipeline/06_validate_outputs.R:56-246` | Per-file range + dup-key + freshness assertions (M) |
| PIPE-07 | P2 | `timezone: Australia/Brisbane` is **not a GitHub Actions key** (GitLab concept) — schedules are UTC-only, so the job runs at 17:00 AEST, not the intended 07:00, and Friday-UTC runs land Saturday Brisbane. A test asserts the invalid key must be present, cementing it. | `.github/workflows/data-refresh.yml:6-7`; `tests/test_data_refresh_workflow.R:24` | `cron: '0 21 * * 0-4'` (Brisbane has no DST); delete the key; update the test (S) |
| PIPE-08 | P2 | No guard against large silent revisions: bot pushes straight to the default branch, no diff cap, no quarantine, no failure alerting. (Positive: `required=TRUE` fetches mean an ABS outage fails the job before commit — garbage is not committed on outage.) | workflow:45-55 | Diff-threshold → auto-PR instead of direct push; `if: failure()` issue step (M) |
| PIPE-09 | P2 | Version-fragile endpoints: unversioned SDMX dataflows with positional dimension keys (`CPI/1.115522.10.50.Q`); a dataflow restructure could 200-return a *different series*. Mitigated for CPI by stage-4 checks; unmitigated for the four LF series. | `pipeline/02_fetch_abs_timeseries.R:135,247,386-414` | Pin dataflow versions; assert returned dimension labels post-parse (S) |
| PIPE-10 | P2 | Deposit Gap hard-coded base price — merged into **ECON-01**. | `04_derive_indicators.R:229-247` | — |
| PIPE-11 | P2 | "RPPI" proxy rebases to the **first available** observation per city — if ABS shortens the published window, the whole series silently rescales (compounds PIPE-08). The label also misrepresents a mean-price series as the discontinued RPPI. | `pipeline/02_fetch_abs_timeseries.R:58-63` | Fixed base quarter, loud error if absent (S) |
| PIPE-12 | P2 | SIH Excel parsing uses hardcoded `skip=` rows, sheet names, and **positional** column semantics ("Cols G–I = renter subtypes"); failure is a warning + empty tibble. The next SIH release (2025-26) will shift layouts; positional parsing could mislabel tenure columns and still produce valid-looking rows. Benchmarks cover only 10 cells across 4 of 11 outputs. | `pipeline/01_process_sih.R:52,321,394,…` | Anchor on header text; assert expected headers before assigning names; ≥1 benchmark cell per output (M) |
| PIPE-13 | P3 | Actions pinned to mutable tags (with `contents: write`) — merged with **TEST-07**. | workflow:22,25,29 | Pin to SHAs (S) |

**Validation-coverage summary** (full table in lens output): `affordability_indices`, the six core SIH cost/stress files, and `sih_estimate_quality` have good structural validation (columns, dup keys, finite values); `abs_timeseries`/`rba_rates` get column+required-series checks only; `abs_supply_demand`, `sih_timeseries_national`, `sih_state_timeseries`, `sih_recent_buyers_2020`, `sih_geographic_2020`, `data_vintage` get stage-gate-or-less. **No file** has range, monotonicity, or hard freshness checks.

### 3.4 R/Shiny engineering (SHINY)

| ID | Sev | Finding | Location | Fix (effort) |
|---|---|---|---|---|
| SHINY-01 | P1 | Missing/empty CSV → `data.frame()` silently; `precompute_dashboard_series()` then throws "object 'series' not found" **during startup sourcing** (cryptic Connect error page) for core files, while SIH-file renders fail with raw errors because `filter()`/joins run *before* `validate(need(...))` (e.g. rental module filters at :133, validates at :155). Load failure is surfaced nowhere — no log line names the missing file. | `R/data_loader.R:3-10`; `plot_setup.R:67-77`; `R/rental_market_module.R:133-155`; `R/affordability_module.R:271-275` | Warn-with-filename + typed empty tibbles per `pipeline_contracts.R` schemas; startup assertion; move `validate()` above filters (M) |
| SHINY-02 | P1 | Deployment manifest ships internal files (tests/, 13 MB resources/ incl. all SIH workbooks + PDFs, plots/, app_old.R, CLAUDE.md, AGENTS.md) that `.rscignore` excludes — manifest predates .rscignore. Merged with **TEST-02** (also missing 12 required files). | `manifest.json`; `.rscignore:1-10` | Regenerate with current rsconnect; CI check manifest ⊅ .rscignore patterns (S) |
| SHINY-03 | P2 | `calc_sensitivity` render lacks the input guard its seven sibling outputs have — clearing "Dwelling Price ($)" gives a friendly message everywhere except the adjacent sensitivity chart (raw/sanitized error). | `R/affordability_module.R:631-648` vs :581-598 | Shared validated reactive (reuse calc_vals tryCatch) (S) |
| SHINY-04 | P2 | YoY fallback label — merged into **STAT-02**. | `R/contextual_kpi_helpers.R:196-214` | — |
| SHINY-05 | P2 | ~290 lines of mechanical duplication: 14 copies of the 12-line defensive-`source()` guard (already drifted — methodology's variant silently skips), 5 near-identical SIH quality-hover mutate blocks, 2 identical KPI-row loops. | guards: `R/affordability_module.R:3-53` et al.; hovers: 5 modules | `require_helper()` in project_paths.R; `sih_quality_hover_frame()`; `policy_kpi_row()` (M) |
| SHINY-06 | P2 | Direct mutation of plotly's **private** session state (`session$userData$plotlyShinyEventIDs`) to pre-register a click event — will break silently on a plotly upgrade. | `R/overview_module.R:304-307` | Remove; tryCatch/req-gate `event_data()` instead; document (S) |
| SHINY-07 | P2 | Houses Share KPI ignores the page's own state/sector filters (scope mismatch with the adjacent KPI) and rescans the full table six times per render; an "Australia" series added upstream would silently corrupt the denominator. | `R/housing_supply_module.R:212-269` | Precompute keyed share series; honour filters or retitle (S-M) |
| SHINY-08 | P3 | 680-line CSS blob + JS inline in app.R (76% of the file), re-sent uncached on every load. | `app.R:96-816` | Move to `www/` assets (S) |
| SHINY-09 | P3 | Dead files confirmed unreferenced: `app_old.R`, `README_old.md`, `_check_cpi.R`, `project_plan.md` (0 bytes), `save_plots.R`+`plots/` (standalone export utility) — but `tests/test_public_release_hygiene.R:55-85` *pins them in place* (requires _check_cpi.R tracked; forbids gitignoring app_old.R). | repo root | Archive to docs/archive/ or delete; update the hygiene test in the same change; regenerate manifest (S) |

**Verified clean:** namespacing idiomatic throughout (NS/moduleServer everywhere); `is_dark` passed as reactive argument, no global reach; zero `options()` side effects; observe-vs-reactive use correct; all 25 renderPlotly outputs bindCache'd with complete keys (regression-tested). Startup loads all data once per process (sound architecture); cost not measurable here (no R runtime in sandbox).

### 3.5 UX / accessibility (UX) — delta-only

| ID | Sev | Finding | Location | Fix (effort) |
|---|---|---|---|---|
| UX-01 | P1 | National rent CPI series is **pure black (#000000)** — on the dark panel (#111B2E) contrast is 1.22:1: effectively invisible in dark mode, and it's in the default Price Trends selection. | `R/dashboard_formatting.R:142-152` | Theme-aware colour via `semantic_colours()` (S) |
| UX-02 | P1 | City palette pairs red (#e41a1c Sydney) and green (#4daf4a Brisbane) — the classic deuteranopia confusion — across 8-9-line hue-only charts; Overview uses a *different* palette with two near-identical purples/oranges. The Okabe-Ito semantic palette exists (`visual_semantics.R`) but city palettes bypass it. | `R/dashboard_formatting.R:142-152`; `R/overview_module.R:609-613` | One shared Okabe-Ito/Tol city palette + direct end-labels (M) |
| UX-03 | P1 | KPI better/worse text fails WCAG AA: #D55E00 on white = 3.87:1, #0072B2 on dark card = 3.32:1, at 0.76rem/600 (not "large text"). The most economically loaded signal on the page is its lowest-contrast text. | `app.R:666-670,446` | Theme-specific variants via CSS vars (S) |
| UX-04 | P2 | All 24 plotly charts have no text alternative — no role="img"/aria-label/figcaption; screen readers get nothing. Only the info icon and vintage badge carry aria-labels app-wide. | all module UIs | Wrapper helper adding role+label from card title; later a "view as table" toggle (M) |
| UX-05 | P2 | No startup loading state: browser gets zero bytes until ~30 sources + 8.5 MB CSVs load — indistinguishable from broken on cold start. No `bslib::useBusyIndicators()`. | `app.R:40-65` | useBusyIndicators() + static splash removed on connect (S) |
| UX-06 | P2 | Hover formats inconsistent: `disable_hovertemplate=TRUE` strips templates, so non-SIH charts show raw floats ("plot_value: 912345.6") while SIH charts have rich formatted hover. | `R/dashboard_theme.R:59-63` | Formatted text aes + tooltip="text" in line builders (M) |
| UX-07 | P2 | Shared layout force-applies date-axis tickformat/dtick to xaxis1..9 of **every** chart incl. categorical bars and heatmaps (works only because plotly currently ignores invalid combos); modebar config inconsistent (only the score trend trims it). | `R/dashboard_theme.R:36-55` | Conditional date-axis application; centralised modebar config (M) |
| UX-08 | P2 | New official-burden strip + KPI rows are fixed 4-column grids with no mobile rule (4 boxes in ~360px); official vs stylised distinction is carried by wording alone — same box style, same accent rotation. | `R/overview_module.R:205-214,443-458`; `app.R:673-752` | `layout_column_wrap(width="220px")`; "Official SIH/NHHA" pill reusing measure_class (M) |
| UX-09 | P3 | Validation-error red #A43D3D = 2.72:1 on dark panel — empty-state messages near-illegible in dark mode. | `app.R:621` | Dark override (S) |
| UX-10 | P3 | Cost-burden heatmap: always-black tile text + white grout in dark mode; sibling rental heatmap solved this (`tile_text_colour`) but its labels are ~6.7pt. | `R/chart_builders.R:649-662,271-273` | Port adaptive text colour; bump label size (S) |
| UX-11 | P3 | Same cities, different colours per page (Sydney blue on Overview, red on Price Trends). | `overview_module.R:609-613` vs `dashboard_formatting.R:142-152` | Merge with UX-02 (S) |
| UX-12 | P3 | CSS-pseudo-element tooltips not dismissible/touch-reachable (WCAG 1.4.13); ~680 inline CSS lines with heavy `!important` fight bslib theming. | `app.R:361-431` | `bslib::tooltip()`; migrate CSS to www/ + `bs_add_rules()` (M) |

Note: static-analysis-only — no R runtime in this sandbox; app last confirmed booting in the 2026-05-31 review. Plus one-line fix: Rental Market sidebar still `open="open"` (FR-P2-5).

### 3.6 Testing / CI / release (TEST)

| ID | Sev | Finding | Location | Fix (effort) |
|---|---|---|---|---|
| TEST-01 | P1 | **No push/PR CI** — the only workflow triggers are schedule + manual dispatch; 50 of 56 test scripts (incl. all 8 module tests and `test_pipeline_outputs.R`, the README's own verify command) never run automatically. Regressions reach main unvalidated and surface, at best, in the next morning's data refresh. | `.github/workflows/data-refresh.yml:3-7` | `ci.yml` on push/PR: renv restore + run full suite (interim: bash loop over tests/*.R) (S) |
| TEST-02 | P1 | `manifest.json` committed 2026-05-03; app code changed through 2026-06-07. The manifest's 145 files **omit 12 files app.R sources at startup** (data_vintage.R, national_affordability_score.R, official_burden_summary.R, recent_buyers_helpers.R, data/data_vintage.csv, run_app.R, …) — a git-backed Connect redeploy ships a bundle that crashes on boot, and pins packages to the May 3 snapshot. Nothing validates or regenerates it; daily data commits silently invalidate its checksums. (Merged: SHINY-02 — also ships ~15 MB internal files.) | `manifest.json` | `rsconnect::writeManifest()` now; CI check: every sourced file ∈ manifest, manifest ≥ renv.lock date, no .rscignore-pattern paths (S) |
| TEST-03 | P2 | 56 ad-hoc scripts with an identical hand-rolled 8-line `check()` harness duplicated per file; no runner, no skips, no parallelism; README documents 1 of 56. Migration is mechanical: `check(cond,msg)` → `expect_true()`; files already match `test_*.R` naming. | `tests/*.R` | testthat + `test_dir("tests")` in CI (M, ~2-3 days) |
| TEST-04 | P2 | 18 of 56 scripts assert against **live** `data/*.csv` (no `tests/fixtures/` exists) — a scheduled refresh can break code tests with no code change and vice versa; failures non-attributable. | e.g. `tests/test_pipeline_outputs.R:13-50` | Frozen fixtures for unit/module tests; keep contract tests on live data in the refresh job only (M) |
| TEST-05 | P2 | "UI tests" are regex greps over source text (exact-string contracts incl. embedded JS); a module can throw on startup while every contract passes. No shinytest2/testServer anywhere despite prior reviews repeatedly noting browser QA unavailable. | `tests/test_ui_smoke_contracts.R:90-331` | One `shinytest2::AppDriver` smoke (8 panels, no error outputs) + 2-3 `testServer()` tests for calculator/score (M) |
| TEST-06 | P3 | Auto-commit race (no `pull --rebase` before push → whole pipeline run wasted on a concurrent human push); `cancel-in-progress: true` can kill a mid-pipeline scheduled run on manual dispatch; zero failure alerting (and GitHub auto-disables schedules after 60 days of repo inactivity). | workflow:12-14,45-55 | rebase-before-push; queue instead of cancel; `if: failure()` issue step (S) |
| TEST-07 | P3 | Actions on mutable tags (`checkout@v6`, `r-lib/actions@v2`) in a `contents: write` workflow — supply-chain exposure. (PIPE-13 merged.) | workflow:22,25,30 | Pin SHAs + Dependabot for actions (S) |
| TEST-08 | P3 | No tags, version never bumped (0.1.0), no documented deploy procedure — "what is live on Connect right now" is unanswerable from the repo (compounds TEST-02). | `DESCRIPTION:4` | Document deploy (writeManifest → push → redeploy); tag on deploy; NEWS.md (S) |
| TEST-09 | P3 | Contract tests assert literal source text byte-for-byte (newline-sensitive SDMX URL strings; the exact — invalid — cron line), training contributors to update tests to match code. The "ABS API contract" test never calls the API. | `tests/test_data_refresh_workflow.R:20-37` | Parse YAML structurally; mock/recorded API responses (httptest2) (M) |
| TEST-10 | P3 | `pipeline/04_derive_indicators.R` — the stage computing the indicators the dashboard exists to show — has **zero** test references; downstream checks are schema-only. | `pipeline/04_derive_indicators.R` | Unit-test derivations against hand-computed fixtures (pairs with TEST-04) (M) |

**CI coverage:** 6 scripts run (schedule/dispatch only) — refresh contracts. Never run in CI: 8 pipeline/data-contract, ~19 module/scenario, ~13 static UI/chart, ~10 release/repro-hygiene scripts.

---

## 4. Cross-cutting themes

1. **Failure paths are the weak layer.** The happy path is verified excellent (exact ABS reproduction, correct caching, loud `required=TRUE` fetches). But almost every failure path degrades silently: empty-data.frame loader (SHINY-01), warning-downgraded parser errors + stale-file-passing gates (PIPE-03), regex selection misses (PIPE-04/05), frozen RBA cache stamped "fresh" (PIPE-01/02), and a stale manifest no check would catch (TEST-02).
2. **Two components misstate what they measure.** Deposit Gap (mis-cited anchor, ECON-01) and the MSI score component (interest-only captioned as repayments, ECON-02) — both have small, already-in-codebase fixes.
3. **Labels lag the data by one step.** State means as cities (STAT-01), "national" rents that are capital-city (ECON-05), "Bottom 40%" covering two populations (ECON-03), "YoY" that may not be (STAT-02), frequency "Quarter" on semiannual series (STAT-05). Individually small; collectively they erode the credibility the methodology work has earned.
4. **The test corpus is inverted.** 8.5k lines of tests, 89% never run automatically; the deployment artifact is the one critical file with no test at all.
5. **Accessibility is the one genuinely uncovered front-end area** — contrast, colour-blind safety, screen-reader support all fail in ways the prior UI reviews didn't examine.

## 5. Appendix — method & coverage

- Files read: app.R, plot_setup.R, all R/ modules and helpers cited above, all pipeline stages, the CI workflow, manifest.json, .rscignore, 56 test scripts (10 deep-read), renv.lock/DESCRIPTION metadata, prior docs/ reviews, ABS statistical guide PDF, NZ methodology PDF, SIH workbooks 4/5/8/13 + Table A3 (via readxl), graphify report.
- Verification: NHHA/stress-band/cost-ratio cells compared numerically against workbook values (all exact); all P1 file:line citations re-verified in source before publication; annuity math in `market_entry_scenarios.R` checked by hand (correct); CPI splice continuity checked (clean).
- **Not covered:** live browser/runtime QA (no R runtime in the review sandbox — startup time and rendered-output checks remain open; last confirmed boot 2026-05-31); `app_old.R` internals (legacy; archival recommended); load/concurrency testing; security posture beyond CI supply-chain notes.
- Companion document: `docs/roadmap.md` (prioritized feature/change roadmap derived from these findings).

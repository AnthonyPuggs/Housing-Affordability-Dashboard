# Track 3 — Hygiene: Implementation Plan

**Date:** 2026-06-12 · Executes the Track 3 items of `docs/roadmap.md` in five independently shippable batches. Status boxes below track progress between branches/sessions.

**Strategic anchor:** SIH 2025-26 readiness (PIPE-12). ABS cancelled SIH 2023-24, so the next survey release (~2027) is the dashboard's biggest future event — the positional Excel parser must be rewritten to header-anchored parsing *before* that release lands, so layout shifts re-anchor automatically or fail loudly instead of shipping wrong numbers. The rewrite also eliminates ~20,000 known duplicate key rows (parser artifacts) currently tolerated by the ratchet gate in `pipeline/06_validate_outputs.R`.

**Closed without work:** SHINY-05 "module scaffolding dedup" — `policy_kpi_box`/`policy_info_icon`/`policy_card` are already centralized in `R/ui_style_system.R`; the duplicated helpers named in the review no longer exist.

**Cross-cutting rules:**
- Byte-literal contract tests pin source text (workflow YAML, module text, README). Any change to pinned text updates the matching `tests/test_*.R` in the same commit.
- After file add/removals affecting the bundle: regenerate `manifest.json` (`rsconnect::writeManifest(appDir='.')`) and run the release checklist.

---

## Batch 1 — Quick wins (S items) — [ ]

- [ ] **1a. Archive legacy files (SHINY-09):** move `app_old.R`, `README_old.md`, `_check_cpi.R`, `save_plots.R`, `plots/` → `archive/` (git mv); delete empty `project_plan.md`. Update `tests/test_public_release_hygiene.R:57-93` patterns, `.rscignore`, `README.md:293`, `plot_setup.R:4` comment. Regenerate manifest.
- [ ] **1b. Plotly private-state fix (SHINY-06):** `R/overview_module.R:304-309` — replace `session$userData$plotlyShinyEventIDs` mutation with `plotly::event_register()` on the chart object + guarded `event_data()` reads. Verify score-chart click-to-filter.
- [ ] **1c. Houses Share KPI (SHINY-07):** retitle subtitle to "% of total approvals (national)" (`R/housing_supply_module.R:76-81`) — KPI is national-only and ignores page filters; label must match data.
- [ ] **1d. SDMX pinning + price-index honesty (PIPE-09/11):** extract SDMX endpoint URLs (`pipeline/02_fetch_abs_timeseries.R` ~138, ~272, ~407-410) to constants in `pipeline/00_config.R`; pin dataflow versions; assert expected series labels post-fetch. Fixed named base quarter for the mean-price index (currently `first(value)`, line 53); rename `"RPPI"` label (line 77) honestly — it is 6432.0 mean dwelling price, not the discontinued RPPI. Chase consumers + pinned tests.
- [ ] **1e. align_quarterly + RSE sentence (STAT-06/07):** complete-quarter rule in `R/derivation_helpers.R:11-26` (monthly inputs need 3 obs/quarter) + test. Methodology sentence in `R/methodology_module.R` (~171): RSE > 50% flagged not suppressed; no error propagation on derived indices. Update method-text contract test.
- [ ] **1f. Deploy discipline (TEST-08):** bump `DESCRIPTION` 0.1.0 → 0.2.0; create `NEWS.md`; `docs/deploying.md` procedure (manifest regen → checklist → push → tag vX.Y.Z); tag the deployed state.

## Batch 2 — CSS/JS to `www/` + real tooltips (SHINY-08, UX-12) — [ ]

- Move inline CSS (`app.R:100-816`, ~717 lines) → `www/dashboard.css`; the two JS blocks (`app.R:817-854` navbar collapse, `866-875` splash removal) → `www/dashboard.js`; link via `tags$link`/`tags$script`. Keep R-interpolated CSS inline if any (check first).
- Replace CSS pseudo-element tooltips (`content: attr(data-tooltip)`, app.R ~421-470) with `bslib::tooltip()` on `policy_info_icon()` (`R/ui_style_system.R:29-37`) — fixes WCAG 1.4.13 dismissibility. Keep `aria-label`. Update `tests/test_ui_style_system.R:99-100`.
- Regenerate manifest; confirm `.rscignore` doesn't exclude `www/`; UI smoke both themes.

## Batch 3 — SIH header-anchored parser rewrite (PIPE-12, priority) — [ ]

**Root cause (confirmed):** the four ratcheted outputs (`sih_timeseries_national` 1,936 dups, `sih_state_timeseries` 15,337, `sih_recent_buyers_2020` 216, `sih_geographic_2020` 2,858; gate at `pipeline/06_validate_outputs.R:250-291`) are exactly the four parsers that never call `estimate_block_rows()` — they parse RSE/MOE blocks as estimates. The six clean files already block-bound, so expected diffs are fully predictable.

**Architecture:** new `pipeline/sih_layouts.R` (sourced by `01_process_sih.R`) — spec-driven engine; `01_process_sih.R` becomes an orchestrator with one declarative layout per workbook. Core helpers:

- `read_sheet_raw(file, sheet)` — readxl, `col_names=FALSE`, `col_types="text"`, no skip
- `require_label_row(raw, pattern, file, sheet, what)` — header row by text or `pipeline_problem()` naming file/sheet/label
- `find_block_bounds(raw, start="^ESTIMATES", stop="^(RELATIVE STANDARD ERROR|95% margin of error|Source|...)")` — generalizes `estimate_block_rows()`; reused with swapped bounds by quality parsers
- `anchor_columns(raw, header_band, column_spec, file, sheet)` — logical→physical columns by header text (forward-fill merged headers); replaces all 6 hard-coded column maps; asserts matched count == spec length
- `sih_assert(cond, file, sheet, what)` — fail-loud wrapper over `pipeline_problem()`
- Orientation engines: `sih_parse_years_across` (Files 1, 12), `sih_parse_columns_down` (3, 4, 5, 6, 9, 11), `sih_parse_state_sections` (8), `sih_parse_year_rows_sectioned` (13)
- Every engine ends with an in-parser zero-duplicate assertion on the 7-column key; `classify_tenure()`/section classifiers return NA on no-match and the engine raises (no silent `"all"` default)

**Migration order & per-file verification** (committed workbooks → deterministic; committed CSVs are baselines): regenerate one CSV → `git diff` → assert expected diff class → `validate_sih_workbook_benchmarks()` → `sih_estimate_quality.csv` unchanged.

1. Zero-diff proving: Files 3+4 → 5 → 6 (byte-identical required)
2. Dup removal (diff = only RSE/MOE rows vanish; removed keys must persist among retained rows, no new keys, retained values unchanged): File 11 → File 9 → Files 1+12
3. Files 8 + 13 (complex orientation, zero-diff) → 3 quality parsers last (must be dup-free without the `distinct()` masking at `01_process_sih.R:1209`)

**Cleanup (load-bearing order):** (1) all four dirty files migrated → full pipeline run → dup-free `data/`; (2) delete ratchet → universal `duplicate_count(...) == 0` over all SIH outputs; (3) regenerate frozen fixtures (`tests/fixtures/generate_fixtures.R`) — they're currently frozen from dup-containing outputs; (4) remove `geo_keep_largest_estimate` (`R/geographic_affordability_module.R:82-90`) + 4 call sites (~216, 272, 282, 337); verify displayed values unchanged.

**Benchmarks (≥1 cell per output):** add one stable "Total/All households" cell per uncovered output (timeseries_national, costs_2020, age_tenure_2020, recent_buyers_2020, geographic_2020, state_timeseries) to `R/sih_benchmarks.R`, hand-read from workbooks; add `value_column` field (default `"value"`, `"quality_value"` for the quality output). Update `tests/test_sih_workbook_benchmarks.R` floors.

**Risks:** merged-header forward-fill ambiguity (mitigated by column-count assert + benchmarks); File 12's positional 24-sheet state×metric cycle isn't header-derived — assert sheet count/cycle minimum, title-cell anchoring is stretch; fixture/data regeneration order above.

**Commit phasing:** (1) engine + Files 3/4, (2) Files 5/6, (3) File 11, (4) File 9, (5) Files 1/12, (6) Files 8/13 + quality parsers, (7) ratchet deletion + fixtures + workaround removal, (8) benchmark expansion.

## Batch 4 — Refresh-workflow hardening (TEST-06/07, PIPE-08) — [ ]

`.github/workflows/data-refresh.yml`: `git pull --rebase` before push; `cancel-in-progress: false` (queue); `if: failure()` step opening/commenting a GitHub issue; diff-threshold guard (`git diff --numstat -- data/`) → open PR instead of direct push for large revisions; pin actions to commit SHAs (with version comments) in both workflows. Same commit: update `tests/test_data_refresh_workflow.R` — natural moment to convert it to structural YAML parsing (TEST-09 start).

## Batch 5 — Polish (opportunistic) — [ ]

- **Structural contract tests (TEST-09):** convert top offenders (`test_data_refresh_workflow.R` done in Batch 4, `test_app_method_text.R` → single source of truth for required phrases, endpoint-grep tests → test the `00_config.R` constants). Convert others only when they break.
- **Hover/modebar consistency (UX-06/07):** shared modebar config + formatted hover-text aes convention in `R/chart_builders.R`; conditional date-axis helper. `plotly_layout()` (`R/dashboard_theme.R:29-68`) is the central hook.
- **Chart accessibility (UX-02/04/08/10/11):** verify `city_palette()` colour-blind safety + apply everywhere; `role="img"` + `aria-label` wrappers; official-vs-stylised marker on KPI boxes; mobile-safe KPI grids; heatmap text-colour port.

---

## Verification (every batch)

1. `Rscript -e "testthat::test_dir('tests', stop_on_failure = TRUE)"` (smoke test needs Chrome/Edge)
2. `Rscript -e "source('R/release_checklist.R'); validate_release_checklist()"`
3. Pipeline batches (1d, 3): `Rscript pipeline/05_driver.R` and inspect `git diff data/` against the expected diff class
4. App batches (1b, 1c, 2, 5): run app + `docs/` UI smoke checklist, both themes
5. Regenerate `manifest.json` after any bundle-affecting file change
6. `graphify update .` after code changes

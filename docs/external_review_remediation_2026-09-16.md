# External review remediation — implementation record

Date: 16 September 2026. Branch: `codex/external-review-remediation`.
Baseline: `0b5e17583cdca9bcc4e44937822859bdfa5ee57b`.

This records execution of the [remediation plan](superpowers/plans/2026-09-16-external-review-remediation.md). The [independent assessment](external_reviews_assessment_2026-09-16.md) retains the complete finding-by-finding rulings, including rejected and qualified security claims.

## Implemented scope

| Finding | Implementation |
|---|---|
| B1, B3, B7 and direct chart/migration consumers | Exact calendar comparisons replace positional lags. Missing comparison periods produce unavailable results. Price/rent chart windows retain the prior observations needed for growth calculations. Annual migration totals require four consecutive quarters. |
| B2 | Explicit AWE frequency regime: quarterly historically, half-yearly from November 2012. Quarter-aligned dependent indicators preserve the same cadence. Registry, outputs and frozen fixtures agree. |
| B6, B8 | Shared monthly-rate aggregation requires three distinct finite months. Partial quarters and duplicate months cannot pass. App repayment/AWE percentages remain distinct from the pipeline repayment/WPI index. |
| B4, B9e | RBA acquisition validates temporary candidates before promotion, checks exact required name/ID pairs, recognises valid CSV/XLSX caches and preserves the previous cache on failure. Strict refresh failures stop; non-strict stale fallback warns explicitly. All returned observations must be finite. |
| B5 / qualified VULN-001 | Runtime confidence and provenance share a snapshot; repository Git checks remain strict in CI and are explicitly unavailable in runtime reporting. The rejected tab-toggle exploit premise is not reinstated by this optimisation. |
| B9c | Arithmetic and geometric sensitivity scenarios require all included components to be finite. A leave-one-out scenario may still evaluate when its deliberately omitted component is unavailable. |
| B9g | Conflicting SIH quality payloads fail with key/provenance diagnostics before joining or stage-06 validation. Identical repeated payloads may collapse. |

## Evidence

- Regression tests were witnessed failing before fixes for calendar gaps, cadence metadata, incomplete quarters, RBA acquisition failures, missing sensitivity inputs and conflicting SIH quality metadata.
- Independent review identified three additional cases, all reproduced and corrected: the geometric sensitivity path, non-finite secondary RBA series and the non-strict cache test under `CI=true`.
- The RBA failure suite passes 48 assertions under `CI=true`, using mocked transport and a real synthetic XLSX fixture parsed by the production parser. It makes no external request.
- A Shiny reactive test verifies growth at the first displayed date when its comparison date lies outside the selected chart window; index rebasing remains tied to the selected window.
- An independent byte/field comparison of all four edited CSVs confirmed unchanged row counts and unchanged non-frequency fields: production source 7,584 rows, production derived 1,009 rows, fixture source 2,279 rows and fixture derived 1,000 rows.
- Stage 04 was rerun against saved inputs in a temporary directory. All 1,009 resulting indicator rows match the committed, metadata-corrected output within `1e-12` tolerance. No live refresh or production CSV rewrite was performed by this check.
- The production serviceability series gives October 2025 minus October 2024 = −1.750301 percentage points, displayed as **−1.8 pp YoY**.
- The ABS housing-affordability guide, the NZ indicator/living-cost references and ABS SIH workbooks 4, 5, 8 and 13 were consulted. Official SIH cells, denominators, score weights, reference window and normalisation are unchanged.

## Final verification

- Full `testthat::test_dir("tests")` run: **1,677 passed, 0 failed, 0 errors, 0 skipped, 1 warning**. This includes 27 browser smoke assertions covering all nine navigation panels.
- Strict `validate_release_checklist()`: **33/33 checks passed**.
- The remaining warning is the known Plotly `overview_afford_score` event-registration warning in the isolated server-reactive test. No browser output error was detected. The inherited Windows `C.UTF-8` startup locale is invalid; setting the test process to `English_Australia.utf8` avoids truncation when the existing YAML test reads UTF-8 workflow text.
- Runtime tests use an actual manifest-shaped bundle without `.git` or `renv/activate.R`. Runtime marks repository Git hygiene and the repository renv bootstrap unavailable, while repository mode retains blocking checks. Missing saved data and missing manifest members still fail.
- Invocation counters confirm one inventory build and zero Git calls across a shared snapshot, two sessions and repeated writes. A static observer remains at one invocation after 20 suspend/resume cycles.
- Manual local-browser checks confirmed the **−1.8 pp YoY** subtitle, the runtime confidence explanation and a successful Methodology download event. Gapped-date and chart-window behaviour was verified with deterministic helper and Shiny-reactive tests. No production deployment was tested.
- Independent source review closed all four findings it raised: geometric sensitivity completeness, secondary RBA non-finite values, CI strictness in a non-strict test, and the unbundled renv activation script. The latter was reproduced by a corrected bundle fixture before the fix.
- `graphify update .` completed successfully using AST extraction. `manifest.json` was regenerated using tracked files filtered by `.rscignore`; all 120 dependency names and versions remain unchanged and match `renv.lock`. Regeneration refreshes platform build metadata from the local Windows installation and includes the tracked licence as well as the new calendar helper.
- `git diff --check` passed. No package versions or official SIH data files changed.

All seven implementation tasks are complete. The fixes are separated into local task commits, followed by a documentation/generated-artifact commit; the final integrated tree is the tested release candidate.

| Task | Local commit |
|---|---|
| Calendar comparisons | `d882da5` |
| AWE cadence metadata | `80e5a43` |
| Complete monthly quarters | `b98e3d3` |
| Validated RBA cache promotion | `743b86a` |
| Runtime confidence/provenance | `c24eed0` |
| Sensitivity completeness | `eaba1a1` |
| SIH quality conflicts | `b0fc4b6` |

## Local integration into main

The requested local merge incorporated the nine automated data-refresh commits between the implementation baseline and upstream `main` at `da2b344`. The derived-data merge conflict was resolved by retaining the refreshed observations and applying the intended AWE-dependent frequency metadata. An independent field comparison confirmed that all non-frequency fields were preserved in the 7,624 source rows and 1,017 derived rows.

The merged code reproduced all 1,017 refreshed derived rows within `1e-12` tolerance. The newer serviceability series now ends in April 2026, with a year-on-year change of +3.385689 percentage points, displayed as **+3.4 pp YoY**. The earlier −1.8 pp result above records the original implementation snapshot, not the subsequently refreshed data.

Merged-tree verification passed **1,684 assertions, with zero failures, errors or skips**, including all nine browser panels. The same isolated Plotly warning remains. The strict release checklist passed **33/33** checks. Integration is local; no push or deployment was requested.

## Scope boundaries

No live ABS/RBA refresh, dependency upgrade, remote push or deployment forms part of this change. The original assessment's lower-priority UI validation messages, broader helper sensitivity domain, tooltip escaping and workflow-comment repair remain separate follow-ups. Tenure classification and fixed index-base decisions remain deferred methodology work.

The supplied security review did not establish the claimed tab-toggle denial-of-service exploit or package-authentication failure. Runtime caching and optional renv cache hashes must not be represented as fixes for demonstrated exploits.

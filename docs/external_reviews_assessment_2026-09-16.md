# Independent assessment of the external bug and security reviews

Date: 16 September 2026. Scope: assess the supplied advice and specify proportionate fixes. Application code, calculations, committed CSVs, dependencies and deployment configuration have not been changed by this assessment.

## Conclusion

The bug review identifies several real defects, particularly calendar comparisons and RBA cache handling. Its proposed fixes require correction in several places. The security review's sole Medium finding is **not established as described**: switching tabs does not repeatedly invalidate the static Methodology output. Repeated downloads and new sessions do cause repeated work and warrant performance hardening, but production resource exhaustion was not demonstrated.

Prioritise calendar correctness, truthful frequency metadata, valid complete-quarter aggregation, reliable RBA acquisition, and separating repository release checks from deployed runtime reporting. Do not replace the AWE-based repayment burden with the WPI-based serviceability index, introduce interpolation, weaken CI failures, or treat renv cache hashes as package-content integrity signatures.

## Evidence and scope

The files were absent from `resources/` and were read from:

- `C:/Users/antho/Downloads/BUG_REVIEW.md`, dated 12 September 2026; SHA-256 `1135FA57710620E54C00527DF169E73BA597DF8807B9785A83BD08E3705C942F`.
- `C:/Users/antho/Downloads/SECURITY_REVIEW.md`, dated 13 September 2026; SHA-256 `E9DA8FEB8DD435053E5B5A295265846943C9637E347B66FA385A0AEF93A1B2DC`.

Processes used: Superpowers receiving-code-review, systematic-debugging, writing-plans and verification-before-completion. The graph report was consulted for orientation; current source and executable behaviour took precedence over that older graph. The review traced every numbered finding and all ten rows in the bug review's nominally “nine smaller items” table. This is a validation of the supplied findings and their direct consumers, not a new exhaustive security audit.

R 4.5.3 and the existing project packages were used. Initial sandboxed runs could not read the external renv cache; authorised execution outside that sandbox resolved the issue without installing or upgrading packages. Temporary fixtures and mock downloads were used; no external pipeline downloads or deployment load tests were run.

Fresh targeted tests: **284 passed assertions; 0 failed, 0 errors, 0 skipped, 0 warnings**, across the 11 matching test files below. These are baseline tests, not evidence that the defects are fixed. Separate adversarial checks reproduced failures that the baseline suite misses.

```r
testthat::test_dir(
  "tests",
  filter = paste(c(
    "derivation_helpers", "overview_module", "scenario_sensitivity",
    "serviceability_scenario_controls", "national_affordability_score",
    "sih_quality_helpers", "rba_raw_cache_hygiene", "provenance_report",
    "release_checklist", "dependency_reproducibility", "pipeline_outputs"
  ), collapse = "|"),
  stop_on_failure = TRUE
)
```

The full suite, browser smoke test, live pipeline, deployed bundle and dependency restore were not run. Local resource files were listed, but an R workbook read did not resolve the SIH directory; no workbook-methodology validation is claimed. Before implementing parser or affordability-methodology changes, consult the required local PDFs and workbooks as specified in `AGENTS.md`.

The supplied review's data snapshot differs from this checkout. Here, serviceability and the saved Deposit Gap end at **1 October 2025**, rather than the review's April 2026. Findings below use the current local data, not the external report's row counts.

## Finding register

Priorities are remediation order: P1 means current misleading output or a reproducible source-acquisition failure; P2 means conditional correctness or deployment behaviour; P3 means hardening or hygiene. They are not security CVSS ratings.

| ID | Independent ruling | Evidence and corrected scope | Recommended disposition |
|---|---|---|---|
| B1 | **Confirmed, P1** | `R/overview_module.R:572` compares rows 1 and 5. Actual serviceability has 31 rows; the latest comparison spans October 2025 to October 2023. It displays approximately **-0.8 pp YoY**, whereas the exact October 2024 comparison is approximately **-1.8 pp**. | Replace the inline row comparison with an exact prior-year date lookup. Blank the change when that observation is absent. A zero previous burden is valid for a percentage-point difference and need not be excluded. |
| B2 | **Confirmed metadata defect, P1; proposed repair unsuitable** | `pipeline/02_fetch_abs_timeseries.R:375`, `R/indicator_registry.R:161`, stage 04's `indicator_output()` and `R/national_affordability_score.R:256` propagate “Quarter”. AWE has four observations in 2010–11, three in 2012, then two per complete year from 2013. Current Deposit Gap and score carry “Quarter” despite half-year spacing. Executing the suggested existing inference function on post-2013 AWE returns **“Year”**, not half-year. | Define an explicit historical frequency regime and distinguish the quarter-aligned date from observation cadence. Update registry, source metadata, score outputs, data and fixtures together. Do not let a modal gap redefine intended frequency when a source observation is missing. |
| B3 | **Confirmed latent defect, P2** | `R/dashboard_formatting.R:20` interprets a row offset as a calendar interval. A gapped fixture returns 50% “YoY” across 15 months; its actual prior-year increase is 36.36%. | Use an explicit calendar interval in the API and migrate all callers, including monthly labour indicators. Preserve percentage-point versus relative-change semantics. |
| B4 | **Confirmed, P1** | `pipeline/00_config.R:267` writes into the cache before checking HTTP status. With a mocked HTTP 500 followed by successful XLSX fallback, the first call returned XLSX and the second returned the HTTP-error CSV without another request. Separately, `parse_rba_file()` returned an empty tibble with `PIPELINE_STRICT = TRUE`; its direct `warning()` bypasses `pipeline_problem()`. | Download and validate temporary candidates before promotion; recognise a valid fallback cache; retain known-good data on failure. Fail at stage 03 with table identity on missing, unparseable or empty required tables. Check required series outside non-empty guards. |
| B5 / VULN-001 | **Partly confirmed, P2; tab-toggle exploit rejected** | `R/methodology_module.R:303` calls the repository checklist, which runs three Git commands and reads saved CSVs. Simulated unavailable Git produced two false `fail` hygiene rows and one `warn` manifest row. A static Shiny output ran once after 20 tab-input changes; a static observer ran once after 20 suspend/resume cycles. The Methodology module has no `bindCache()` siblings, contrary to the security report. | Separate deployment-time repository checks from runtime confidence reporting. Share a static runtime snapshot between display and download. Retain strict repository/CI failures; represent intentionally unavailable repository checks explicitly in runtime mode. Avoid process-global `setwd()`. |
| B6 | **Confirmed conditional inconsistency, P2; suggested substitution invalid** | `R/precomputed_series.R:104` uses partial quarterly means for monthly rates. Removing the final monthly rate from the latest serviceability quarter made the app retain the quarter while `complete_quarter_mean()` excluded it. Current app-only rate coverage also includes April 2004, before the displayed serviceability sample. | Share complete-quarter aggregation. The app's output is repayment/AWE in per cent; the pipeline Mortgage Serviceability Index is repayment/WPI rebased to 100. They are different measures and must not be substituted for one another. No current latest-quarter numerical disagreement was demonstrated. |
| B7 | **Confirmed latent defect, P2** | `R/derivation_helpers.R:152` returns 50% for the same 15-month fixture instead of the true 36.36% annual change. Current real house-price growth (55 rows), real wage growth (112 rows) and MSI (59 rows) have no non-quarterly gaps. | Use exact period keys and return no growth value when the prior-year observation is missing. |
| B8 | **Confirmed latent defect, P2; one-line fix incomplete** | `R/derivation_helpers.R:17` averages January=10, February=NA, March=30 to 20 and accepts the quarter. A duplicate January row replacing February also passes. | Require the expected distinct periods and finite values. Count unique months for monthly inputs; reject duplicates. Preserve legitimate quarterly and half-year input treatment. `sum(!is.na(value))` alone still admits infinite values and duplicated months. |
| B9a | **Confirmed UI inconsistency, P3** | `R/affordability_module.R:496` calls the validating scenario helper without converting its errors to Shiny validation messages. Numerical validation still exists; this is not a bypass of the calculator's domain constraints. | Add the same `tryCatch(..., error = function(e) validate(need(FALSE, conditionMessage(e))))` boundary used by sibling calculators. |
| B9b | **Confirmed helper limitation, P3; ordinary UI trigger overstated** | `R/market_entry_scenarios.R:403` omits a deposit of 45% and produces deposit cases 35% and 40%. However, `R/affordability_module.R:303` exposes a 5–40% slider; 45% requires a direct helper call or modified client request. Boundary deduplication and fewer than four points are not inherently defective. | Explicitly define the sensitivity domain. Prefer retaining every valid base case in the helper rather than restricting the more general scenario calculator. Require the base case, not exactly four distinct points. |
| B9c | **Confirmed sensitivity-only defect, P2/P3** | The cited `sum(..., na.rm = TRUE)` is inside `national_affordability_score_sensitivity()`, not `calculate_national_affordability_score()`. Scores 50, 50, NA produce a default sensitivity value of **37.5**. | Return NA for a scenario when a positively weighted component is unavailable. Leave-one-out scenarios may remain available if all included components exist. Do not silently renormalise the headline or change its published weights. |
| B9d | **True local observation; no demonstrated validation bypass, P3** | Individual `all(numeric(0))` range gates pass, but `pipeline/06_validate_outputs.R:309` subsequently tests every registered indicator's minimum count, treating an absent indicator as zero. | Optional clearer local non-empty assertions. Add a deletion regression proving the complete validator rejects absent indicators. Avoid describing this as an unguarded pipeline hole. |
| B9e | **Valid weak-contract criticism, P2/P3; “never fires” too absolute** | The F6 regex accepts unrelated rate/loan series and does not establish presence of the required new owner-occupier loan rate. It can still fail for names matching none of its alternatives. Downstream exact selection already provides protection. | Require the exact series identity consumed by the splice before writing stage-03 output. Validate the expected ID/name pair and retain any separately required chart series. |
| B9f | **Defence-in-depth, P3; exploitable XSS not established** | `R/rental_market_helpers.R:106` and `R/affordability_module.R:659` concatenate source labels with tooltip HTML. No attacker-controlled cross-user delivery path was demonstrated. Plotly's limited HTML handling also does not establish arbitrary script execution merely from concatenation. | Escape data-derived text fragments while preserving deliberately generated `<br>` markup. Verify literal angle brackets, ampersands and tag-like labels in tooltips. |
| B9g | **Confirmed latent ambiguity, P2** | `R/sih_quality_helpers.R:177,191` keeps the first quality row per reduced key. Injecting a second source with a different value changed the joined RSE from **1.4 to 11.4** when row order was reversed. Current 1,652 quality rows contain zero duplicate reduced-key/measure groups. | Detect conflicting metadata before joining. Identical repeated values may be collapsed explicitly; conflicting values must fail with source provenance. Do not add source keys to the join without also giving estimates those keys. |
| B9h | **Partly valid parser risk; blanket fix rejected** | `classify_tenure()` is also called for family, age and income rows. Current File 5 outputs legitimately carry tenure `all` for labels such as “15 to 24” and “Couple family with dependent children”. | Make classification depend on parser section. Fail on unknown labels only where the section represents tenure; retain validated demographic sections. No currently misclassified official estimate is claimed. Workbook and PDF checks are required before this change. |
| B9i | **Valid methodology/reproducibility concern, P3** | `index_to_base()` bases on the first surviving common observation. History extension or removal of the base period changes index levels. That is a methodology stability issue, not proof the current arithmetic is wrong. | Specify fixed reference dates separately for each index; fail if an agreed base is absent. Review against the methodology references and document the version/rebasing decision. Defer from the initial bug-fix batch. |
| B9j | **Confirmed documentation mismatch, P3** | `.github/workflows/data-refresh.yml:54` says human review; its later branch explicitly creates and merges the PR in the same run. | Correct the stale comment and its literal contract tests. Retain current automatic-merge policy unless the owner deliberately changes it. |
| VERIFY-001 | **Missing hashes confirmed; security interpretation rejected** | All 120 lockfile records lack `Hash`. renv documents this as an optional cache hash. The installed implementation reads DESCRIPTION metadata and passes it to `renv_hash_record()`; this is not an archive integrity signature. The cause of this lockfile's construction was not established. | Treat as dependency-reproducibility housekeeping: inspect `renv::status()` and schema validity, compare a controlled snapshot, and preserve versions. Do not claim that adding hashes authenticates package contents or infer a compromised lockfile. |
| VERIFY-002 | **Conditional hardening, not a demonstrated exploit** | App-scoped caches are the Shiny default, but the documented default is bounded at 200 MB and evicts entries. Invalid or continuously varying keys can still create CPU work and cache churn. No private user dataset is involved here. | Validate/normalise categorical and date keys; preserve numerical domain checks. Inspect actual deployment cache and worker limits before changing scope or limits. Session-scoped caching trades shared eviction for repeated computation and is not automatically safer. |

## Direct-consumer omissions found during validation

`R/chart_builders.R:11,63,70` also computes annual/quarterly changes by row position. Both annual transformations reproduced the gapped-series error. Include price and rent chart transformations in B3/B7's repair; otherwise KPI and chart behaviour can diverge. `R/market_context_module.R:93` also constructs annual migration totals from four successive rows: require four consecutive quarters and the comparable prior-year window before labelling these annual totals or changes. This is a bounded extension of the same cadence contract, not a claim of a currently wrong migration total.

## Implementation decisions

1. **Calendar correctness first.** Resolve exact year/month/quarter comparison keys, with a declared date convention; never use a nearest available observation. Use explicit intervals rather than `periods_back` to infer calendar meaning. Test missing comparison dates, internal gaps, leap dates, shuffled input and duplicate keys.
2. **Cadence is metadata, not imputation.** The ABS change began with November 2012. Preserve historical quarterly AWE metadata and mark subsequent observations half-yearly. Quarter-start alignment does not create quarterly observations. A missing quarterly observation must fail its own coverage contract rather than change a series to half-yearly.
3. **Complete monthly quarters need three distinct finite monthly observations.** Apply this to monthly rate inputs shared by app and pipeline. Do not require three observations for the quarter-aligned AWE input. Keep the different AWE and WPI denominators explicit.
4. **Only validated RBA candidates become caches.** Test HTTP errors, transport errors, malformed HTTP-200 payloads, interrupted writes, empty parser output, fallback reuse and unavailable required series. A failure must not refresh the apparent age of invalid data.
5. **Runtime reporting should consume runtime evidence.** Keep Git hygiene and tracked-file inspection in repository/CI checks. Prepare a per-process runtime summary and static provenance body once; use it for the table and each download. If reporting a download timestamp, generate that lightweight wrapper at request time. A session-only cache still repeats work for each new session and does not fix downloads that bypass it.
6. **Keep smaller hardening changes separate.** Sensitivity completeness, scenario validation, quality conflicts and tooltip escaping are individually reviewable. Tenure and index-base changes require domain validation. Neither security recommendation justifies a broad authentication redesign or dependency upgrade.

The execution sequence and test contracts are in [the remediation plan](superpowers/plans/2026-09-16-external-review-remediation.md).

## Source checks

- ABS states that May 2012 was the final quarterly AWE issue and November 2012 the first biannual issue: [ABS May 2012 release](https://www.abs.gov.au/AUSSTATS/abs%40.nsf/Lookup/6302.0Main%2BFeatures1May%202012).
- Shiny documents resumption as scheduling execution when an observer was invalidated while suspended, consistent with the local experiment: [observer reference](https://shiny.posit.co/r/reference/shiny/latest/observe.html).
- Shiny documents app-scoped caching, eviction and its default size: [Shiny caching guide](https://shiny.posit.co/r/articles/improve/caching/).
- renv describes lockfile fields as derived from installed DESCRIPTION files and its hash as a cache identifier: [lockfile anatomy](https://rstudio.github.io/renv/articles/lockfile.html), [renv FAQ](https://pkgs.rstudio.com/renv/articles/faq.html). Context7 was also consulted for Shiny, renv and lubridate documentation.

## Completion checklist

- [x] Read both external reports in full.
- [x] Trace every finding to the current source and its immediate consumers.
- [x] Reproduce the material defects with real functions and isolated counterexamples.
- [x] Independently challenge the proposed fixes and exploit assumptions.
- [x] Run relevant existing tests and distinguish baseline coverage from new evidence.
- [x] Specify priorities, fix boundaries and acceptance checks.
- [ ] Implement remediation in separately verified batches; this is subsequent work.

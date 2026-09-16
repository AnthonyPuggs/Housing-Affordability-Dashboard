# External Review Remediation Implementation Plan

**Execution status:** Tasks 1–7 are implemented and committed on `codex/external-review-remediation`. The checklist below preserves the original plan; the [implementation record](../../external_review_remediation_2026-09-16.md) records the final evidence, adaptations, independent review and deferred scope. Final verification passed 1,677 assertions and all 33 strict release checks.

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task. Subagent-driven development is an alternative if the user chooses it. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Correct substantiated calendar, data-acquisition, aggregation and runtime-reporting defects while preserving the dashboard's statistical definitions.

**Architecture:** Retain saved CSVs as the app's only data source. Share pure calendar and aggregation helpers across relevant consumers; isolate source acquisition from validated cache promotion; separate repository release validation from runtime confidence reporting.

**Tech Stack:** R, Shiny, dplyr, lubridate, readr, httr, testthat, renv and rsconnect at the existing locked versions.

**Spec:** [Independent assessment](../../external_reviews_assessment_2026-09-16.md). Its complete finding register governs accepted, qualified and rejected recommendations.

## Global constraints

- Australian English; official SIH/NHHA estimates remain separate from stylised scenarios.
- Preserve current CSV column schemas and the public indicator names.
- No interpolation or carry-forward of AWE to manufacture quarterly observations.
- Keep AWE repayment percentages and WPI-normalised serviceability indexes distinct.
- Keep the score weights, reference window and calculation unchanged in metadata-only work.
- Consult `resources/` PDFs and the relevant ABS workbooks before changing affordability calculations or SIH interpretation.
- Use Context7 for library/API guidance; use the pinned local implementation when verifying version-specific behaviour.
- Keep dependency versions fixed. Use `renv::restore()` if restoration is needed, never ad-hoc package installation.
- Do not run a live refresh during unit tests. Use temporary directories and mocked acquisition; retain committed data until deterministic regeneration is reviewed.
- Run matching byte-literal module/workflow/document contracts when changing their text.
- After source changes, run `graphify update .`. Regenerate `manifest.json` when sourced-file membership or dependencies change, then run the release checklist.
- Baseline on 16 September 2026: the 11 selected test files passed 284 assertions. This does not cover the new edge cases.

## Scope and execution

Tasks 1–5 are the first remediation wave. Tasks 6–7 address separately reproduced latent defects. Lower-priority validation-message, sensitivity-domain, tooltip and workflow-comment repairs are specified in the assessment and should follow as independent small changes. Tenure classification and fixed index bases require a separate methodology decision and source-workbook validation.

Work in the existing checkout only after inspecting its current state. Preserve unrelated edits. Each task has its own failing regression, implementation, verification and reviewable commit; do not combine all changes into one unverified patch. The code blocks below define the central interfaces and counterexamples; wire them into the named consumers and test harness in the same task.

## Task 1: Calendar-keyed changes

**Findings:** B1, B3, B7 and the direct chart/migration consumers.

**Files:** create `R/calendar_helpers.R` and `tests/test_calendar_helpers.R`; modify `R/dashboard_formatting.R`, `R/derivation_helpers.R`, `R/overview_module.R`, `R/chart_builders.R`, `R/market_context_module.R`, `R/housing_supply_module.R`, `plot_setup.R`, `pipeline/04_derive_indicators.R`; extend `tests/test_overview_module.R`, `tests/test_derivation_helpers.R`, `tests/test_chart_builders.R` and matching module tests. Regenerate the deployment manifest because a sourced file is added.

**Interface:** `calendar_prior_values(date, value, months_back = 12L)` returns the value at each row's exact prior calendar date, in input order. The inputs describe one series. Duplicate or missing date keys fail. Missing comparison dates return `NA_real_`.

- [ ] Add and run these regressions before implementation:

```r
test_that("annual comparison uses calendar dates and never skips a gap", {
  date <- as.Date(c("2024-01-01", "2024-04-01", "2024-07-01",
                    "2024-10-01", "2025-04-01"))
  value <- c(100, 110, 120, 130, 150)
  expect_equal(tail(calendar_prior_values(date, value), 1), 110)
  expect_equal(tail(calendar_prior_values(date[-2], value[-2]), 1), NA_real_)
  expect_error(calendar_prior_values(c(date, date[1]), c(value, 999)),
               "unique")
})
```

- [ ] Implement the shared lookup with no nearest-date fallback:

```r
calendar_prior_values <- function(date, value, months_back = 12L) {
  date <- as.Date(date)
  if (length(date) != length(value) || anyNA(date) || anyDuplicated(date)) {
    stop("A series requires unique, non-missing date keys.", call. = FALSE)
  }
  if (length(months_back) != 1L || !is.finite(months_back) ||
      months_back <= 0 || months_back != as.integer(months_back)) {
    stop("months_back must be a positive whole number.", call. = FALSE)
  }
  target <- lubridate::add_with_rollback(
    date, -lubridate::months(months_back)
  )
  as.numeric(value[match(target, date)])
}
```

- [ ] Change `latest_change()` to take an explicit `months_back` argument; migrate its existing callers to 12 months. Derive default labels from 1/3/12 months as MoM/QoQ/YoY, or use the caller's explicit label. Reject duplicate series dates. Keep finite-value handling, zero-denominator handling for relative percentages and existing favourable-direction styling. Use the same helper for the Overview's percentage-point serviceability comparison; do not require five rows.
- [ ] For real growth and each grouped chart transformation, use the prior value from the same series/group:

```r
prior <- calendar_prior_values(date, real_level, months_back = 12L)
value <- ifelse(is.finite(prior) & prior != 0,
                100 * (real_level / prior - 1), NA_real_)
```

  Use `value` rather than `real_level` as the level vector in price/rent transformations. Use 3 months for QoQ. Preserve the existing quarterly date convention and perform transformations on sufficient source history before cropping the displayed window.
- [ ] In migration KPIs, match the four exact quarter keys ending at the latest quarter and the four ending one year earlier; require every matched value to be finite before summing. An absent quarter produces unavailable annual/change output, not a sum of four non-consecutive rows.
- [ ] Add module assertions for the half-year serviceability fixture, a missing exact prior-year observation, a valid zero prior burden in a percentage-point comparison, shuffled rows, multiple cities and leap-date rollback. Run `test_calendar_helpers.R`, the named derivation/chart/module tests, and the manifest checks. Confirm current gap-free series retain their values and the current serviceability change becomes approximately -1.8 pp rather than -0.8 pp.
- [ ] Review and commit this calendar repair independently.

## Task 2: Truthful AWE and derived-output cadence

**Finding:** B2. **Files:** modify `R/indicator_registry.R`, `R/national_affordability_score.R`, `pipeline/02_fetch_abs_timeseries.R`, `pipeline/04_derive_indicators.R`, `pipeline/06_validate_outputs.R`, score/methodology wording and the registry/score/pipeline tests. Regenerate affected `data/*.csv` and frozen fixtures only after verification.

**Interface:** `awe_observation_frequency(date)` maps raw ABS reference dates to `Quarter` through May 2012 and `Half-year` from November 2012. `awe_aligned_frequency(date)` applies the equivalent quarter-start boundary of October 2012 to derived AWE-dependent observations. The registry's frequency wording describes the historical change rather than claiming a uniformly quarterly series.

- [ ] Add a failing metadata regression:

```r
expect_equal(
  awe_observation_frequency(as.Date(c("2012-05-15", "2012-11-15",
                                     "2025-05-15", "2025-11-15"))),
  c("Quarter", "Half-year", "Half-year", "Half-year")
)
expect_equal(awe_aligned_frequency(as.Date("2025-10-01")), "Half-year")
```

- [ ] Implement explicit source-period metadata, rather than guessing from median or modal gaps:

```r
awe_observation_frequency <- function(date) {
  ifelse(as.Date(date) < as.Date("2012-11-01"), "Quarter", "Half-year")
}
awe_aligned_frequency <- function(date) {
  ifelse(as.Date(date) < as.Date("2012-10-01"), "Quarter", "Half-year")
}
```

- [ ] Apply the raw helper after `normalize_abs()` so existing upstream `frequency` columns cannot override the correction. In stage 04, apply aligned frequency only to Deposit Gap; in score generation, apply it to the headline and all three component outputs. Preserve dates and numeric values exactly.
- [ ] Update registry/public wording to “quarterly historically; half-yearly from November 2012” for the AWE-derived history, explaining quarter-start alignment. Add a stage-06 contract for AWE's allowed source months after the transition (May/November), its metadata and duplicate period keys. Keep coverage-gap validation distinct: missing May does not make November an annual series.
- [ ] Test the 2012 transition, missing half-year observations, quarter-aligned outputs and unrelated quarterly/monthly indicators. Regenerate outputs from fixed local inputs; compare all `(date, indicator, value)` triples before/after and require equality. Update fixtures and matched literal contracts in the same commit.
- [ ] Run registry, score, metadata and pipeline-output tests and review the data diff before committing.

## Task 3: Complete, finite monthly quarters in both app and pipeline

**Findings:** B6, B8. **Files:** modify `R/derivation_helpers.R`, `R/precomputed_series.R`, `plot_setup.R` and `tests/test_derivation_helpers.R`; extend app/precomputed-series and serviceability tests. Update sourcing/manifest membership if required.

**Interface:** extend `complete_quarter_mean(df, value_name = "value", expected_months = NULL)` and `quarterly_mean()` to accept an explicit expected count. Monthly rates call with `expected_months = 3L`. Existing single-observation quarter inputs retain their intended treatment; AWE is not interpolated.

- [ ] Add failing cases for NA, Inf, missing February, duplicate January, an all-incomplete monthly sample, empty input and a valid quarterly series. Core counterexample:

```r
monthly <- data.frame(
  date = as.Date(c("2024-01-01", "2024-02-01", "2024-03-01")),
  value = c(10, NA_real_, 30)
)
expect_equal(nrow(complete_quarter_mean(monthly, expected_months = 3L)), 0L)
monthly$value[2] <- 20
monthly$date[2] <- monthly$date[1]
expect_error(complete_quarter_mean(monthly, expected_months = 3L), "duplicate")
```

- [ ] Validate unique monthly keys for monthly inputs and count finite distinct periods. Preserve a typed empty result when no complete quarter remains. The central aggregation is:

```r
with_qtr <- df |>
  dplyr::mutate(qtr = lubridate::floor_date(date, "quarter"),
                month_key = format(date, "%Y-%m"))
if (anyDuplicated(with_qtr$month_key)) {
  stop("Monthly/quarterly input contains duplicate month keys.", call. = FALSE)
}
out <- with_qtr |>
  dplyr::group_by(qtr) |>
  dplyr::filter(dplyr::n_distinct(month_key[is.finite(value)]) == expected_months,
                all(is.finite(value))) |>
  dplyr::summarise(!!value_name := mean(value), .groups = "drop") |>
  dplyr::rename(date = qtr)
```

  Resolve `expected_months` before this block: validate an explicit positive integer; for legacy calls infer the typical distinct-month count, with a typed-empty early return. Explicitly pass 3 for every known monthly rate input so a sample containing only partial quarters cannot infer 1. Do not apply this monthly duplicate-key rule to daily data without a separate daily aggregation contract.
- [ ] Replace `mortgage_rate_qtr`'s naive mean with this helper and use the same path for monthly rate aggregation in `compute_mortgage_serviceability()` and `compute_real_mortgage_rate()`. Check the actual CPI inflation source cadence before choosing its expected count. Keep AWE/price aggregation separate and explicit.
- [ ] Re-run the synthetic latest-quarter deletion against app and pipeline consumers; both must exclude the incomplete monthly-rate quarter. Verify the annuity calculations and each distinct denominator remain unchanged for complete quarters. Consult the required methodology PDFs before finalising this calculation-path change.
- [ ] Run derivation, scenario and precomputed-series tests, regenerate affected outputs only if necessary, review any date coverage change, and commit.

## Task 4: Validated RBA cache promotion and stage-local failures

**Findings:** B4 and B9e. **Files:** modify `pipeline/00_config.R`, `pipeline/03_fetch_rba.R`, `tests/test_rba_raw_cache_hygiene.R`; create `tests/test_rba_fetch_failures.R`. Keep testable acquisition/parser helpers sourceable without executing pipeline stages.

**Interfaces:** `fetch_rba_table(table_id, cache_dir = DATA_DIR)` retains its return type: a validated CSV/XLSX path. Extract candidate validation and download dependencies so tests can mock HTTP without network access. Required-table parsing either returns data or raises a source-specific error in strict mode.

- [ ] Write an HTTP mock reproducing the existing two-call failure:

```r
requests <- character()
fake_get <- function(url, config) {
  requests <<- c(requests, url)
  is_csv <- grepl("/csv/", url, fixed = TRUE)
  writeLines(if (is_csv) "HTTP 500 error body" else "validated fixture", config$path)
  list(status = if (is_csv) 500L else 200L)
}
fake_write_disk <- function(path, overwrite = FALSE) list(path = path)
fake_http_error <- function(response) response$status >= 400L
```

  Inject these into a local function environment with a fixture validator. Assert that the successful XLSX fallback remains usable on the next call, the error body is never accepted as CSV, and failed acquisition does not change a known-good cache's bytes or timestamp.
- [ ] Change acquisition to write each attempt to a temporary candidate in the cache directory and validate both HTTP status and table structure before promotion. The state transition is:

```r
candidate <- tempfile(paste0("rba_", table_id, "_"),
                      tmpdir = cache_dir, fileext = ".csv")
on.exit(unlink(candidate), add = TRUE)
response <- GET(url, write_disk(candidate, overwrite = TRUE))
if (http_error(response)) stop("RBA candidate HTTP request failed.")
normalise_rba_csv_cache(candidate)
```

  Continue by validating title/series metadata, parseable observations and the required series contract, then promote the candidate. Implement promotion with a checked backup-and-restore sequence when replacement is not atomic on Windows; never unlink the last valid file before confirming its replacement. CSV failure must continue to XLSX candidates without leaving a fresh invalid CSV. Cache selection must consider valid CSV and XLSX candidates explicitly.
- [ ] Replace parser direct warning/empty returns with `pipeline_problem()` or an equivalent strict-mode-aware source error. Check `NULL`, missing files and empty frames outside the `nrow(...) > 0` branches. Error messages name F1/F5/F6/E2 and the failed operation.
- [ ] After F6 parsing, require `INDICATOR_SOURCE_RBA_NEW_LOAN_RATE` exactly before writing, and verify its current series ID against the saved input; do not use the broad “rate” regex as proof. Retain separately consumed series for charts.
- [ ] Test HTTP 404/500, interrupted transport, malformed HTTP 200, all fallbacks failing, valid fallback reuse, cache expiry, failed promotion, empty tables and a non-empty F6 table missing the required series. Test strict and non-strict behaviour separately.
- [ ] Run RBA cache/failure and pipeline-output tests without network requests, review the exact failure messages, then commit.

## Task 5: Runtime confidence snapshot and strict repository checks

**Findings:** B5/VULN-001. **Files:** modify `R/release_checklist.R`, `R/provenance_report.R`, `R/methodology_module.R`, `app.R`; extend `tests/test_release_checklist.R`, `tests/test_provenance_report.R` and Methodology tests.

**Interfaces:** `release_checklist(..., context = c("repository", "runtime"))` keeps repository mode as its strict default. Runtime mode excludes inapplicable Git hygiene assertions and records their unavailability. A process-scoped snapshot contains `confidence` and a static provenance body; both the table and download consume that snapshot.

- [ ] Write tests with a counting Git stub and an intentionally failing Git result. Repository mode must retain a blocking failure; runtime mode must make zero Git calls and must not present unavailable repository checks as verified passes. Real missing data or manifest members must still fail in both relevant contexts.
- [ ] Make the Git wrapper process-directory independent and catch launch failures:

```r
result <- tryCatch(
  system2("git", c("-C", shQuote(repo_root), args),
          stdout = TRUE, stderr = TRUE),
  error = function(e) structure(conditionMessage(e), status = 127L)
)
status <- attr(result, "status")
if (is.null(status)) status <- 0L
```

  Preserve the distinction between unavailable Git and an actual failed hygiene check. A `.git` path may be a file in a worktree, so runtime behaviour must be explicit rather than inferred solely with `dir.exists()`.
- [ ] Add an optional supplied confidence summary to `methodology_provenance_report()` so it does not recompute the checklist. Build the snapshot outside the Shiny server function, after data loading. Supply it to `methodologyPageServer()` and render/download from that same immutable value. Keep the expensive CSV inventory and static report body out of the download callback; add only the request timestamp there if required.
- [ ] Use invocation counters to test two sessions and repeated downloads. Require one snapshot construction per app process and no additional Git/CSV inventory work on each request. Also retain the static observer suspend/resume test; do not assert the original false premise that tab toggles currently rerun the output.
- [ ] Run repository checklist, a bundle-shaped temporary runtime fixture without `.git`, Methodology and provenance tests. Verify CI still blocks missing repository prerequisites. Benchmark locally as a regression signal only, not evidence of production DoS resistance.
- [ ] Commit this runtime/reporting change independently.

## Task 6: Missing components in score sensitivity

**Finding:** B9c. **Files:** modify `R/national_affordability_score.R`; extend `tests/test_national_affordability_score.R` and Methodology table tests.

**Interface:** each sensitivity scenario requires every positively weighted component. Missing input produces `NA_real_` for that scenario; intentional leave-one-out scenarios evaluate only the components they retain.

- [ ] Add a failing regression with two component scores of 50 and one NA. The default sensitivity must be NA, while the scenario excluding the missing component remains 50. Repeat with an omitted component row and an infinite component.
- [ ] Replace the weighted sum with an explicit completeness contract:

```r
weighted_score <- function(weights) {
  needed <- names(weights)[weights > 0]
  selected <- scores[needed]
  if (length(selected) != length(needed) || any(!is.finite(selected))) {
    return(NA_real_)
  }
  sum(selected * weights[needed])
}
```

- [ ] Verify all complete-input outputs are numerically unchanged, missing outputs display as unavailable, and headline score generation is unchanged. Run score and Methodology tests, then commit.

## Task 7: Unambiguous SIH quality joins

**Finding:** B9g. **Files:** modify `R/sih_quality_helpers.R` and `pipeline/06_validate_outputs.R`; extend `tests/test_sih_quality_helpers.R` and `tests/test_sih_estimate_quality.R`.

**Interface:** quality rows are grouped by `sih_quality_key_cols()` plus `quality_measure`. Identical duplicate payloads may collapse; differing values, units or reliability metadata raise an error naming the estimate key and source provenance. Do not alter SIH estimate schema.

- [ ] Add an order-independence regression. Use one estimate and two quality rows from different sources with RSE values 1.4 and 11.4. Both orders must fail with a conflict error rather than choosing a different value.
- [ ] Validate each group before reducing it; compare payload fields while retaining the source columns for diagnostics:

```r
payload <- intersect(c("quality_value", "quality_unit", "reliability_flag",
                       "reliability_note"), names(group))
if (nrow(unique(group[payload])) > 1L) {
  stop("Conflicting SIH quality metadata: ",
       paste(unique(paste(group$source_file, group$source_table, sep = ":")),
             collapse = ", "), call. = FALSE)
}
```

  Apply this to each reduced-key/measure group before the RSE/MOE joins. Include the estimate key in the final error. Stage 06 must apply the same conflict contract so a future cache cannot reach the app with ambiguous metadata.
- [ ] Test identical duplicates, conflicting RSE, conflicting MOE/unit, missing metadata and input-order permutations. Current 1,652 rows must continue to join unchanged. Run quality/uncertainty and pipeline tests, then commit.

## Final verification for the remediation wave

- [ ] Run the full suite from the repository root with `testthat::test_dir("tests", stop_on_failure = TRUE)` and the release checklist.
- [ ] Run the browser smoke test and manually inspect the serviceability subtitle, gapped-date chart behaviour, Methodology confidence display and provenance download.
- [ ] Confirm no official SIH cell values, scenario denominators, score weights or normalisation rules changed unintentionally.
- [ ] Regenerate the deployment manifest where required, run `graphify update .`, and inspect the final diff for unrelated changes and generated artefacts.
- [ ] Report exactly which tests and live/deployment checks ran, plus any remaining limitations. Do not claim a security vulnerability is fixed solely because caching was added.

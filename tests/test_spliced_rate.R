# Runs standalone via `Rscript tests/test_spliced_rate.R` (repo root) and under
# testthat::test_dir("tests"); see tests/helper-contracts.R.
if (!exists("contracts_harness_loaded", mode = "function")) {
  source(file.path(if (basename(getwd()) == "tests") "." else "tests",
                   "helper-contracts.R"))
}

test_that("spliced_rate contracts", {
  repo_root <- repo_root_path()


  source(file.path(repo_root, "R", "indicator_registry.R"), local = TRUE)

  check(exists("rba_new_loan_rate_spliced", mode = "function"),
        "rba_new_loan_rate_spliced() must be defined in R/indicator_registry.R")
  check(exists("INDICATOR_SOURCE_RBA_NEW_LOAN_RATE"),
        "INDICATOR_SOURCE_RBA_NEW_LOAN_RATE constant must be defined")

  if (exists("rba_new_loan_rate_spliced", mode = "function")) {
    # Synthetic fixture: advertised F5 sits a constant 1pp above actual F6, so
    # the wedge must be 1.0 and the level-adjusted pre-F6 history must align
    # exactly with the F6 segment.
    f5_dates <- seq(as.Date("2018-01-31"), as.Date("2022-12-31"), by = "month")
    f6_dates <- f5_dates[f5_dates >= as.Date("2019-07-01")]
    fixture <- rbind(
      data.frame(date = f5_dates, value = 4.0,
                 series = INDICATOR_SOURCE_RBA_MORTGAGE_RATE),
      data.frame(date = f6_dates, value = 3.0,
                 series = INDICATOR_SOURCE_RBA_NEW_LOAN_RATE)
    )

    spliced <- rba_new_loan_rate_spliced(fixture)
    wedge <- attr(spliced, "splice_wedge")

    check(isTRUE(all.equal(wedge, 1.0)),
          "Splice wedge must equal the constant F5-F6 gap in the fixture")
    check(isTRUE(all.equal(unique(spliced$value), 3.0)),
          "Level-adjusted history must align exactly with the F6 segment")
    check(identical(min(spliced$date), min(f5_dates)),
          "Spliced series must extend back to the start of the F5 history")
    check(identical(max(spliced$date), max(f6_dates)),
          "Spliced series must end at the latest F6 observation")
    check(!is.unsorted(spliced$date),
          "Spliced series must be ordered by date")

    # The F6 segment must be passed through untouched.
    f6_segment <- spliced[spliced$date >= min(f6_dates), , drop = FALSE]
    check(isTRUE(all.equal(f6_segment$value, rep(3.0, length(f6_dates)))),
          "F6 segment must be untouched actual rates")

    # Missing source series must fail loudly, not degrade silently.
    missing_f6 <- fixture[fixture$series == INDICATOR_SOURCE_RBA_MORTGAGE_RATE, ]
    loud <- tryCatch({
      rba_new_loan_rate_spliced(missing_f6)
      FALSE
    }, error = function(e) {
      grepl(INDICATOR_SOURCE_RBA_NEW_LOAN_RATE, conditionMessage(e), fixed = TRUE)
    })
    check(isTRUE(loud),
          "A missing F6 series must raise an error naming the missing series")

    # Insufficient overlap inside the fixed splice window must also fail loudly.
    short_overlap <- fixture[
      fixture$series == INDICATOR_SOURCE_RBA_MORTGAGE_RATE |
        fixture$date >= as.Date("2021-04-01"),
    ]
    loud_overlap <- tryCatch({
      rba_new_loan_rate_spliced(short_overlap)
      FALSE
    }, error = function(e) {
      grepl("overlap", conditionMessage(e), fixed = TRUE)
    })
    check(isTRUE(loud_overlap),
          "Insufficient F5/F6 overlap in the splice window must raise an error")

    # Live-data contract: the saved rba_rates.csv must support the splice with
    # history reaching the score window and an F6 pass-through segment.
    rba_path <- file.path(repo_root, "data", "rba_rates.csv")
    if (file.exists(rba_path)) {
      rba <- read.csv(rba_path, stringsAsFactors = FALSE)
      live <- tryCatch(rba_new_loan_rate_spliced(rba), error = function(e) e)
      check(!inherits(live, "error"),
            paste("Spliced rate must build from saved rba_rates.csv:",
                  if (inherits(live, "error")) conditionMessage(live) else ""))
      if (!inherits(live, "error")) {
        check(min(live$date) <= as.Date("2012-07-01"),
              "Spliced rate history must reach back to the score start date")
        live_wedge <- attr(live, "splice_wedge")
        check(is.numeric(live_wedge) && live_wedge > 0 && live_wedge < 3,
              "Live splice wedge must be a plausible positive advertised-vs-actual gap")
      }
    }
  }
})
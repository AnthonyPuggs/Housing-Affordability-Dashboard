# Runs standalone via `Rscript tests/test_derivation_helpers.R` (repo root) and under
# testthat::test_dir("tests"); see tests/helper-contracts.R.
if (!exists("contracts_harness_loaded", mode = "function")) {
  source(file.path(if (basename(getwd()) == "tests") "." else "tests",
                   "helper-contracts.R"))
}

test_that("derivation_helpers unit checks (hand-computed)", {
  repo_root <- repo_root_path()

  suppressPackageStartupMessages({
    library(dplyr)
    library(lubridate)
  })
  source(file.path(repo_root, "R", "derivation_helpers.R"), local = TRUE)

  q <- function(...) as.Date(c(...))

  # --- align_quarterly: monthly values average into their quarter -------------
  monthly <- data.frame(
    date = q("2024-01-31", "2024-02-29", "2024-03-31"),
    value = c(100, 110, 120)
  )
  quarterly <- data.frame(date = q("2024-01-01"), value = c(50))
  aligned <- align_quarterly(monthly, quarterly, "m", "q")
  check(nrow(aligned) == 1 && aligned$m == 110 && aligned$q == 50,
        "align_quarterly() must average monthly values within the quarter")

  # --- index_to_base ------------------------------------------------------------
  check(isTRUE(all.equal(index_to_base(c(50, 75, 100)), c(100, 150, 200))),
        "index_to_base() must rebase at the first value")
  check(all(is.na(index_to_base(c(0, 10)))),
        "index_to_base() must return NA when the base is zero")

  # --- get_series_exact: loud on every failure mode ------------------------------
  frame <- data.frame(
    date = q("2024-01-01", "2024-04-01"),
    value = c(1, 2),
    series = "Known"
  )
  ok <- get_series_exact(frame, "Known", dataset = "fixture")
  check(nrow(ok) == 2 && identical(names(ok), c("date", "value")),
        "get_series_exact() must return date/value for a clean series")
  check(grepl("missing required series",
              tryCatch(get_series_exact(frame, "Unknown", dataset = "fixture"),
                       error = function(e) conditionMessage(e)),
              fixed = TRUE),
        "get_series_exact() must fail loudly on a missing series")
  dup_frame <- rbind(frame, frame[1, ])
  check(grepl("duplicate dates",
              tryCatch(get_series_exact(dup_frame, "Known", dataset = "fixture"),
                       error = function(e) conditionMessage(e)),
              fixed = TRUE),
        "get_series_exact() must fail loudly on duplicate dates")
  check(grepl("expected at least",
              tryCatch(get_series_exact(frame, "Known", min_rows = 5,
                                        dataset = "fixture"),
                       error = function(e) conditionMessage(e)),
              fixed = TRUE),
        "get_series_exact() must enforce the minimum row count")

  # --- Price-to-Income Ratio ------------------------------------------------------
  # rppi 100 -> 110, wpi 100 -> 105: index ratio at q2 = 110/105*100.
  rppi <- data.frame(date = q("2024-01-01", "2024-04-01"), value = c(100, 110))
  wpi <- data.frame(date = q("2024-01-01", "2024-04-01"), value = c(100, 105))
  pti <- compute_price_to_income(rppi, wpi)
  check(isTRUE(all.equal(pti$value, c(100, 100 * 110 / 105),
                         tolerance = 1e-10)),
        "Price-to-Income must equal indexed price over indexed wages x 100")

  # --- Mortgage Serviceability Index (annuity P&I) --------------------------------
  # Mean price $500k, 80% LVR loan $400k, 30-year monthly annuity.
  # Hand-computed payments: 3% -> $1,686.41/month; 6% -> $2,398.20/month.
  # With constant WPI the index is the payment ratio: 142.21 (NOT 200 - the
  # interest-only v1 behaviour this formula replaced).
  price <- data.frame(date = q("2024-01-01", "2024-04-01"), value = c(500, 500))
  wpi_flat <- data.frame(date = q("2024-01-01", "2024-04-01"), value = c(100, 100))
  rate <- data.frame(date = q("2024-01-15", "2024-04-15"), value = c(3, 6))
  msi <- compute_mortgage_serviceability(price, wpi_flat, rate)
  check(isTRUE(all.equal(msi$monthly_pmt, c(1686.42, 2398.20),
                         tolerance = 1e-4)),
        "Annuity payments must match hand-computed 30-year P&I values")
  check(isTRUE(all.equal(msi$value, c(100, 142.2073), tolerance = 1e-4)),
        "MSI must index the annuity repayment burden (rate doubling raises it ~42%, not 100%)")

  # --- Rental Affordability Index ---------------------------------------------------
  rents <- data.frame(date = q("2024-01-01", "2024-04-01"), value = c(100, 120))
  wpi_r <- data.frame(date = q("2024-01-01", "2024-04-01"), value = c(100, 110))
  rai <- compute_rental_affordability(rents, wpi_r)
  check(isTRUE(all.equal(rai$value, c(100, 100 * 120 / 110), tolerance = 1e-10)),
        "RAI must equal indexed rents over indexed wages x 100")

  # --- Deposit Gap --------------------------------------------------------------------
  # $1,000k mean price -> $200k deposit; AWE $2,000/wk -> $104k income,
  # 15% saved = $15.6k/yr -> 12.8205 years.
  price_dg <- data.frame(date = q("2024-01-01"), value = 1000)
  awe <- data.frame(date = q("2024-01-01"), value = 2000)
  dg <- compute_deposit_gap(price_dg, awe)
  check(isTRUE(all.equal(dg$value, 200000 / 15600, tolerance = 1e-10)),
        "Deposit gap must be deposit dollars over annual savings (12.82 years)")

  # --- Real growth YoY ----------------------------------------------------------------
  dates5 <- q("2023-01-01", "2023-04-01", "2023-07-01", "2023-10-01",
              "2024-01-01")
  series5 <- data.frame(date = dates5, value = c(100, 102, 104, 106, 110))
  cpi_flat <- data.frame(date = dates5, value = rep(100, 5))
  ryoy <- compute_real_growth_yoy(series5, cpi_flat)
  check(nrow(ryoy) == 1 && isTRUE(all.equal(ryoy$value, 10, tolerance = 1e-10)),
        "Real YoY growth must be the four-quarter change of the deflated level")

  # --- Real mortgage rate -----------------------------------------------------------
  rate_m <- data.frame(
    date = q("2024-01-31", "2024-02-29", "2024-03-31"),
    value = c(6, 6, 6)
  )
  infl_q <- data.frame(date = q("2024-01-01"), value = 2.5)
  rmr <- compute_real_mortgage_rate(rate_m, infl_q)
  check(nrow(rmr) == 1 && isTRUE(all.equal(rmr$value, 3.5, tolerance = 1e-10)),
        "Real mortgage rate must be the nominal quarterly mean minus inflation")
})

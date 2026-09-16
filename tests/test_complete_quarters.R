if (!exists("contracts_harness_loaded", mode = "function")) {
  source(file.path(if (basename(getwd()) == "tests") "." else "tests", "helper-contracts.R"))
}
test_that("monthly quarters require three distinct finite months", {
  suppressPackageStartupMessages({library(dplyr);library(lubridate)})
  source(file.path(repo_root_path(), "R", "derivation_helpers.R"), local = TRUE)
  d <- data.frame(date = as.Date(c("2024-01-01", "2024-02-01", "2024-03-01")), value = c(10, NA, 30))
  expect_equal(nrow(complete_quarter_mean(d)), 0L)
  d$value[2] <- Inf
  expect_equal(nrow(complete_quarter_mean(d)), 0L)
  d$value[2] <- 20
  expect_equal(complete_quarter_mean(d, expected_months = 3L)$value, 20)
  expect_equal(nrow(complete_quarter_mean(d[1, ], expected_months = 3L)), 0L)
  d$date[2] <- d$date[1]
  expect_error(complete_quarter_mean(d, expected_months = 3L), "duplicate")
  empty <- complete_quarter_mean(d[FALSE, ], "rate", expected_months = 3L)
  expect_equal(names(empty), c("date", "rate"))
  expect_s3_class(empty$date, "Date")
  expect_equal(nrow(empty), 0L)
  expect_equal(complete_quarter_mean(d[1, ], expected_months = 1L)$value, 10)
})
test_that("app drops incomplete rate quarters while retaining AWE observation quarters", {
  use_fixture_data()
  source(file.path(repo_root_path(), "plot_setup.R"), local = TRUE)
  latest <- max(serviceability_ts$date)
  short <- rba_rates[!(lubridate::floor_date(rba_rates$date, "quarter") == latest &
                        lubridate::month(rba_rates$date) %% 3 == 0), ]
  p <- precompute_dashboard_series(abs_ts, short, afford_idx)
  expect_false(latest %in% p$serviceability_ts$date)
  expect_true(latest %in% p$awe_ts$date)
})

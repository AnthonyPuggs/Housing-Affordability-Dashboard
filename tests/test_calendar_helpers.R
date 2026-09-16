if (!exists("contracts_harness_loaded", mode = "function")) {
  source(file.path(if (basename(getwd()) == "tests") "." else "tests", "helper-contracts.R"))
}

test_that("calendar comparisons preserve gaps and reject ambiguous dates", {
  suppressPackageStartupMessages({ library(dplyr); library(lubridate) })
  path <- file.path(repo_root_path(), "R", "calendar_helpers.R")
  if (file.exists(path)) source(path, local = TRUE)
  source(file.path(repo_root_path(), "R", "dashboard_formatting.R"), local = TRUE)
  source(file.path(repo_root_path(), "R", "derivation_helpers.R"), local = TRUE)
  source(file.path(repo_root_path(), "R", "chart_builders.R"), local = TRUE)
  d <- data.frame(date = as.Date(c("2024-01-01", "2024-04-01", "2024-07-01",
                                  "2024-10-01", "2025-04-01")),
                  value = c(100, 110, 120, 130, 150), series = "x", city = "x")
  expect_equal(latest_change(d, "series", "x")$change, 100 * (150 / 110 - 1))
  expect_equal(latest_change(d[-2, ], "series", "x")$label, "")
  expect_equal(tail(compute_real_growth_yoy(d[, 1:2], transform(d[, 1:2], value = 100))$value, 1),
               100 * (150 / 110 - 1))
  expect_equal(tail(price_series_transform(d, "yoy")$value, 1), 100 * (150 / 110 - 1))
  expect_equal(tail(rent_cpi_series_transform(d, "yoy")$value, 1), 100 * (150 / 110 - 1))
  expect_false(as.Date("2025-04-01") %in% rent_cpi_series_transform(d, "qoq")$date)
  expect_error(latest_change(rbind(d, d[1, ]), "series", "x"), "unique")
  expect_equal(calendar_prior_values(as.Date(c("2023-02-28", "2024-02-29")), c(10, 20)),
               c(NA_real_, 10))
  monthly <- data.frame(date = as.Date(c("2024-02-29", "2025-02-28")), value = c(100, 110), series = "m")
  expect_equal(latest_change(monthly, "series", "m")$change, 10)
  half <- data.frame(date = as.Date(c("2024-10-01", "2025-04-01", "2025-10-01")), value = c(0, 1, 2), series = "h")
  expect_equal(latest_change(half, "series", "h", change_type = "percentage_points")$change, 2)
  expect_equal(latest_change(half[c(3, 1, 2), ], "series", "h", change_type = "percentage_points")$change, 2)
})

test_that("annual migration totals require four consecutive quarters", {
  path <- file.path(repo_root_path(), "R", "calendar_helpers.R")
  if (file.exists(path)) source(path, local = TRUE)
  d <- data.frame(date = seq(as.Date("2023-01-01"), by = "quarter", length.out = 8), value = 1:8)
  expect_equal(calendar_quarter_total(d$date, d$value), 26)
  expect_equal(calendar_quarter_total(d$date, d$value, end_date = as.Date("2023-10-01")), 10)
  expect_true(is.na(calendar_quarter_total(d$date[-6], d$value[-6])))
})
test_that("chart date windows retain the history needed for calendar growth", {
  use_fixture_data()
  suppressPackageStartupMessages({library(shiny); library(plotly)})
  source(file.path(repo_root_path(), "plot_setup.R"), local = TRUE)
  source(file.path(repo_root_path(), "R", "chart_builders.R"), local = TRUE)
  source(file.path(repo_root_path(), "R", "price_trends_module.R"), local = TRUE)
  rppi_combined <- data.frame(date = as.Date(c("2024-01-01", "2025-01-01")),
                              value = c(100, 120), city = "Australia",
                              dwelling_type = "state_mean")
  rent_cpi_combined <- data.frame(date = as.Date(c("2024-10-01", "2025-01-01")),
                                  value = c(100, 110), city = rent_cpi_national_city)
  testServer(priceTrendsPageServer, args = list(is_dark = reactive(FALSE)), {
    session$setInputs(price_cities = "Australia", price_dwelling = "state_mean",
                      price_dates = as.Date(c("2025-01-01", "2025-01-01")),
                      price_transform = "yoy", rent_cpi_view = "national",
                      rent_cpi_dates = as.Date(c("2025-01-01", "2025-01-01")),
                      rent_cpi_datatype = "qoq")
    expect_equal(price_data()$value, 20)
    expect_equal(rent_cpi_data()$value, 10)
    expect_equal(price_data()$date, as.Date("2025-01-01"))
    session$setInputs(price_transform = "index")
    expect_equal(price_data()$value, 100)
  })
})

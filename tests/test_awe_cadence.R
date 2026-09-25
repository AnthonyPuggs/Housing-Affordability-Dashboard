if (!exists("contracts_harness_loaded", mode = "function")) {
  source(file.path(if (basename(getwd()) == "tests") "." else "tests", "helper-contracts.R"))
}
test_that("AWE cadence preserves its historical transition without inferring gaps", {
  source(file.path(repo_root_path(), "R", "indicator_registry.R"), local = TRUE)
  expect_equal(awe_observation_frequency(as.Date(c("2012-05-15", "2012-11-15", "2025-11-15"))),
               c("Quarter", "Half-year", "Half-year"))
  expect_equal(awe_aligned_frequency(as.Date(c("2012-04-01", "2012-10-01", "2025-10-01"))),
               c("Quarter", "Half-year", "Half-year"))
  expect_error(validate_awe_cadence(data.frame(date = as.Date("2025-08-15"), frequency = "Half-year")), "May/November")
  expect_error(validate_awe_cadence(data.frame(date = as.Date("2025-05-15"), frequency = "Quarter")), "frequency")
  expect_silent(validate_awe_cadence(data.frame(date = as.Date("2025-11-15"), frequency = "Half-year")))
})
test_that("score frequency follows AWE-aligned periods without changing the score", {
  source(file.path(repo_root_path(), "R", "national_affordability_score.R"), local = TRUE)
  dates <- as.Date(c("2024-04-01", "2024-10-01", "2025-04-01"))
  d <- expand.grid(date = dates, indicator = national_affordability_score_inputs()$input_indicator)
  d$value <- rep(c(1, 2, 3), 3)
  score <- calculate_national_affordability_score(d)
  expect_true(all(score$frequency == "Half-year"))
  expect_equal(score$value[score$indicator == "National Housing Affordability Score"], c(100, 50, 0))
})

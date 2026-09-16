if (!exists("contracts_harness_loaded", mode = "function")) {
  source(file.path(if (basename(getwd()) == "tests") "." else "tests", "helper-contracts.R"))
}
test_that("sensitivity scenarios require every included score", {
  source(file.path(repo_root_path(), "R", "national_affordability_score.R"), local = TRUE)
  x <- data.frame(component = national_affordability_score_weights()$component, score = c(50, 50, NA_real_))
  for (bad in list(x, x[1:2, ], transform(x, score = c(50, 50, Inf)),
                  transform(x, score = c(50, 50, -Inf)))) {
    d <- national_affordability_score_sensitivity(bad)
    expect_true(is.na(d$score[d$scenario == "default_40_35_25"]))
    expect_true(is.na(d$score[d$scenario == "geometric_default"]))
    expect_equal(d$score[d$scenario == "leave_out_deposit_barrier"], 50)
  }
})

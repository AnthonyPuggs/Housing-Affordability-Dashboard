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
test_that("conflicting survey metadata fails regardless of source row order", {
  source(file.path(repo_root_path(), "R", "sih_quality_helpers.R"), local = TRUE)
  q <- read.csv(file.path(repo_root_path(), "tests/fixtures/data/sih_estimate_quality.csv"), stringsAsFactors = FALSE)
  q <- q[q$quality_measure == "rse_pct", ][1, ]
  estimate <- q[sih_quality_key_cols()]; estimate$value <- 100
  other <- q; other$source_file <- "conflicting-source.xlsx"; other$quality_value <- q$quality_value + 10
  expect_error(join_sih_quality(estimate, rbind(q, other)), "Conflicting SIH quality")
  expect_error(join_sih_quality(estimate, rbind(other, q)), "Conflicting SIH quality")
  other$quality_value <- q$quality_value
  expect_equal(join_sih_quality(estimate, rbind(q, other))$rse_pct, q$quality_value)
  other$quality_unit <- "incompatible unit"
  expect_error(join_sih_quality(estimate, rbind(q, other)), "Conflicting SIH quality")
})

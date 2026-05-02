repo_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
failures <- character()

check <- function(condition, message) {
  if (!isTRUE(condition)) {
    failures <<- c(failures, message)
  }
}

helper_path <- file.path(repo_root, "R", "sih_quality_helpers.R")
quality_path <- file.path(repo_root, "data", "sih_estimate_quality.csv")
nhha_path <- file.path(repo_root, "data", "sih_nhha_rental_stress.csv")
stress_path <- file.path(repo_root, "data", "sih_stress_bands_2020.csv")
lower_income_path <- file.path(repo_root, "data", "sih_lower_income_states.csv")

check(file.exists(helper_path), "R/sih_quality_helpers.R does not exist")
check(file.exists(quality_path), "data/sih_estimate_quality.csv does not exist")
check(file.exists(nhha_path), "data/sih_nhha_rental_stress.csv does not exist")
check(file.exists(stress_path), "data/sih_stress_bands_2020.csv does not exist")
check(file.exists(lower_income_path), "data/sih_lower_income_states.csv does not exist")

if (file.exists(helper_path)) {
  parsed <- tryCatch({
    parse(helper_path)
    TRUE
  }, error = function(e) conditionMessage(e))
  check(identical(parsed, TRUE),
        paste(helper_path, "does not parse:", parsed))
}

if (file.exists(helper_path)) {
  source(helper_path)
  check(exists("sih_quality_interval_bounds", mode = "function"),
        "sih_quality_interval_bounds() must be defined")
}

if (file.exists(helper_path) &&
    exists("sih_quality_interval_bounds", mode = "function")) {
  bounds <- sih_quality_interval_bounds(40, 2.5, "percentage_points")
  check(identical(bounds$estimate_lower_95, 37.5),
        "40 minus 2.5 percentage points should be 37.5")
  check(identical(bounds$estimate_upper_95, 42.5),
        "40 plus 2.5 percentage points should be 42.5")
  check(grepl("95% MOE", bounds$interval_label, fixed = TRUE),
        "Interval label should mention 95% MOE")

  capped <- sih_quality_interval_bounds(
    value = c(1, 99),
    moe_95 = c(5, 5),
    quality_unit = c("percentage_points", "percentage_points")
  )
  check(identical(capped$estimate_lower_95, c(0, 94)),
        "Interval lower bounds should cap at 0")
  check(identical(capped$estimate_upper_95, c(6, 100)),
        "Interval upper bounds should cap at 100")

  missing_moe <- sih_quality_interval_bounds(40, NA_real_, "percentage_points")
  check(is.na(missing_moe$estimate_lower_95),
        "Missing MOE should not produce a lower interval bound")
  check(is.na(missing_moe$estimate_upper_95),
        "Missing MOE should not produce an upper interval bound")
  check(identical(missing_moe$interval_label, ""),
        "Missing MOE should produce a blank interval label")

  non_pp <- sih_quality_interval_bounds(40, 2.5, "per_cent")
  check(is.na(non_pp$estimate_lower_95),
        "Non-percentage-point units should not produce interval bounds")
  check(is.na(non_pp$estimate_upper_95),
        "Non-percentage-point units should not produce interval bounds")
  check(identical(non_pp$interval_label, ""),
        "Non-percentage-point units should produce a blank interval label")
}

if (all(file.exists(c(helper_path, quality_path, nhha_path,
                      stress_path, lower_income_path)))) {
  suppressPackageStartupMessages({
    library(dplyr)
    library(readr)
  })

  source(helper_path)
  sih_quality <- read_csv(quality_path, show_col_types = FALSE)
  nhha <- read_csv(nhha_path, show_col_types = FALSE)
  sih_stress <- read_csv(stress_path, show_col_types = FALSE)
  lower_income <- read_csv(lower_income_path, show_col_types = FALSE)

  key_cols <- c("survey_year", "metric", "tenure", "breakdown_var",
                "breakdown_val", "geography", "stat_type")

  quality_moe_keys <- sih_quality %>%
    filter(quality_measure == "moe_95",
           quality_unit == "percentage_points") %>%
    select(all_of(key_cols))

  nhha_moe <- nhha %>% semi_join(quality_moe_keys, by = key_cols)
  stress_moe <- sih_stress %>% semi_join(quality_moe_keys, by = key_cols)
  lower_moe <- lower_income %>% semi_join(quality_moe_keys, by = key_cols)

  check(nrow(nhha_moe) > 0, "Expected NHHA rows with 95% MOE metadata")
  check(nrow(stress_moe) > 0, "Expected File 5 stress rows with 95% MOE metadata")
  check(nrow(lower_moe) > 0, "Expected File 8 lower-income rows with 95% MOE metadata")

  for (sample_data in list(nhha_moe[1, ], stress_moe[1, ], lower_moe[1, ])) {
    joined <- join_sih_quality(sample_data, sih_quality)
    check(nrow(joined) == nrow(sample_data),
          "join_sih_quality() must not duplicate MOE estimate rows")
    check("estimate_lower_95" %in% names(joined),
          "join_sih_quality() must add estimate_lower_95")
    check("estimate_upper_95" %in% names(joined),
          "join_sih_quality() must add estimate_upper_95")
    check("interval_label" %in% names(joined),
          "join_sih_quality() must add interval_label")
    check(!is.na(joined$estimate_lower_95[1]),
          "MOE rows should receive a lower 95% interval bound")
    check(!is.na(joined$estimate_upper_95[1]),
          "MOE rows should receive an upper 95% interval bound")
    check(grepl("95% MOE", joined$interval_label[1], fixed = TRUE),
          "MOE rows should receive interval hover text")
  }

  rse_only_key <- sih_quality %>%
    filter(quality_measure == "rse_pct") %>%
    anti_join(quality_moe_keys, by = key_cols) %>%
    select(all_of(key_cols)) %>%
    distinct() %>%
    slice_head(n = 1)
  check(nrow(rse_only_key) > 0,
        "Expected at least one RSE-only quality record")

  if (nrow(rse_only_key) > 0) {
    rse_only_row <- bind_rows(nhha, sih_stress, lower_income) %>%
      semi_join(rse_only_key, by = key_cols) %>%
      slice_head(n = 1)
    check(nrow(rse_only_row) == 1,
          "Expected an estimate row matching the RSE-only quality record")
    joined_rse_only <- join_sih_quality(rse_only_row, sih_quality)
    check(!is.na(joined_rse_only$rse_pct[1]),
          "RSE-only rows should retain RSE metadata")
    check(is.na(joined_rse_only$estimate_lower_95[1]),
          "RSE-only rows must not fabricate lower interval bounds")
    check(is.na(joined_rse_only$estimate_upper_95[1]),
          "RSE-only rows must not fabricate upper interval bounds")
    check(identical(joined_rse_only$interval_label[1], ""),
          "RSE-only rows should have a blank interval label")
  }
}

if (length(failures) > 0) {
  stop(
    paste(c("SIH uncertainty interval checks failed:", paste0("- ", failures)),
          collapse = "\n"),
    call. = FALSE
  )
}

cat("SIH uncertainty interval checks passed.\n")

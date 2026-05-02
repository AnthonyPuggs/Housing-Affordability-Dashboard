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

check(file.exists(helper_path), "R/sih_quality_helpers.R does not exist")
check(file.exists(quality_path), "data/sih_estimate_quality.csv does not exist")
check(file.exists(nhha_path), "data/sih_nhha_rental_stress.csv does not exist")

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

  required_functions <- c(
    "join_sih_quality",
    "sih_quality_hover_text",
    "sih_reliability_marker"
  )
  missing_functions <- required_functions[
    !vapply(required_functions, exists, logical(1), mode = "function")
  ]
  check(length(missing_functions) == 0,
        paste("Missing SIH quality helper functions:",
              paste(missing_functions, collapse = ", ")))
}

if (all(file.exists(c(helper_path, quality_path, nhha_path)))) {
  suppressPackageStartupMessages({
    library(dplyr)
    library(readr)
  })

  source(helper_path)
  sih_quality <- read_csv(quality_path, show_col_types = FALSE)
  nhha <- read_csv(nhha_path, show_col_types = FALSE)

  key_cols <- c("survey_year", "metric", "tenure", "breakdown_var",
                "breakdown_val", "geography", "stat_type")

  rse_quality <- sih_quality %>%
    filter(quality_measure == "rse_pct")

  known_caution <- nhha %>%
    inner_join(
      rse_quality %>%
        filter(reliability_flag == "use_with_caution") %>%
        select(all_of(key_cols), quality_value, reliability_flag),
      by = key_cols
    )

  check(nrow(known_caution) > 0,
        "Expected at least one NHHA row with an RSE caution record")

  if (nrow(known_caution) > 0) {
    sample_row <- known_caution[1, names(nhha)]
    joined <- join_sih_quality(sample_row, sih_quality)

    check(nrow(joined) == nrow(sample_row),
          "join_sih_quality() must not duplicate estimate rows")
    check("rse_pct" %in% names(joined),
          "join_sih_quality() must add rse_pct")
    check("moe_95" %in% names(joined),
          "join_sih_quality() must add moe_95")
    check("quality_hover" %in% names(joined),
          "join_sih_quality() must add quality_hover")
    check("reliability_marker" %in% names(joined),
          "join_sih_quality() must add reliability_marker")
    check(identical(joined$rse_reliability_flag[1], "use_with_caution"),
          "Known caution row must retain use_with_caution reliability flag")
    check(identical(joined$reliability_marker[1], "\u2020"),
          "use_with_caution rows should receive a dagger marker")
    check(grepl("RSE", joined$quality_hover[1], fixed = TRUE),
          "Known caution hover text must include RSE")
    check(grepl("interpret with caution", joined$quality_hover[1],
                fixed = TRUE),
          "Known caution hover text must include caution wording")
  }

  unmatched <- nhha[1, ]
  unmatched$survey_year <- "2099-00"
  joined_unmatched <- join_sih_quality(unmatched, sih_quality)

  check(nrow(joined_unmatched) == 1,
        "Unmatched estimate rows should remain valid")
  check(identical(joined_unmatched$reliability_marker[1], ""),
        "Unmatched estimate rows should not receive a reliability marker")
  check(grepl("not available", joined_unmatched$quality_hover[1], fixed = TRUE),
        "Unmatched estimate rows should explain that quality metadata is unavailable")
}

if (length(failures) > 0) {
  stop(
    paste(c("SIH quality helper checks failed:", paste0("- ", failures)),
          collapse = "\n"),
    call. = FALSE
  )
}

cat("SIH quality helper checks passed.\n")

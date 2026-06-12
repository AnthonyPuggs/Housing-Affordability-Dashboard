# ==============================================================================
# 01_process_sih.R — Parse SIH Excel workbooks into clean CSVs
# ==============================================================================
# Input:  resources/ABS_data/housing_occupancy_and_costs_SIH/*.xlsx
# Output: data/sih_*.csv
#
# All output CSVs use long format with standard columns:
#   survey_year | value | metric | tenure | breakdown_var | breakdown_val |
#   geography | stat_type
# ==============================================================================

cat("--- Processing SIH workbooks ---\n")

# Header-anchored layout engine (read_sheet_raw, anchor_columns,
# find_block_bounds, sih_parse_columns_down, sih_assert*).
source(project_path("pipeline", "sih_layouts.R"))

# --- File paths ---------------------------------------------------------------
sih_files <- list(
  f1  = list.files(SIH_DIR, pattern = "^1\\.", full.names = TRUE),
  f3  = list.files(SIH_DIR, pattern = "^3\\.", full.names = TRUE),
  f4  = list.files(SIH_DIR, pattern = "^4\\.", full.names = TRUE),
  f5  = list.files(SIH_DIR, pattern = "^5\\.", full.names = TRUE),
  f6  = list.files(SIH_DIR, pattern = "^6\\.", full.names = TRUE),
  f8  = list.files(SIH_DIR, pattern = "^8\\.", full.names = TRUE),
  f9  = list.files(SIH_DIR, pattern = "^9\\.", full.names = TRUE),
  f11 = list.files(SIH_DIR, pattern = "^11\\.", full.names = TRUE),
  f12 = list.files(SIH_DIR, pattern = "^12\\.", full.names = TRUE),
  f13 = list.files(SIH_DIR, pattern = "^13\\.", full.names = TRUE)
)

# Verify all files exist
missing <- names(sih_files)[sapply(sih_files, function(x) length(x) == 0)]
if (length(missing) > 0) {
  warning("Missing SIH files: ", paste(missing, collapse = ", "))
}

# --- Survey year columns (shared across File 1 and File 12) -------------------
SURVEY_YEARS <- c("1994-95", "1995-96", "1996-97", "1997-98", "1999-00",
                  "2000-01", "2002-03", "2003-04", "2005-06", "2007-08",
                  "2009-10", "2011-12", "2013-14", "2015-16", "2017-18",
                  "2019-20")

# --- Generic parser for File 1 / File 12 time-series tables -------------------
#' Parse a time-series SIH table (years across columns, Files 1 and 12)
#' Survey-year columns are anchored to the year header band above the
#' ESTIMATES marker (en-dashes and footnote markers normalised away) and
#' block bounding keeps the RSE/MOE sub-blocks out of the estimates.
parse_timeseries_table <- function(file, sheet, metric,
                                   geography = "National") {
  emit <- function(label, unit, section, subsection, years, values) {
    tibble(
      survey_year   = years,
      value         = values,
      metric        = metric,
      tenure        = classify_tenure(label),
      breakdown_var = ifelse(is.na(section), "tenure", simplify_section(section)),
      breakdown_val = str_trim(str_remove(label, "\\([a-z]\\)$")),
      geography     = geography,
      stat_type     = ifelse(str_detect(metric, "[Mm]edian"), "median", "mean"),
      .subsection   = subsection
    )
  }

  # Footnote-stripped labels can collide within a section (e.g. "Total (c)"
  # under "One family households" and "Total (d)" under "Non-family
  # households" both become "Total" under family_type). Qualify only the
  # colliding keys with their subsection so every section total stays a
  # distinct observation and untouched keys keep their legacy form.
  disambiguate_subsection_totals <- function(out) {
    if (nrow(out) == 0) return(out)
    keys <- out[intersect(SIH_ESTIMATE_KEY, names(out))]
    dup <- duplicated(keys) | duplicated(keys, fromLast = TRUE)
    fixable <- dup & !is.na(out$.subsection)
    out$breakdown_val[fixable] <- paste(
      out$breakdown_val[fixable],
      str_trim(str_remove(out$.subsection[fixable], "\\s*\\([a-z]\\)$")),
      sep = " | "
    )
    out$.subsection <- NULL
    out
  }

  sih_parse_years_across(
    file, sheet,
    emit = emit,
    label_skip_pattern = "^(ESTIMATES|RELATIVE STANDARD ERRORS|Source|Exclud)",
    section_pattern = "(Tenure|Family|Gross|Equivalised|Main source|Government|Age|Weekly|Housing)",
    post_process = disambiguate_subsection_totals
  )
}


# --- Tenure classifier --------------------------------------------------------
classify_tenure <- function(label) {
  label <- str_trim(label)
  case_when(
    str_detect(label, regex("owner without", ignore_case = TRUE)) ~ "owner_outright",
    str_detect(label, regex("owner with", ignore_case = TRUE))    ~ "owner_mortgage",
    str_detect(label, regex("total owners", ignore_case = TRUE))  ~ "owner_total",
    str_detect(label, regex("state or territory|public", ignore_case = TRUE)) ~ "renter_social",
    str_detect(label, regex("private landlord", ignore_case = TRUE)) ~ "renter_private",
    str_detect(label, regex("total renters", ignore_case = TRUE)) ~ "renter_total",
    str_detect(label, regex("^total\\b", ignore_case = TRUE))     ~ "all",
    str_detect(label, regex("^all house", ignore_case = TRUE))    ~ "all",
    str_detect(label, regex("^renter$", ignore_case = TRUE))      ~ "renter_total",
    str_detect(label, regex("^owner$", ignore_case = TRUE))       ~ "owner_total",
    TRUE ~ "all"
  )
}

# --- Section name simplifier --------------------------------------------------
simplify_section <- function(section) {
  if (is.na(section)) return("tenure")
  section <- str_trim(section)
  case_when(
    str_detect(section, regex("tenure", ignore_case = TRUE))        ~ "tenure",
    str_detect(section, regex("family", ignore_case = TRUE))        ~ "family_type",
    str_detect(section, regex("gross.*income.*quintile|income quintile", ignore_case = TRUE)) ~ "income_quintile",
    str_detect(section, regex("equivalised", ignore_case = TRUE))   ~ "equiv_income_quintile",
    str_detect(section, regex("main source", ignore_case = TRUE))   ~ "income_source",
    str_detect(section, regex("government", ignore_case = TRUE))    ~ "govt_payment",
    str_detect(section, regex("age", ignore_case = TRUE))           ~ "age_group",
    str_detect(section, regex("weekly", ignore_case = TRUE))        ~ "weekly_income",
    str_detect(section, regex("housing", ignore_case = TRUE))       ~ "housing_type",
    TRUE ~ str_to_lower(str_replace_all(section, "\\s+", "_"))
  )
}

# --- SIH sampling-error metadata helpers --------------------------------------
sih_estimate_quality_columns <- c(
  "source_file",
  "source_table",
  "survey_year",
  "metric",
  "tenure",
  "breakdown_var",
  "breakdown_val",
  "geography",
  "stat_type",
  "quality_measure",
  "quality_value",
  "quality_unit",
  "reliability_flag",
  "reliability_note"
)

empty_sih_estimate_quality <- function() {
  tibble(
    source_file = character(),
    source_table = character(),
    survey_year = character(),
    metric = character(),
    tenure = character(),
    breakdown_var = character(),
    breakdown_val = character(),
    geography = character(),
    stat_type = character(),
    quality_measure = character(),
    quality_value = numeric(),
    quality_unit = character(),
    reliability_flag = character(),
    reliability_note = character()
  )
}

classify_quality_marker <- function(marker) {
  marker <- str_to_lower(str_squish(as.character(marker)))
  case_when(
    str_detect(marker, "moe") ~ "moe_95",
    str_detect(marker, "rse") ~ "rse_pct",
    TRUE ~ NA_character_
  )
}

quality_unit_for <- function(quality_measure) {
  case_when(
    identical(quality_measure, "moe_95") ~ "percentage_points",
    identical(quality_measure, "rse_pct") ~ "per_cent",
    TRUE ~ NA_character_
  )
}

reliability_flag_for <- function(quality_measure, value) {
  if (identical(quality_measure, "rse_pct")) {
    if (value > 50) return("too_unreliable")
    if (value >= 25) return("use_with_caution")
  }
  "standard"
}

reliability_note_for <- function(quality_measure, value) {
  flag <- reliability_flag_for(quality_measure, value)
  if (identical(quality_measure, "moe_95")) {
    return("95% margin of error for a proportion estimate, in percentage points.")
  }
  if (identical(flag, "too_unreliable")) {
    return("Relative standard error is greater than 50%; ABS considers the estimate too unreliable for general use.")
  }
  if (identical(flag, "use_with_caution")) {
    return("Relative standard error is 25% to 50%; ABS advises users to interpret with caution.")
  }
  "Relative standard error is below 25%; no high-RSE caution flag is applied."
}

quality_row <- function(source_file, source_table, survey_year, metric, tenure,
                        breakdown_var, breakdown_val, geography, stat_type,
                        quality_measure, quality_value) {
  tibble(
    source_file = basename(source_file),
    source_table = source_table,
    survey_year = survey_year,
    metric = metric,
    tenure = tenure,
    breakdown_var = breakdown_var,
    breakdown_val = breakdown_val,
    geography = geography,
    stat_type = stat_type,
    quality_measure = quality_measure,
    quality_value = quality_value,
    quality_unit = quality_unit_for(quality_measure),
    reliability_flag = reliability_flag_for(quality_measure, quality_value),
    reliability_note = reliability_note_for(quality_measure, quality_value)
  )
}

classify_nhha_section <- function(section) {
  if (is.na(section) || section == "") {
    return(list(metric = NA_character_, stat_type = NA_character_))
  }

  section_norm <- section %>%
    str_remove("\\s*\\([a-z]\\)$") %>%
    str_to_lower() %>%
    str_squish()

  if (str_detect(section_norm, "^proportion of lower income renter households paying more than 30%")) {
    return(list(metric = "pct_rental_stress_over_30", stat_type = "proportion"))
  }
  if (str_detect(section_norm, "^number of lower income renter households paying more than 30%")) {
    return(list(metric = "number_rental_stress_over_30", stat_type = "count"))
  }
  if (str_detect(section_norm, "^number of lower income renter households$")) {
    return(list(metric = "number_lower_income_renter_households", stat_type = "count"))
  }

  list(metric = NA_character_, stat_type = NA_character_)
}

# Stress-band column spec shared by the File 5 estimates and quality parsers.
sih_f5_band_spec <- list(
  sih_col("pct_25_or_less", "^25 % or less$"),
  sih_col("pct_25_to_30",   "^More than 25% to 30%$"),
  sih_col("pct_30_to_50",   "^More than 30% to 50%$"),
  sih_col("pct_over_50",    "^More than 50%$"),
  sih_col("pct_total",      "^Total$"),
  sih_col("pct_over_30",    "^More than 30%$"),
  sih_col("households_000", "^All households$")
)

parse_stress_bands_quality <- function(file, sheet, population_label) {
  raw <- read_sheet_raw(file, sheet)
  est_row <- require_label_row(raw, "^ESTIMATES", file, sheet,
                               "ESTIMATES block marker")
  cols <- anchor_columns(raw, est_row - 1L, sih_f5_band_spec, file, sheet)
  bounds <- find_block_bounds(raw, file, sheet,
                              start_pattern = "^95% margin of error",
                              stop_pattern = "^relative standard error",
                              what = "95% margin of error block")

  results <- list()
  current_section <- NA_character_
  quality_markers <- rep(NA_character_, length(cols))

  for (i in bounds[["first"]]:bounds[["last"]]) {
    row <- raw[i, ]
    label <- str_trim(as.character(row[[1]]))
    vals <- vapply(cols, function(k) as.character(row[[k]]), character(1))
    marker_row <- vapply(vals, classify_quality_marker, character(1))

    if (is.na(label) || label == "" || label == "NA") {
      if (any(!is.na(marker_row))) {
        quality_markers[seq_along(marker_row)] <- marker_row
      }
      next
    }
    if (str_detect(label, "^(Source|Exclud|NA|#|Cells|\\([a-z]\\)|np not)")) next

    if (all(is.na(quality_markers)) && any(!is.na(marker_row))) {
      quality_markers[seq_along(marker_row)] <- marker_row
      next
    }

    has_data <- any(!is.na(suppressWarnings(as.numeric(clean_abs_values(vals)))))
    if (!has_data) {
      current_section <- label
      next
    }

    numeric_vals <- as_numeric_clean(vals)
    for (j in seq_along(cols)) {
      quality_measure <- quality_markers[j]
      if (is.na(quality_measure) || is.na(numeric_vals[j])) next

      results[[length(results) + 1]] <- quality_row(
        source_file = file,
        source_table = sheet,
        survey_year = "2019-20",
        metric = names(cols)[j],
        tenure = classify_tenure(label),
        breakdown_var = ifelse(is.na(current_section), "tenure",
                               simplify_section(current_section)),
        breakdown_val = str_trim(str_remove(label, "\\s*\\([a-z]\\)$")),
        geography = "National",
        stat_type = population_label,
        quality_measure = quality_measure,
        quality_value = numeric_vals[j]
      )
    }
  }

  out <- if (length(results) == 0) empty_sih_estimate_quality() else bind_rows(results)
  sih_assert(nrow(out) > 0, file, sheet, "no quality rows parsed")
  sih_assert_no_duplicates(out, file, sheet, SIH_QUALITY_KEY)
  out
}

parse_lower_income_quality <- function(file, sheet) {
  raw <- read_sheet_raw(file, sheet)
  est_row <- require_label_row(raw, "^ESTIMATES", file, sheet,
                               "ESTIMATES block marker")
  cols <- anchor_columns(raw, est_row - c(1L, 2L), sih_f8_spec, file, sheet)
  # Table 8.1 publishes one combined "95% margin of error ... and relative
  # standard error" block running to the end of the sheet; the marker row
  # assigns moe/rse per column.
  bounds <- find_block_bounds(raw, file, sheet,
                              start_pattern = "^95% margin of error",
                              stop_pattern = NULL,
                              what = "sampling error block")

  results <- list()
  current_state <- NA_character_
  current_section <- NA_character_
  quality_markers <- rep(NA_character_, length(cols))

  for (i in bounds[["first"]]:bounds[["last"]]) {
    row <- raw[i, ]
    label <- str_trim(as.character(row[[1]]))
    vals <- vapply(cols, function(k) as.character(row[[k]]), character(1))
    marker_row <- vapply(vals, classify_quality_marker, character(1))

    if (is.na(label) || label == "" || label == "NA") {
      if (all(is.na(quality_markers)) && any(!is.na(marker_row))) {
        quality_markers[seq_along(marker_row)] <- marker_row
      }
      next
    }
    if (str_detect(label, "^(Source|Exclud|NA|#|\\*|Cells|\\([a-z]\\)|©)")) next

    if (all(is.na(quality_markers)) && any(!is.na(marker_row))) {
      quality_markers[seq_along(marker_row)] <- marker_row
      next
    }

    all_other_na <- all(is.na(vals) | vals == "" | vals == "NA")
    if (all_other_na && (label %in% names(sih_state_names) ||
                         str_detect(label, sih_state_label_pattern))) {
      current_state <- unname(sih_state_names[label])
      if (is.na(current_state)) current_state <- label
      current_section <- NA_character_
      next
    }

    has_data <- any(!is.na(suppressWarnings(as.numeric(clean_abs_values(vals)))))
    if (!has_data) {
      current_section <- label
      next
    }

    numeric_vals <- as_numeric_clean(vals)
    for (j in seq_along(cols)) {
      quality_measure <- quality_markers[j]
      if (is.na(quality_measure) || is.na(numeric_vals[j])) next

      results[[length(results) + 1]] <- quality_row(
        source_file = file,
        source_table = sheet,
        survey_year = "2019-20",
        metric = names(cols)[j],
        tenure = classify_tenure(label),
        breakdown_var = "lower_income_state",
        breakdown_val = str_trim(str_remove(label, "\\s*\\([a-z]\\)$")),
        geography = ifelse(is.na(current_state), "Unknown", current_state),
        stat_type = "lower_income",
        quality_measure = quality_measure,
        quality_value = numeric_vals[j]
      )
    }
  }

  out <- if (length(results) == 0) empty_sih_estimate_quality() else bind_rows(results)
  sih_assert(nrow(out) > 0, file, sheet, "no quality rows parsed")
  sih_assert_no_duplicates(out, file, sheet, SIH_QUALITY_KEY)
  out
}

# State columns shared by the File 13 (NHHA) estimates and quality parsers.
# Output names keep the published abbreviations; footnote markers in the
# headers ("NT (a)", "ACT (b)") are stripped by anchoring.
sih_f13_state_spec <- list(
  sih_col("NSW",   "^NSW$"),
  sih_col("Vic.",  "^Vic\\.$"),
  sih_col("Qld",   "^Qld$"),
  sih_col("SA",    "^SA$"),
  sih_col("WA",    "^WA$"),
  sih_col("Tas.",  "^Tas\\.$"),
  sih_col("NT",    "^NT$"),
  sih_col("ACT",   "^ACT$"),
  sih_col("Aust.", "^Aust\\.$")
)

# Shared File 13 row machine: metric section headers, location subsections
# ("Greater capital city areas" / "Rest of state" / "Total"), then survey-year
# data rows across the anchored state columns.
scan_nhha_rows <- function(raw, row_range, state_cols, skip_pattern,
                           emit_year_row) {
  results <- list()
  current_location <- NA_character_
  current_section <- NA_character_

  for (i in row_range) {
    row <- raw[i, ]
    label <- str_trim(as.character(row[[1]]))

    if (is.na(label) || label == "" || label == "NA") next
    if (str_detect(label, skip_pattern)) next

    numeric_vals <- as_numeric_clean(
      vapply(state_cols, function(k) as.character(row[[k]]), character(1))
    )
    names(numeric_vals) <- names(state_cols)

    if (all(is.na(numeric_vals))) {
      section_info <- classify_nhha_section(label)
      if (!is.na(section_info$metric)) {
        current_section <- label
        current_location <- NA_character_
      } else if (str_detect(label, regex("greater capital|rest of state|total|location",
                                         ignore_case = TRUE))) {
        current_location <- label
      }
      next
    }

    if (str_detect(label, "\\d{4}[-–]\\d{2}")) {
      emitted <- emit_year_row(
        year_label = str_replace_all(label, "–", "-"),
        section = current_section,
        location = current_location,
        values = numeric_vals
      )
      if (!is.null(emitted) && nrow(emitted) > 0) {
        results[[length(results) + 1L]] <- emitted
      }
    }
  }

  bind_rows(results)
}

parse_nhha_quality <- function(file, sheet) {
  raw <- read_sheet_raw(file, sheet)
  est_row <- require_label_row(raw, "^ESTIMATES", file, sheet,
                               "ESTIMATES block marker")
  state_cols <- anchor_columns(raw, est_row - 1L, sih_f13_state_spec,
                               file, sheet)
  quality_skip <- "^(Source|Exclud|NA|#|\\*|Cells|\\([a-z]\\)|©|na  not)"

  emit_for <- function(measure) {
    function(year_label, section, location, values) {
      section_info <- classify_nhha_section(section)
      if (is.na(section_info$metric)) return(NULL)
      rows <- list()
      for (j in seq_along(values)) {
        if (is.na(values[j])) next
        rows[[length(rows) + 1L]] <- quality_row(
          source_file = file,
          source_table = sheet,
          survey_year = year_label,
          metric = section_info$metric,
          tenure = "renter_lower_income",
          breakdown_var = "nhha_location",
          breakdown_val = ifelse(is.na(location), "Total", location),
          geography = names(values)[j],
          stat_type = section_info$stat_type,
          quality_measure = measure,
          quality_value = values[[j]]
        )
      }
      if (length(rows) == 0) NULL else bind_rows(rows)
    }
  }

  moe_bounds <- find_block_bounds(raw, file, sheet,
                                  start_pattern = "^95% margin of error",
                                  stop_pattern = "^relative standard error",
                                  what = "95% margin of error block")
  rse_bounds <- find_block_bounds(raw, file, sheet,
                                  start_pattern = "^relative standard error",
                                  stop_pattern = NULL,
                                  what = "relative standard error block")

  out <- bind_rows(
    scan_nhha_rows(raw, moe_bounds[["first"]]:moe_bounds[["last"]],
                   state_cols, quality_skip, emit_for("moe_95")),
    scan_nhha_rows(raw, rse_bounds[["first"]]:rse_bounds[["last"]],
                   state_cols, quality_skip, emit_for("rse_pct"))
  )
  sih_assert(nrow(out) > 0, file, sheet, "no quality rows parsed")
  sih_assert_no_duplicates(out, file, sheet, SIH_QUALITY_KEY)
  out
}


# ==============================================================================
# FILE 1: National time series (Tables 1.1, 1.2, 1.3)
# ==============================================================================
cat("  Processing File 1: National time series...\n")

f1_result <- tryCatch({
  t1_1 <- parse_timeseries_table(
    sih_files$f1, "Table 1.1",
    metric = "mean_weekly_cost_real", geography = "National"
  )

  t1_2 <- parse_timeseries_table(
    sih_files$f1, "Table 1.2",
    metric = "cost_income_ratio", geography = "National"
  )

  t1_3 <- parse_timeseries_table(
    sih_files$f1, "Table 1.3",
    metric = "pct_households", geography = "National"
  )

  bind_rows(t1_1, t1_2, t1_3)
}, error = function(e) {
  pipeline_problem("Error processing File 1: ", conditionMessage(e))
  tibble()
})

if (nrow(f1_result) > 0) {
  write_pipeline_csv(f1_result, "sih_timeseries_national.csv")
}

# ==============================================================================
# FILE 3: Housing costs by tenure × demographics (cross-section)
# ==============================================================================
cat("  Processing File 3: Housing costs...\n")

#' Parse a cross-sectional table with tenure columns (Files 3, 4)
#' Columns are anchored to the tenure header band above the ESTIMATES marker;
#' rows run down column A with demographic section headers.
parse_tenure_crosstab <- function(file, sheet, metric, stat_type) {
  tenure_spec <- list(
    sih_col("owner_outright", "^Owner without a mortgage"),
    sih_col("owner_mortgage", "^Owner with a mortgage"),
    sih_col("owner_total",    "^Total owners"),
    sih_col("renter_social",  "^State or territory housing"),
    sih_col("renter_private", "^Private landlord"),
    sih_col("renter_total",   "^Total renters"),
    sih_col("all",            "^All households")
  )

  emit <- function(label, unit, section, values) {
    vals <- values[!is.na(values)]
    if (length(vals) == 0) return(NULL)
    tibble(
      survey_year   = "2019-20",
      value         = unname(vals),
      metric        = metric,
      tenure        = names(vals),
      breakdown_var = ifelse(is.na(section), "overall", simplify_section(section)),
      breakdown_val = str_trim(str_remove(label, "\\s*\\([a-z]\\)$")),
      geography     = "National",
      stat_type     = stat_type
    )
  }

  sih_parse_columns_down(
    file, sheet,
    column_spec = tenure_spec,
    emit = emit,
    label_skip_pattern = "^(ESTIMATES|RELATIVE|Source|Mean|Median|Proportion|Exclud)"
  )
}

f3_result <- tryCatch({
  t3_1 <- parse_tenure_crosstab(sih_files$f3, "Table 3.1",
                                 "weekly_housing_cost", "mean")
  t3_2 <- parse_tenure_crosstab(sih_files$f3, "Table 3.2",
                                 "weekly_housing_cost", "median")
  bind_rows(t3_1, t3_2)
}, error = function(e) {
  pipeline_problem("Error processing File 3: ", conditionMessage(e))
  tibble()
})

if (nrow(f3_result) > 0) {
  write_pipeline_csv(f3_result, "sih_costs_2020.csv")
}

# ==============================================================================
# FILE 4: Housing cost-to-income ratios (cross-section)
# ==============================================================================
cat("  Processing File 4: Cost-to-income ratios...\n")

f4_result <- tryCatch({
  t4_1 <- parse_tenure_crosstab(sih_files$f4, "Table 4.1",
                                 "cost_income_ratio", "mean")
  t4_2 <- parse_tenure_crosstab(sih_files$f4, "Table 4.2",
                                 "cost_income_ratio", "median")
  bind_rows(t4_1, t4_2)
}, error = function(e) {
  pipeline_problem("Error processing File 4: ", conditionMessage(e))
  tibble()
})

if (nrow(f4_result) > 0) {
  write_pipeline_csv(f4_result, "sih_cost_ratios_2020.csv")
}

# ==============================================================================
# FILE 5: Housing stress bands
# ==============================================================================
cat("  Processing File 5: Housing stress bands...\n")

#' Parse stress-band table (File 5)
#' Stress-band columns are anchored to the ratio-range header band above the
#' ESTIMATES marker; rows run down column A with demographic section headers.
parse_stress_bands <- function(file, sheet, population_label) {
  emit <- function(label, unit, section, values) {
    vals <- values[!is.na(values)]
    if (length(vals) == 0) return(NULL)
    tibble(
      survey_year   = "2019-20",
      value         = unname(vals),
      metric        = names(vals),
      tenure        = classify_tenure(label),
      breakdown_var = ifelse(is.na(section), "tenure", simplify_section(section)),
      breakdown_val = str_trim(str_remove(label, "\\s*\\([a-z]\\)$")),
      geography     = "National",
      stat_type     = population_label
    )
  }

  sih_parse_columns_down(
    file, sheet,
    column_spec = sih_f5_band_spec,
    emit = emit,
    label_skip_pattern = "^(ESTIMATES|RELATIVE|Source|Proportion|Exclud|NA)"
  )
}

f5_result <- tryCatch({
  t5_1 <- parse_stress_bands(sih_files$f5, "Table 5.1", "all_households")
  t5_2 <- parse_stress_bands(sih_files$f5, "Table 5.2", "lower_income")
  bind_rows(t5_1, t5_2)
}, error = function(e) {
  pipeline_problem("Error processing File 5: ", conditionMessage(e))
  tibble()
})

if (nrow(f5_result) > 0) {
  write_pipeline_csv(f5_result, "sih_stress_bands_2020.csv")
}

# ==============================================================================
# FILE 6: Age of household reference person
# ==============================================================================
cat("  Processing File 6: Age-tenure breakdown...\n")

f6_result <- tryCatch({
  age_spec <- list(
    sih_col("15_to_24",       "^15 to 24$"),
    sih_col("25_to_34",       "^25 to 34$"),
    sih_col("35_to_44",       "^35 to 44$"),
    sih_col("45_to_54",       "^45 to 54$"),
    sih_col("55_to_64",       "^55 to 64$"),
    sih_col("65_to_74",       "^65 to 74$"),
    sih_col("75_and_over",    "^75 and over$"),
    sih_col("all_households", "^All households$")
  )

  emit_age_tenure <- function(label, unit, section, values) {
    vals <- values[!is.na(values)]
    if (length(vals) == 0) return(NULL)
    row_label <- str_trim(str_remove(label, "\\s*\\([a-z]\\)$"))
    section_key <- ifelse(is.na(section), "overall", simplify_section(section))
    row_metric <- case_when(
      !is.na(unit) && unit == "$" ~ "weekly_housing_cost",
      !is.na(unit) && unit == "'000" ~ "households_000",
      !is.na(unit) && unit == "no." &&
        str_detect(label, regex("bedrooms", ignore_case = TRUE)) ~ "average_bedrooms",
      !is.na(unit) && unit == "no." &&
        str_detect(label, regex("persons", ignore_case = TRUE)) ~ "average_persons",
      !is.na(unit) && unit == "no." ~ "households_in_sample",
      TRUE ~ "pct_households"
    )
    row_stat_type <- case_when(
      !is.na(unit) && unit == "%" ~ "proportion",
      !is.na(unit) && unit == "$" ~ "dollars",
      !is.na(unit) && unit == "'000" ~ "count_000",
      !is.na(unit) && unit == "no." ~ "count",
      TRUE ~ "value"
    )
    tibble(
      survey_year   = "2019-20",
      value         = unname(vals),
      metric        = row_metric,
      tenure        = classify_tenure(label),
      breakdown_var = paste0(section_key, "_by_age_group"),
      breakdown_val = paste(row_label, str_replace_all(names(vals), "_", " "),
                            sep = " | "),
      geography     = "National",
      stat_type     = row_stat_type
    )
  }

  sih_parse_columns_down(
    sih_files$f6, "Table 6.1",
    column_spec = age_spec,
    emit = emit_age_tenure,
    label_skip_pattern = "^(ESTIMATES|RELATIVE|Source|Proportion|Exclud)"
  )
}, error = function(e) {
  pipeline_problem("Error processing File 6: ", conditionMessage(e))
  tibble()
})

if (nrow(f6_result) > 0) {
  write_pipeline_csv(f6_result, "sih_age_tenure_2020.csv")
}

# ==============================================================================
# FILE 8: Lower income households by state
# ==============================================================================
cat("  Processing File 8: Lower income by state...\n")

# State abbreviations used as section headers in Files 8 (column A) and
# shared by the File 8 quality parser.
sih_state_names <- c("NSW" = "New South Wales", "Vic." = "Victoria", "VIC" = "Victoria",
                     "Qld" = "Queensland", "QLD" = "Queensland",
                     "SA" = "South Australia", "WA" = "Western Australia",
                     "Tas." = "Tasmania", "TAS" = "Tasmania",
                     "NT" = "Northern Territory", "ACT" = "Australian Capital Territory",
                     "Aust." = "Australia", "AUST" = "Australia",
                     "Australia" = "Australia", "Total Australia" = "Australia")
sih_state_label_pattern <- "^(NSW|VIC|Vic|QLD|Qld|SA|WA|TAS|Tas|NT|ACT|Aust|Total Australia)"

# Column spec shared by the File 8 estimates and quality parsers: median
# columns anchor to the row-5 headers, stress bands and the household count
# to the row-6 band headers.
sih_f8_spec <- list(
  sih_col("median_weekly_cost",       "^Median housing costs$"),
  sih_col("median_cost_income_ratio", "^Median ratio of housing costs to gross household income$"),
  sih_col("pct_25_or_less",           "^25 % or less$"),
  sih_col("pct_25_to_30",             "^More than 25% to 30%$"),
  sih_col("pct_30_to_50",             "^More than 30% to 50%$"),
  sih_col("pct_over_50",              "^More than 50%$"),
  sih_col("pct_total",                "^Total$"),
  sih_col("pct_over_30",              "^More than 30%$"),
  sih_col("households_000",           "^All households$")
)

f8_result <- tryCatch({
  emit_lower_income <- function(label, section, state, values) {
    vals <- values[!is.na(values)]
    if (length(vals) == 0) return(NULL)
    tibble(
      survey_year   = "2019-20",
      value         = unname(vals),
      metric        = names(vals),
      tenure        = classify_tenure(label),
      breakdown_var = "lower_income_state",
      breakdown_val = str_trim(str_remove(label, "\\s*\\([a-z]\\)$")),
      geography     = ifelse(is.na(state), "Unknown", state),
      stat_type     = "lower_income"
    )
  }

  sih_parse_state_sections(
    sih_files$f8, "Table 8.1",
    column_spec = sih_f8_spec,
    emit = emit_lower_income,
    label_skip_pattern = "^(ESTIMATES|RELATIVE|Source|Proportion|Exclud)",
    state_map = sih_state_names,
    state_pattern = sih_state_label_pattern
  )
}, error = function(e) {
  pipeline_problem("Error processing File 8: ", conditionMessage(e))
  tibble()
})

if (nrow(f8_result) > 0) {
  write_pipeline_csv(f8_result, "sih_lower_income_states.csv")
}

# ==============================================================================
# FILE 9: Recent home buyer households
# ==============================================================================
cat("  Processing File 9: Recent home buyers...\n")

f9_result <- tryCatch({
  # Buyer-type groups (merged headers, forward-filled) x dwelling-type leaf
  # headers, anchored above the ESTIMATES marker. Block bounding keeps the
  # MOE/RSE sub-blocks out of the estimates.
  buyer_spec <- list(
    sih_col("first_home_new",         "^New$",         group = "^First home buyer$"),
    sih_col("first_home_established", "^Established$", group = "^First home buyer$"),
    sih_col("first_home_total",       "^Total$",       group = "^First home buyer$"),
    sih_col("changeover_new",         "^New$",         group = "^Changeover buyer$"),
    sih_col("changeover_established", "^Established$", group = "^Changeover buyer$"),
    sih_col("changeover_total",       "^Total$",       group = "^Changeover buyer$"),
    sih_col("all_recent_new",         "^New$",         group = "^All recent home buyer households$"),
    sih_col("all_recent_established", "^Established$", group = "^All recent home buyer households$"),
    sih_col("all_recent_total",       "^Total$",       group = "^All recent home buyer households$")
  )

  emit_recent_buyers <- function(label, unit, section, values) {
    vals <- values[!is.na(values)]
    if (length(vals) == 0) return(NULL)
    # Split buyer column into buyer_type and dwelling_type
    parts <- str_split(names(vals), "_(?=new|established|total)", n = 2)
    buyer_type <- vapply(parts, function(p) p[[1]], character(1))
    dwelling_type <- vapply(parts, function(p) {
      if (length(p) > 1) p[[2]] else "total"
    }, character(1))
    row_metric <- ifelse(is.na(section), label,
                         str_trim(str_remove(label, "\\s*\\([a-z]\\)$")))
    # Bare "Total" rows recur under several proportion sections (tenure,
    # family, age, ...) with otherwise identical keys; qualify them with
    # their section so each section total stays a distinct observation.
    if (!is.na(section) && identical(row_metric, "Total")) {
      row_metric <- paste(row_metric,
                          str_trim(str_remove(section, "\\s*\\([a-z]\\)$")),
                          sep = " | ")
    }
    tibble(
      survey_year   = "2019-20",
      value         = unname(vals),
      metric        = row_metric,
      tenure        = "owner_mortgage",
      breakdown_var = paste0("buyer_", buyer_type, "_", dwelling_type),
      breakdown_val = str_trim(str_remove(label, "\\s*\\([a-z]\\)$")),
      geography     = "National",
      stat_type     = ifelse(!is.na(unit) && unit == "%", "proportion",
                             ifelse(!is.na(unit) && unit == "$", "dollars", "count"))
    )
  }

  sih_parse_columns_down(
    sih_files$f9, "Table 9.1",
    column_spec = buyer_spec,
    emit = emit_recent_buyers,
    label_skip_pattern = "^(ESTIMATES|RELATIVE|Source|Exclud)",
    group_offset = 2L
  )
}, error = function(e) {
  pipeline_problem("Error processing File 9: ", conditionMessage(e))
  tibble()
})

if (nrow(f9_result) > 0) {
  write_pipeline_csv(f9_result, "sih_recent_buyers_2020.csv")
}

# ==============================================================================
# FILE 11: Greater capital city statistical areas
# ==============================================================================
cat("  Processing File 11: Geographic breakdowns...\n")

f11_result <- tryCatch({
  # File 11 holds two distinct layouts: Tables 11.1-11.3 break down greater
  # capital city areas vs rest of state; Tables 11.4-11.6 break down states
  # and territories. Each trio is median weekly cost / median cost-to-income
  # ratio / household proportions. (The legacy positional parser applied the
  # GCC columns and made-up mean_* metric names to the state tables, so state
  # medians shipped as GCC means; header anchoring makes that impossible.)
  gcc_spec <- list(
    sih_col("Gr. Sydney",          "^Greater Sydney$"),
    sih_col("Gr. Melbourne",       "^Greater Melbourne$"),
    sih_col("Gr. Brisbane",        "^Greater Brisbane$"),
    sih_col("Gr. Adelaide",        "^Greater Adelaide$"),
    sih_col("Gr. Perth",           "^Greater Perth$"),
    sih_col("Gr. Hobart",          "^Greater Hobart$"),
    sih_col("Gr. Darwin",          "^Greater Darwin$"),
    sih_col("ACT",                 "^ACT$"),
    sih_col("Total GCC",           "^Total greater capital city areas$"),
    sih_col("Rest of NSW",         "^Rest of NSW$"),
    sih_col("Rest of Vic.",        "^Rest of Vic\\.$"),
    sih_col("Rest of Qld",         "^Rest of Qld$"),
    sih_col("Rest of SA",          "^Rest of SA$"),
    sih_col("Rest of WA",          "^Rest of WA$"),
    sih_col("Rest of Tas.",        "^Rest of Tas\\.$"),
    sih_col("Total rest of state", "^Total rest of state$")
  )
  state_spec <- list(
    sih_col("New South Wales",              "^NSW$"),
    sih_col("Victoria",                     "^Vic\\.$"),
    sih_col("Queensland",                   "^Qld$"),
    sih_col("South Australia",              "^SA$"),
    sih_col("Western Australia",            "^WA$"),
    sih_col("Tasmania",                     "^Tas\\.$"),
    sih_col("Northern Territory",           "^NT$"),
    sih_col("Australian Capital Territory", "^ACT$"),
    sih_col("Australia",                    "^Total Australia$")
  )

  f11_layouts <- list(
    "Table 11.1" = list(spec = gcc_spec, metric = "median_weekly_cost",
                        stat_type = "median"),
    "Table 11.2" = list(spec = gcc_spec, metric = "median_cost_income_ratio",
                        stat_type = "median"),
    "Table 11.3" = list(spec = gcc_spec, metric = "pct_households_tenure",
                        stat_type = "mean"),
    "Table 11.4" = list(spec = state_spec, metric = "median_weekly_cost",
                        stat_type = "median"),
    "Table 11.5" = list(spec = state_spec, metric = "median_cost_income_ratio",
                        stat_type = "median"),
    "Table 11.6" = list(spec = state_spec, metric = "pct_households_tenure",
                        stat_type = "mean")
  )

  sheets_11 <- excel_sheets(sih_files$f11)
  found_tables_11 <- sheets_11[str_detect(sheets_11, "Table")]
  if (!setequal(found_tables_11, names(f11_layouts))) {
    stop("File 11 table sheets changed - expected ",
         paste(names(f11_layouts), collapse = ", "), " but found ",
         paste(found_tables_11, collapse = ", "), call. = FALSE)
  }

  map_dfr(names(f11_layouts), function(sheet_name) {
    layout <- f11_layouts[[sheet_name]]

    emit <- function(label, unit, section, values) {
      vals <- values[!is.na(values)]
      if (length(vals) == 0) return(NULL)
      tibble(
        survey_year   = "2019-20",
        value         = unname(vals),
        metric        = layout$metric,
        tenure        = classify_tenure(label),
        breakdown_var = ifelse(is.na(section), "tenure",
                               simplify_section(section)),
        breakdown_val = str_trim(str_remove(label, "\\s*\\([a-z]\\)$")),
        geography     = names(vals),
        stat_type     = layout$stat_type
      )
    }

    sih_parse_columns_down(
      sih_files$f11, sheet_name,
      column_spec = layout$spec,
      emit = emit,
      label_skip_pattern = "^(ESTIMATES|RELATIVE|Source|Exclud|Median|Mean|Proportion)"
    )
  })
}, error = function(e) {
  pipeline_problem("Error processing File 11: ", conditionMessage(e))
  tibble()
})

if (nrow(f11_result) > 0) {
  write_pipeline_csv(f11_result, "sih_geographic_2020.csv")
}

# ==============================================================================
# FILE 12: State-level time series (Tables 12.1-12.24)
# ==============================================================================
cat("  Processing File 12: State time series...\n")

f12_result <- tryCatch({
  sheets_12 <- excel_sheets(sih_files$f12)
  data_sheets <- sheets_12[str_detect(sheets_12, "Table")]

  # Each sheet title names its metric and state (e.g. "Table 12.1 MEAN WEEKLY
  # HOUSING COSTS, Selected household characteristics, New South Wales, ...").
  # Anchor both from the title instead of assuming the 3-tables-per-state
  # sheet cycle, so a reordered workbook cannot mislabel a state's series.
  f12_metric_patterns <- c(
    "MEAN WEEKLY HOUSING COSTS" = "mean_weekly_cost_real",
    "HOUSING COSTS AS A PROPORTION OF GROSS HOUSEHOLD INCOME" = "cost_income_ratio",
    "HOUSEHOLD ESTIMATES" = "pct_households"
  )
  f12_states <- c("New South Wales", "Victoria", "Queensland",
                  "South Australia", "Western Australia", "Tasmania",
                  "Northern Territory", "Australian Capital Territory")

  map_dfr(data_sheets, function(sheet_name) {
    raw <- read_sheet_raw(sih_files$f12, sheet_name)
    title_row <- require_label_row(raw, "^Table \\d", sih_files$f12,
                                   sheet_name, "table title")
    title <- str_squish(as.character(raw[title_row, 1]))
    metric_hits <- f12_metric_patterns[
      vapply(names(f12_metric_patterns), function(p) {
        str_detect(title, fixed(p))
      }, logical(1))
    ]
    state_hits <- f12_states[
      vapply(f12_states, function(s) str_detect(title, fixed(s)), logical(1))
    ]
    sih_assert(length(metric_hits) == 1, sih_files$f12, sheet_name,
               paste0("table title must name exactly one metric: ", title))
    sih_assert(length(state_hits) == 1, sih_files$f12, sheet_name,
               paste0("table title must name exactly one state: ", title))

    parse_timeseries_table(
      sih_files$f12, sheet_name,
      metric = unname(metric_hits), geography = state_hits
    )
  })
}, error = function(e) {
  pipeline_problem("Error processing File 12: ", conditionMessage(e))
  tibble()
})

if (nrow(f12_result) > 0) {
  write_pipeline_csv(f12_result, "sih_state_timeseries.csv")
}

# ==============================================================================
# FILE 13: NHHA rental affordability
# ==============================================================================
cat("  Processing File 13: NHHA rental stress...\n")

f13_result <- tryCatch({
  raw <- read_sheet_raw(sih_files$f13, "Table 13.1")
  est_row <- require_label_row(raw, "^ESTIMATES", sih_files$f13, "Table 13.1",
                               "ESTIMATES block marker")
  state_cols <- anchor_columns(raw, est_row - 1L, sih_f13_state_spec,
                               sih_files$f13, "Table 13.1")
  bounds <- find_block_bounds(raw, sih_files$f13, "Table 13.1",
                              stop_pattern = "^95% margin of error|^relative standard error")

  emit_nhha_estimate <- function(year_label, section, location, values) {
    section_info <- classify_nhha_section(section)
    if (is.na(section_info$metric)) {
      warning("Skipping File 13 data row with unrecognised section: ", section)
      return(NULL)
    }
    vals <- values[!is.na(values)]
    if (length(vals) == 0) return(NULL)
    tibble(
      survey_year   = year_label,
      value         = unname(vals),
      metric        = section_info$metric,
      tenure        = "renter_lower_income",
      breakdown_var = "nhha_location",
      breakdown_val = ifelse(is.na(location), "Total", location),
      geography     = names(vals),
      stat_type     = section_info$stat_type
    )
  }

  out <- scan_nhha_rows(
    raw, bounds[["first"]]:bounds[["last"]], state_cols,
    skip_pattern = "^(ESTIMATES|RELATIVE|Source|Exclud|NA|#)",
    emit_year_row = emit_nhha_estimate
  )
  sih_assert(nrow(out) > 0, sih_files$f13, "Table 13.1",
             "no estimate rows parsed")
  sih_assert_no_duplicates(out, sih_files$f13, "Table 13.1")
  out
}, error = function(e) {
  pipeline_problem("Error processing File 13: ", conditionMessage(e))
  tibble()
})


if (nrow(f13_result) > 0) {
  write_pipeline_csv(f13_result, "sih_nhha_rental_stress.csv")
}

cat("  Processing SIH sampling-error metadata...\n")

# Each quality parser is block-bounded and asserts its own zero-duplicate
# key, so no distinct() safety net is needed (or wanted) at combine time.
sih_quality_result <- tryCatch({
  combined_quality <- bind_rows(
    parse_stress_bands_quality(sih_files$f5, "Table 5.1", "all_households"),
    parse_stress_bands_quality(sih_files$f5, "Table 5.2", "lower_income"),
    parse_lower_income_quality(sih_files$f8, "Table 8.1"),
    parse_nhha_quality(sih_files$f13, "Table 13.1")
  ) %>%
    select(all_of(sih_estimate_quality_columns))
  sih_assert_no_duplicates(combined_quality, "SIH quality outputs", "combined",
                           SIH_QUALITY_KEY)
  combined_quality
}, error = function(e) {
  pipeline_problem("Error processing SIH sampling-error metadata: ", conditionMessage(e))
  empty_sih_estimate_quality()
})

if (nrow(sih_quality_result) > 0) {
  write_pipeline_csv(sih_quality_result, "sih_estimate_quality.csv")
}

cat("--- SIH processing complete ---\n")

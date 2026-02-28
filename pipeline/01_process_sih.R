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
#' Parse a time-series SIH table (years across columns)
#'
#' @param file Path to Excel file
#' @param sheet Sheet name
#' @param metric Name of the metric being measured
#' @param unit_expected Expected unit ($ or %)
#' @param geography Geography label
#' @return Long-format tibble
parse_timeseries_table <- function(file, sheet, metric, unit_expected = NULL,
                                   geography = "National") {
  # Read raw — skip first 4 rows (ABS branding), keeping row 5 (year headers)
  raw <- read_excel(file, sheet = sheet, skip = 4,
                    col_names = FALSE, col_types = "text")

  # Row 1 of raw = row 5 of Excel (year labels in cols 3+)
  # Extract year labels from first row
  year_row <- as.character(raw[1, ])
  year_cols <- which(!is.na(year_row) & str_detect(year_row, "\\d{4}"))

  if (length(year_cols) == 0) {
    warning("No year columns found in ", sheet, " of ", basename(file))
    return(tibble())
  }

  # Normalize en-dashes to hyphens in year labels
  years <- str_replace_all(year_row[year_cols], "\u2013", "-")

  # Data starts after ESTIMATES marker and section headers
  # Col A (1) = label, Col B (2) = unit, Cols 3+ = data
  data_raw <- raw[-1, ]  # Remove year header row

  # Build result by iterating rows
  results <- list()
  current_section <- NA_character_
  current_subsection <- NA_character_

  for (i in seq_len(nrow(data_raw))) {
    row <- data_raw[i, ]
    label <- str_trim(as.character(row[[1]]))
    unit_val <- str_trim(as.character(row[[2]]))

    if (is.na(label) || label == "" || label == "NA") next
    if (str_detect(label, "^(ESTIMATES|RELATIVE STANDARD ERRORS|Source|Exclud)")) next

    # Extract numeric values from year columns
    vals <- as.character(row[year_cols])
    has_data <- any(!is.na(suppressWarnings(as.numeric(clean_abs_values(vals)))))

    if (!has_data) {
      # This is a section/subsection header
      if (is.na(unit_val) || unit_val == "" || unit_val == "NA") {
        # Check if this looks like a major section
        if (str_detect(label, "(Tenure|Family|Gross|Equivalised|Main source|Government|Age|Weekly|Housing)")) {
          current_section <- label
          current_subsection <- NA_character_
        } else {
          current_subsection <- label
        }
      }
      next
    }

    # This is a data row
    numeric_vals <- as_numeric_clean(vals)

    for (j in seq_along(years)) {
      if (!is.na(numeric_vals[j])) {
        results[[length(results) + 1]] <- tibble(
          survey_year   = years[j],
          value         = numeric_vals[j],
          metric        = metric,
          tenure        = classify_tenure(label),
          breakdown_var = ifelse(is.na(current_section), "tenure", simplify_section(current_section)),
          breakdown_val = str_trim(str_remove(label, "\\([a-z]\\)$")),
          geography     = geography,
          stat_type     = ifelse(str_detect(metric, "[Mm]edian"), "median", "mean")
        )
      }
    }
  }

  bind_rows(results)
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
  warning("Error processing File 1: ", conditionMessage(e))
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
#' Layout: Col A = label, Col B = unit, Cols C-E = Owner subtypes,
#'         Col F = spacer, Cols G-I = Renter subtypes, Col J = All households
parse_tenure_crosstab <- function(file, sheet, metric, stat_type) {
  tenure_cols <- c("owner_outright", "owner_mortgage", "owner_total",
                   "spacer",
                   "renter_social", "renter_private", "renter_total",
                   "all")

  raw <- read_excel(file, sheet = sheet, skip = 6,
                    col_names = FALSE, col_types = "text")

  results <- list()
  current_section <- NA_character_

  for (i in seq_len(nrow(raw))) {
    row <- raw[i, ]
    label <- str_trim(as.character(row[[1]]))
    unit_val <- str_trim(as.character(row[[2]]))

    if (is.na(label) || label == "" || label == "NA") next
    if (str_detect(label, "^(ESTIMATES|RELATIVE|Source|Mean|Median|Proportion|Exclud)")) next

    # Get values from cols 3-10
    vals <- as.character(row[3:min(10, ncol(raw))])
    has_data <- any(!is.na(suppressWarnings(as.numeric(clean_abs_values(vals)))))

    if (!has_data) {
      current_section <- label
      next
    }

    numeric_vals <- as_numeric_clean(vals)

    for (j in seq_along(tenure_cols)) {
      tname <- tenure_cols[j]
      if (tname == "spacer" || j > length(numeric_vals)) next
      if (!is.na(numeric_vals[j])) {
        results[[length(results) + 1]] <- tibble(
          survey_year   = "2019-20",
          value         = numeric_vals[j],
          metric        = metric,
          tenure        = tname,
          breakdown_var = ifelse(is.na(current_section), "overall", simplify_section(current_section)),
          breakdown_val = str_trim(str_remove(label, "\\s*\\([a-z]\\)$")),
          geography     = "National",
          stat_type     = stat_type
        )
      }
    }
  }

  bind_rows(results)
}

f3_result <- tryCatch({
  t3_1 <- parse_tenure_crosstab(sih_files$f3, "Table 3.1",
                                 "weekly_housing_cost", "mean")
  t3_2 <- parse_tenure_crosstab(sih_files$f3, "Table 3.2",
                                 "weekly_housing_cost", "median")
  bind_rows(t3_1, t3_2)
}, error = function(e) {
  warning("Error processing File 3: ", conditionMessage(e))
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
  warning("Error processing File 4: ", conditionMessage(e))
  tibble()
})

if (nrow(f4_result) > 0) {
  write_pipeline_csv(f4_result, "sih_cost_ratios_2020.csv")
}

# ==============================================================================
# FILE 5: Housing stress bands
# ==============================================================================
cat("  Processing File 5: Housing stress bands...\n")

#' Parse stress-band table (Files 5)
#' Layout: Col A = label, Cols B-F = stress bands (%), Col G = >30% summary,
#'         Col H = household count ('000)
parse_stress_bands <- function(file, sheet, population_label) {
  band_cols <- c("pct_25_or_less", "pct_25_to_30", "pct_30_to_50",
                 "pct_over_50", "pct_total", "pct_over_30", "households_000")

  raw <- read_excel(file, sheet = sheet, skip = 6,
                    col_names = FALSE, col_types = "text")

  results <- list()
  current_section <- NA_character_

  for (i in seq_len(nrow(raw))) {
    row <- raw[i, ]
    label <- str_trim(as.character(row[[1]]))

    if (is.na(label) || label == "" || label == "NA") next
    if (str_detect(label, "^(ESTIMATES|RELATIVE|Source|Proportion|Exclud|NA)")) next

    vals <- as.character(row[2:min(8, ncol(raw))])
    has_data <- any(!is.na(suppressWarnings(as.numeric(clean_abs_values(vals)))))

    if (!has_data) {
      current_section <- label
      next
    }

    numeric_vals <- as_numeric_clean(vals)

    for (j in seq_along(band_cols)) {
      if (j > length(numeric_vals)) next
      if (!is.na(numeric_vals[j])) {
        results[[length(results) + 1]] <- tibble(
          survey_year   = "2019-20",
          value         = numeric_vals[j],
          metric        = band_cols[j],
          tenure        = classify_tenure(label),
          breakdown_var = ifelse(is.na(current_section), "tenure", simplify_section(current_section)),
          breakdown_val = str_trim(str_remove(label, "\\s*\\([a-z]\\)$")),
          geography     = "National",
          stat_type     = population_label
        )
      }
    }
  }

  bind_rows(results)
}

f5_result <- tryCatch({
  t5_1 <- parse_stress_bands(sih_files$f5, "Table 5.1", "all_households")
  t5_2 <- parse_stress_bands(sih_files$f5, "Table 5.2", "lower_income")
  bind_rows(t5_1, t5_2)
}, error = function(e) {
  warning("Error processing File 5: ", conditionMessage(e))
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
  age_groups <- c("15_to_24", "25_to_34", "35_to_44", "45_to_54",
                  "55_to_64", "65_to_74", "75_and_over", "all_households")

  raw <- read_excel(sih_files$f6, sheet = "Table 6.1", skip = 6,
                    col_names = FALSE, col_types = "text")

  results <- list()
  current_section <- NA_character_

  for (i in seq_len(nrow(raw))) {
    row <- raw[i, ]
    label <- str_trim(as.character(row[[1]]))
    unit_val <- str_trim(as.character(row[[2]]))

    if (is.na(label) || label == "" || label == "NA") next
    if (str_detect(label, "^(ESTIMATES|RELATIVE|Source|Proportion|Exclud)")) next

    vals <- as.character(row[3:min(10, ncol(raw))])
    has_data <- any(!is.na(suppressWarnings(as.numeric(clean_abs_values(vals)))))

    if (!has_data) {
      current_section <- label
      next
    }

    numeric_vals <- as_numeric_clean(vals)

    for (j in seq_along(age_groups)) {
      if (j > length(numeric_vals)) next
      if (!is.na(numeric_vals[j])) {
        results[[length(results) + 1]] <- tibble(
          survey_year   = "2019-20",
          value         = numeric_vals[j],
          metric        = ifelse(!is.na(unit_val) && unit_val == "$",
                                 "weekly_housing_cost", "pct_households"),
          tenure        = classify_tenure(label),
          breakdown_var = "age_group",
          breakdown_val = str_replace_all(age_groups[j], "_", " "),
          geography     = "National",
          stat_type     = "proportion"
        )
      }
    }
  }

  bind_rows(results)
}, error = function(e) {
  warning("Error processing File 6: ", conditionMessage(e))
  tibble()
})

if (nrow(f6_result) > 0) {
  write_pipeline_csv(f6_result, "sih_age_tenure_2020.csv")
}

# ==============================================================================
# FILE 8: Lower income households by state
# ==============================================================================
cat("  Processing File 8: Lower income by state...\n")

f8_result <- tryCatch({
  # Layout: Col A = label, Col B = median cost ($), Col C = median ratio (%),
  #         Col D = spacer, Cols E-H = stress bands (%),
  #         Col I-J = >30% and total, Col K = households ('000)
  metric_cols <- c("median_weekly_cost", "median_cost_income_ratio", "spacer",
                   "pct_25_or_less", "pct_25_to_30", "pct_30_to_50",
                   "pct_over_50", "pct_total", "pct_over_30", "households_000")

  raw <- read_excel(sih_files$f8, sheet = "Table 8.1", skip = 8,
                    col_names = FALSE, col_types = "text")

  results <- list()
  current_state <- NA_character_
  current_section <- NA_character_

  state_names <- c("NSW" = "New South Wales", "Vic." = "Victoria", "VIC" = "Victoria",
                   "Qld" = "Queensland", "QLD" = "Queensland",
                   "SA" = "South Australia", "WA" = "Western Australia",
                   "Tas." = "Tasmania", "TAS" = "Tasmania",
                   "NT" = "Northern Territory", "ACT" = "Australian Capital Territory",
                   "Aust." = "Australia", "AUST" = "Australia",
                   "Australia" = "Australia")

  for (i in seq_len(nrow(raw))) {
    row <- raw[i, ]
    label <- str_trim(as.character(row[[1]]))

    if (is.na(label) || label == "" || label == "NA") next
    if (str_detect(label, "^(ESTIMATES|RELATIVE|Source|Proportion|Exclud)")) next

    # Check if this is a state header row (all other cols NA or empty)
    other_vals <- as.character(row[2:min(11, ncol(raw))])
    all_other_na <- all(is.na(other_vals) | other_vals == "" | other_vals == "NA")

    # Match against known state abbreviations
    if (all_other_na && (label %in% names(state_names) ||
                         str_detect(label, "^(NSW|VIC|Vic|QLD|Qld|SA|WA|TAS|Tas|NT|ACT|Aust)"))) {
      current_state <- state_names[label]
      if (is.na(current_state)) current_state <- label
      current_section <- NA_character_
      next
    }

    vals <- as.character(row[2:min(11, ncol(raw))])
    has_data <- any(!is.na(suppressWarnings(as.numeric(clean_abs_values(vals)))))

    if (!has_data) {
      current_section <- label
      next
    }

    numeric_vals <- as_numeric_clean(vals)

    for (j in seq_along(metric_cols)) {
      mname <- metric_cols[j]
      if (mname == "spacer" || j > length(numeric_vals)) next
      if (!is.na(numeric_vals[j])) {
        results[[length(results) + 1]] <- tibble(
          survey_year   = "2019-20",
          value         = numeric_vals[j],
          metric        = mname,
          tenure        = classify_tenure(label),
          breakdown_var = "lower_income_state",
          breakdown_val = str_trim(str_remove(label, "\\s*\\([a-z]\\)$")),
          geography     = ifelse(is.na(current_state), "Unknown", current_state),
          stat_type     = "lower_income"
        )
      }
    }
  }

  bind_rows(results)
}, error = function(e) {
  warning("Error processing File 8: ", conditionMessage(e))
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
  # Cols: A=label, B=unit, C-E = First home buyer (New/Established/Total),
  #       F=spacer, G-I = Changeover (New/Established/Total),
  #       J=spacer, K-M = All recent (New/Established/Total)
  buyer_cols <- c("first_home_new", "first_home_established", "first_home_total",
                  "spacer1",
                  "changeover_new", "changeover_established", "changeover_total",
                  "spacer2",
                  "all_recent_new", "all_recent_established", "all_recent_total")

  raw <- read_excel(sih_files$f9, sheet = "Table 9.1", skip = 6,
                    col_names = FALSE, col_types = "text")

  results <- list()
  current_section <- NA_character_

  for (i in seq_len(nrow(raw))) {
    row <- raw[i, ]
    label <- str_trim(as.character(row[[1]]))
    unit_val <- str_trim(as.character(row[[2]]))

    if (is.na(label) || label == "" || label == "NA") next
    if (str_detect(label, "^(ESTIMATES|RELATIVE|Source|Exclud)")) next

    vals <- as.character(row[3:min(13, ncol(raw))])
    has_data <- any(!is.na(suppressWarnings(as.numeric(clean_abs_values(vals)))))

    if (!has_data) {
      current_section <- label
      next
    }

    numeric_vals <- as_numeric_clean(vals)

    for (j in seq_along(buyer_cols)) {
      bname <- buyer_cols[j]
      if (str_detect(bname, "^spacer") || j > length(numeric_vals)) next
      if (!is.na(numeric_vals[j])) {
        # Split buyer column into buyer_type and dwelling_type
        parts <- str_split(bname, "_(?=new|established|total)", n = 2)[[1]]
        buyer_type <- parts[1]
        dwelling_type <- if (length(parts) > 1) parts[2] else "total"

        results[[length(results) + 1]] <- tibble(
          survey_year   = "2019-20",
          value         = numeric_vals[j],
          metric        = ifelse(is.na(current_section), label,
                                 str_trim(str_remove(label, "\\s*\\([a-z]\\)$"))),
          tenure        = "owner_mortgage",
          breakdown_var = paste0("buyer_", buyer_type, "_", dwelling_type),
          breakdown_val = str_trim(str_remove(label, "\\s*\\([a-z]\\)$")),
          geography     = "National",
          stat_type     = ifelse(!is.na(unit_val) && unit_val == "%", "proportion",
                                ifelse(!is.na(unit_val) && unit_val == "$", "dollars", "count"))
        )
      }
    }
  }

  bind_rows(results)
}, error = function(e) {
  warning("Error processing File 9: ", conditionMessage(e))
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
  # Column layout: A=label, B=unit,
  # C-K = GCC areas (Sydney, Melbourne, Brisbane, Adelaide, Perth, Hobart, Darwin, ACT, Total GCC)
  # L = spacer
  # M-S = Rest of state (NSW, Vic, Qld, SA, WA, Tas, Total rest)
  geo_cols <- c("Gr. Sydney", "Gr. Melbourne", "Gr. Brisbane", "Gr. Adelaide",
                "Gr. Perth", "Gr. Hobart", "Gr. Darwin", "ACT", "Total GCC",
                "spacer",
                "Rest of NSW", "Rest of Vic.", "Rest of Qld", "Rest of SA",
                "Rest of WA", "Rest of Tas.", "Total rest of state")

  sheets_11 <- excel_sheets(sih_files$f11)
  data_sheets <- sheets_11[str_detect(sheets_11, "Table")]

  # Table metrics mapping
  table_metrics <- c(
    "Table 11.1" = "median_weekly_cost",
    "Table 11.2" = "median_cost_income_ratio",
    "Table 11.3" = "pct_households_tenure",
    "Table 11.4" = "pct_households_dwelling",
    "Table 11.5" = "mean_weekly_cost",
    "Table 11.6" = "mean_cost_income_ratio"
  )

  map_dfr(data_sheets, function(sheet_name) {
    metric <- table_metrics[sheet_name]
    if (is.na(metric)) metric <- sheet_name

    raw <- read_excel(sih_files$f11, sheet = sheet_name, skip = 6,
                      col_names = FALSE, col_types = "text")

    results <- list()
    current_section <- NA_character_

    for (i in seq_len(nrow(raw))) {
      row <- raw[i, ]
      label <- str_trim(as.character(row[[1]]))
      unit_val <- str_trim(as.character(row[[2]]))

      if (is.na(label) || label == "" || label == "NA") next
      if (str_detect(label, "^(ESTIMATES|RELATIVE|Source|Exclud|Median|Mean|Proportion)")) next

      vals <- as.character(row[3:min(19, ncol(raw))])
      has_data <- any(!is.na(suppressWarnings(as.numeric(clean_abs_values(vals)))))

      if (!has_data) {
        current_section <- label
        next
      }

      numeric_vals <- as_numeric_clean(vals)

      for (j in seq_along(geo_cols)) {
        gname <- geo_cols[j]
        if (gname == "spacer" || j > length(numeric_vals)) next
        if (!is.na(numeric_vals[j])) {
          results[[length(results) + 1]] <- tibble(
            survey_year   = "2019-20",
            value         = numeric_vals[j],
            metric        = metric,
            tenure        = classify_tenure(label),
            breakdown_var = ifelse(is.na(current_section), "tenure",
                                   simplify_section(current_section)),
            breakdown_val = str_trim(str_remove(label, "\\s*\\([a-z]\\)$")),
            geography     = gname,
            stat_type     = ifelse(str_detect(metric, "median"), "median", "mean")
          )
        }
      }
    }

    bind_rows(results)
  })
}, error = function(e) {
  warning("Error processing File 11: ", conditionMessage(e))
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

  # Each state has 3 tables: costs (mean real $), cost/income ratio, household proportions
  # Pattern: Tables 12.1-12.3 = NSW, 12.4-12.6 = VIC, etc.
  state_order <- c("New South Wales", "Victoria", "Queensland",
                   "South Australia", "Western Australia", "Tasmania",
                   "Northern Territory", "Australian Capital Territory")
  metric_cycle <- c("mean_weekly_cost_real", "cost_income_ratio", "pct_households")

  map_dfr(seq_along(data_sheets), function(idx) {
    sheet_name <- data_sheets[idx]
    state_idx <- ((idx - 1) %/% 3) + 1
    metric_idx <- ((idx - 1) %% 3) + 1

    state <- if (state_idx <= length(state_order)) state_order[state_idx] else "Unknown"
    metric <- metric_cycle[metric_idx]

    parse_timeseries_table(
      sih_files$f12, sheet_name,
      metric = metric, geography = state
    )
  })
}, error = function(e) {
  warning("Error processing File 12: ", conditionMessage(e))
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
  # Layout: Col A = label/year, Col B = unit,
  # Cols C-K = NSW, Vic., Qld, SA, WA, Tas., NT, ACT, Aust.
  state_cols <- c("NSW", "Vic.", "Qld", "SA", "WA", "Tas.", "NT", "ACT", "Aust.")

  raw <- read_excel(sih_files$f13, sheet = "Table 13.1", skip = 6,
                    col_names = FALSE, col_types = "text")

  results <- list()
  current_location <- NA_character_
  current_section <- NA_character_

  for (i in seq_len(nrow(raw))) {
    row <- raw[i, ]
    label <- str_trim(as.character(row[[1]]))

    if (is.na(label) || label == "" || label == "NA") next
    if (str_detect(label, "^(ESTIMATES|RELATIVE|Source|Proportion|Exclud|NA)")) next

    vals <- as.character(row[3:min(11, ncol(raw))])
    has_data <- any(!is.na(suppressWarnings(as.numeric(clean_abs_values(vals)))))

    if (!has_data) {
      # Check if this is a location header
      if (str_detect(label, regex("greater capital|rest of state|total|location",
                                  ignore_case = TRUE))) {
        current_location <- label
      } else {
        current_section <- label
      }
      next
    }

    # Data row — label should be a survey year like "2007-08" or "2007–08" (en-dash)
    if (str_detect(label, "\\d{4}[-\u2013]\\d{2}")) {
      # Normalize en-dash to hyphen
      year_label <- str_replace_all(label, "\u2013", "-")
      numeric_vals <- as_numeric_clean(vals)

      for (j in seq_along(state_cols)) {
        if (j > length(numeric_vals)) next
        if (!is.na(numeric_vals[j])) {
          results[[length(results) + 1]] <- tibble(
            survey_year   = year_label,
            value         = numeric_vals[j],
            metric        = ifelse(is.na(current_section),
                                   "pct_rental_stress_over_30",
                                   paste0("pct_", str_to_lower(str_replace_all(
                                     str_remove(current_section, "\\s*\\([a-z]\\)$"),
                                     "\\s+", "_")))),
            tenure        = "renter_lower_income",
            breakdown_var = "nhha_location",
            breakdown_val = ifelse(is.na(current_location), "Total", current_location),
            geography     = state_cols[j],
            stat_type     = "nhha"
          )
        }
      }
    }
  }

  bind_rows(results)
}, error = function(e) {
  warning("Error processing File 13: ", conditionMessage(e))
  tibble()
})

if (nrow(f13_result) > 0) {
  write_pipeline_csv(f13_result, "sih_nhha_rental_stress.csv")
}

cat("--- SIH processing complete ---\n")

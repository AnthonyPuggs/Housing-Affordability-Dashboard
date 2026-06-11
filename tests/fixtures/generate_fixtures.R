# Regenerates the frozen test fixture data under tests/fixtures/data from the
# currently saved data/*.csv. Run manually from the repository root when the
# fixture set needs a deliberate refresh:
#
#   Rscript tests/fixtures/generate_fixtures.R
#
# The fixtures exist so unit/module tests that boot plot_setup.R are immune to
# scheduled data refreshes (review TEST-04); live-data contract tests keep
# reading data/ directly. Slicing rules keep every file schema-complete and
# non-empty while staying small enough to commit.

suppressPackageStartupMessages({
  library(dplyr)
})

source(file.path("R", "project_paths.R"))

src_dir <- project_path("data")
out_dir <- project_path("tests", "fixtures", "data")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

slice_time_series <- function(filename, min_date) {
  d <- read.csv(file.path(src_dir, filename), stringsAsFactors = FALSE)
  d$date <- as.Date(d$date)
  d <- d[d$date >= as.Date(min_date), , drop = FALSE]
  write.csv(d, file.path(out_dir, filename), row.names = FALSE)
  cat(sprintf("  %-32s %6d rows\n", filename, nrow(d)))
}

copy_whole <- function(filename) {
  file.copy(file.path(src_dir, filename), file.path(out_dir, filename),
            overwrite = TRUE)
  d <- read.csv(file.path(out_dir, filename), stringsAsFactors = FALSE)
  cat(sprintf("  %-32s %6d rows (full copy)\n", filename, nrow(d)))
}

slice_sih <- function(filename, max_rows = 800) {
  d <- read.csv(file.path(src_dir, filename), stringsAsFactors = FALSE)
  # Keep the latest survey years first so default page views have data.
  years <- sort(unique(d$survey_year), decreasing = TRUE)
  keep <- d[d$survey_year %in% head(years, 3), , drop = FALSE]
  if (nrow(keep) > max_rows) {
    keep <- head(keep, max_rows)
  }
  if (nrow(keep) == 0) {
    keep <- head(d, max_rows)
  }
  write.csv(keep, file.path(out_dir, filename), row.names = FALSE)
  cat(sprintf("  %-32s %6d rows\n", filename, nrow(keep)))
}

cat("Writing fixtures to", out_dir, "\n")

# Live time series: enough history for the splice overlap window (2019-07 to
# 2021-06), quarterly joins and YoY transforms.
slice_time_series("abs_timeseries.csv", "2018-01-01")
slice_time_series("rba_rates.csv", "2018-01-01")
slice_time_series("abs_supply_demand.csv", "2018-01-01")

# Derived indicators incl. the score history: small, keep complete.
copy_whole("affordability_indices.csv")
copy_whole("data_vintage.csv")

# SIH cross-sections: small files are copied whole so cross-file joins stay
# relationally coherent — in particular sih_estimate_quality.csv must cover
# every estimate row or join_sih_quality() produces frames that have broken
# ggplotly conversion before. Only the two large files are sliced, by survey
# year (never head(), which cuts mid-table).
copy_whole("sih_timeseries_national.csv")
copy_whole("sih_costs_2020.csv")
copy_whole("sih_cost_ratios_2020.csv")
copy_whole("sih_stress_bands_2020.csv")
copy_whole("sih_nhha_rental_stress.csv")
copy_whole("sih_estimate_quality.csv")
copy_whole("sih_lower_income_states.csv")
copy_whole("sih_recent_buyers_2020.csv")
copy_whole("sih_age_tenure_2020.csv")
slice_sih("sih_state_timeseries.csv", max_rows = Inf)
slice_sih("sih_geographic_2020.csv", max_rows = Inf)

cat("Fixture generation complete.\n")

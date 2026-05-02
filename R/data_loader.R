# Dashboard CSV loading helpers.

load_dashboard_csv <- function(filename, data_dir = project_path("data")) {
  path <- file.path(data_dir, filename)
  if (file.exists(path)) {
    readr::read_csv(path, show_col_types = FALSE)
  } else {
    data.frame()
  }
}

load_dashboard_csvs <- function(data_dir = project_path("data")) {
  list(
    abs_ts = load_dashboard_csv("abs_timeseries.csv", data_dir),
    rba_rates = load_dashboard_csv("rba_rates.csv", data_dir),
    afford_idx = load_dashboard_csv("affordability_indices.csv", data_dir),
    supply_demand = load_dashboard_csv("abs_supply_demand.csv", data_dir),
    sih_national = load_dashboard_csv("sih_timeseries_national.csv", data_dir),
    sih_state_ts = load_dashboard_csv("sih_state_timeseries.csv", data_dir),
    sih_costs = load_dashboard_csv("sih_costs_2020.csv", data_dir),
    sih_cost_ratios = load_dashboard_csv("sih_cost_ratios_2020.csv", data_dir),
    sih_stress = load_dashboard_csv("sih_stress_bands_2020.csv", data_dir),
    sih_nhha = load_dashboard_csv("sih_nhha_rental_stress.csv", data_dir),
    sih_quality = load_dashboard_csv("sih_estimate_quality.csv", data_dir),
    sih_lower_income_states = load_dashboard_csv(
      "sih_lower_income_states.csv", data_dir
    ),
    sih_geographic = load_dashboard_csv("sih_geographic_2020.csv", data_dir)
  )
}

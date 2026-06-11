# Runs standalone via `Rscript tests/test_contextual_kpi_helpers.R` (repo root) and under
# testthat::test_dir("tests"); see tests/helper-contracts.R.
if (!exists("contracts_harness_loaded", mode = "function")) {
  source(file.path(if (basename(getwd()) == "tests") "." else "tests",
                   "helper-contracts.R"))
}

test_that("contextual_kpi_helpers contracts", {
  repo_root <- repo_root_path()


  helper_path <- file.path(repo_root, "R", "contextual_kpi_helpers.R")
  check(file.exists(helper_path), "R/contextual_kpi_helpers.R does not exist")

  if (file.exists(helper_path)) {
    parsed <- tryCatch({
      parse(helper_path)
      TRUE
    }, error = function(e) conditionMessage(e))
    check(identical(parsed, TRUE),
          paste(helper_path, "does not parse:", parsed))

    suppressPackageStartupMessages({
      library(dplyr)
      library(stringr)
    })
    source(helper_path, local = TRUE)

    required_functions <- c(
      "latest_capital_price_extreme",
      "contextual_approval_series_components",
      "selected_approvals_latest",
      "largest_selected_approval",
      "selected_approvals_yoy_change",
      "available_supply_states"
    )
    missing_functions <- required_functions[
      !exists_in(required_functions, environment())
    ]
    check(length(missing_functions) == 0,
          paste("Contextual KPI helper missing functions:",
                paste(missing_functions, collapse = ", ")))

    price_data <- data.frame(
      date = as.Date(c("2026-03-01", "2026-03-01", "2026-03-01",
                       "2025-03-01")),
      city = c("Sydney", "Melbourne", "National Avg", "Sydney"),
      value = c(1500, 1100, 950, 1300),
      stringsAsFactors = FALSE
    )
    if (exists("latest_capital_price_extreme", mode = "function")) {
      high <- latest_capital_price_extreme(price_data, direction = "highest")
      check(identical(high$city[1], "Sydney"),
            "Highest capital price helper must exclude National Avg and return highest latest capital")
      check(identical(high$value[1], 1500),
            "Highest capital price helper returned the wrong value")
    }

    approvals <- data.frame(
      date = as.Date(rep(c("2026-03-01", "2025-03-01"), each = 3)),
      category = "Building Approvals",
      series = rep(c(
        "Building Approvals; New South Wales; Total (Type of Building); Total Sectors",
        "Building Approvals; Victoria; Total (Type of Building); Total Sectors",
        "Building Approvals; Queensland; Houses; Total Sectors"
      ), times = 2),
      value = c(5000, 4000, 3000, 2500, 2000, 1200),
      stringsAsFactors = FALSE
    )

    if (exists("contextual_approval_series_components", mode = "function")) {
      components <- contextual_approval_series_components(unique(approvals$series))
      check(all(c("approval_state", "approval_building_type",
                  "approval_sector", "approval_label") %in% names(components)),
            "Approval component parser must return state, type, sector and label columns")
      check("Queensland" %in% components$approval_state,
            "Approval parser must preserve non-NSW/VIC jurisdictions")
    }

    if (exists("selected_approvals_latest", mode = "function")) {
      selected <- selected_approvals_latest(
        approvals,
        states = c("New South Wales", "Victoria"),
        building_type = "Total approvals",
        sector = "Total sectors"
      )
      check(identical(selected$value[1], 9000),
            "Selected approvals helper must sum latest selected jurisdictions")
    }

    if (exists("largest_selected_approval", mode = "function")) {
      largest <- largest_selected_approval(
        approvals,
        states = c("New South Wales", "Victoria"),
        building_type = "Total approvals",
        sector = "Total sectors"
      )
      check(identical(largest$approval_state[1], "New South Wales"),
            "Largest selected jurisdiction helper must identify the highest latest jurisdiction")
    }

    if (exists("selected_approvals_yoy_change", mode = "function")) {
      yoy <- selected_approvals_yoy_change(
        approvals,
        states = c("New South Wales", "Victoria"),
        building_type = "Total approvals",
        sector = "Total sectors"
      )
      check(isTRUE(all.equal(yoy$change[1], 100)),
            "Selected approvals YoY change should compare selected latest total with 12-month-prior selected total")

      # A missing t-12 observation must blank the YoY label, never fall back
      # to the immediately preceding observation.
      approvals_gap <- approvals
      approvals_gap$date[approvals_gap$date == as.Date("2025-03-01")] <-
        as.Date("2025-12-01")
      yoy_gap <- selected_approvals_yoy_change(
        approvals_gap,
        states = c("New South Wales", "Victoria"),
        building_type = "Total approvals",
        sector = "Total sectors"
      )
      check(is.na(yoy_gap$change[1]),
            "YoY change must be NA when the exact 12-month-prior observation is missing")
      check(identical(yoy_gap$label[1], ""),
            "YoY label must be blank when the exact 12-month-prior observation is missing")

      # A Feb-29 latest date must roll back to Feb-28 rather than erroring or
      # blanking (the previous sprintf date construction produced NA here).
      approvals_leap <- approvals
      approvals_leap$date[approvals_leap$date == as.Date("2026-03-01")] <-
        as.Date("2028-02-29")
      approvals_leap$date[approvals_leap$date == as.Date("2025-03-01")] <-
        as.Date("2027-02-28")
      yoy_leap <- selected_approvals_yoy_change(
        approvals_leap,
        states = c("New South Wales", "Victoria"),
        building_type = "Total approvals",
        sector = "Total sectors"
      )
      check(isTRUE(all.equal(yoy_leap$change[1], 100)),
            "YoY change must roll a Feb-29 latest date back to the Feb-28 prior observation")
    }

    if (exists("available_supply_states", mode = "function")) {
      states <- available_supply_states(approvals)
      check(all(c("New South Wales", "Queensland", "Victoria") %in% states),
            "available_supply_states() must derive all jurisdictions from approvals data")
    }
  }
})
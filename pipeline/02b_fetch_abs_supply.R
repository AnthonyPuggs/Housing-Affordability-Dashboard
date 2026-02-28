# ==============================================================================
# 02b_fetch_abs_supply.R — Fetch ABS population & building approvals
# ==============================================================================
# Source:  ABS via readabs package
# Output:  data/abs_supply_demand.csv
#
# Schema: date | value | series | series_id | category | unit | frequency
# ==============================================================================

cat("--- Fetching ABS supply & demand indicators ---\n")

supply_series <- list()

# ==============================================================================
# 1. Population estimates — ABS 3101.0
# ==============================================================================
cat("  Fetching Population (3101.0)...\n")

pop_raw <- safe_read(
  read_abs(cat_no = "3101.0", tables = "1"),
  "Population 3101.0 Table 1"
)

if (nrow(pop_raw) > 0) {
  # Estimated Resident Population by state
  pop_erp <- pop_raw %>%
    filter(str_detect(series, regex("estimated resident population",
                                    ignore_case = TRUE))) %>%
    normalize_abs(category = "Population", units = "Persons",
                  freq_hint = "Quarter")
  supply_series$pop_erp <- pop_erp
  cat("    ERP:", nrow(pop_erp), "obs\n")

  # Natural increase
  pop_natural <- pop_raw %>%
    filter(str_detect(series, regex("natural increase", ignore_case = TRUE))) %>%
    normalize_abs(category = "Population", units = "Persons",
                  freq_hint = "Quarter")
  supply_series$pop_natural <- pop_natural

  # Net overseas migration
  pop_nom <- pop_raw %>%
    filter(str_detect(series, regex("net overseas migration",
                                    ignore_case = TRUE))) %>%
    normalize_abs(category = "Population", units = "Persons",
                  freq_hint = "Quarter")
  supply_series$pop_nom <- pop_nom
  cat("    NOM:", nrow(pop_nom), "obs\n")
}

# Try Table 2 for state-level components if Table 1 didn't have NOM breakdown
if (is.null(supply_series$pop_nom) || nrow(supply_series$pop_nom) == 0) {
  cat("  Trying 3101.0 Table 2 for NOM by state...\n")
  pop_raw2 <- safe_read(
    read_abs(cat_no = "3101.0", tables = "2"),
    "Population 3101.0 Table 2"
  )
  if (nrow(pop_raw2) > 0) {
    pop_nom2 <- pop_raw2 %>%
      filter(str_detect(series, regex("net overseas migration",
                                      ignore_case = TRUE))) %>%
      normalize_abs(category = "Population", units = "Persons",
                    freq_hint = "Quarter")
    supply_series$pop_nom <- pop_nom2
    cat("    NOM from Table 2:", nrow(pop_nom2), "obs\n")
  }
}

# ==============================================================================
# 2. Building Approvals — ABS 8731.0
# ==============================================================================
cat("  Fetching Building Approvals (8731.0)...\n")

# Table 1: Number of dwelling units approved by sector (trend/seasonally adj)
approvals_raw <- safe_read(
  read_abs(cat_no = "8731.0", tables = "1"),
  "Building Approvals 8731.0 Table 1"
)

if (nrow(approvals_raw) > 0) {
  # Total dwellings approved
  approvals_total <- approvals_raw %>%
    filter(str_detect(series, regex("total.*dwell|number of dwelling",
                                    ignore_case = TRUE))) %>%
    normalize_abs(category = "Building Approvals", units = "Number",
                  freq_hint = "Month")
  supply_series$approvals_total <- approvals_total
  cat("    Total approvals:", nrow(approvals_total), "obs\n")

  # Houses
  approvals_houses <- approvals_raw %>%
    filter(str_detect(series, regex("house", ignore_case = TRUE)),
           !str_detect(series, regex("total|other|value", ignore_case = TRUE))) %>%
    normalize_abs(category = "Building Approvals", units = "Number",
                  freq_hint = "Month")
  supply_series$approvals_houses <- approvals_houses

  # Other residential (apartments/units)
  approvals_other <- approvals_raw %>%
    filter(str_detect(series, regex("other residential|dwelling units excluding",
                                    ignore_case = TRUE))) %>%
    normalize_abs(category = "Building Approvals", units = "Number",
                  freq_hint = "Month")
  supply_series$approvals_other <- approvals_other
  cat("    Houses:", nrow(approvals_houses), "obs |",
      "Other:", nrow(approvals_other), "obs\n")
}

# Table 2: Value of building work approved by state (if available)
approvals_state_raw <- safe_read(
  read_abs(cat_no = "8731.0", tables = "2"),
  "Building Approvals 8731.0 Table 2",
  warn = FALSE
)

if (nrow(approvals_state_raw) > 0) {
  approvals_state <- approvals_state_raw %>%
    filter(str_detect(series, regex("number.*dwell|dwell.*number",
                                    ignore_case = TRUE)),
           !str_detect(series, regex("value|\\$", ignore_case = TRUE))) %>%
    normalize_abs(category = "Building Approvals", units = "Number",
                  freq_hint = "Month")
  supply_series$approvals_state <- approvals_state
  cat("    State approvals:", nrow(approvals_state), "obs\n")
}

# ==============================================================================
# Combine and write
# ==============================================================================
abs_supply <- bind_rows(supply_series) %>%
  distinct(date, series, .keep_all = TRUE) %>%
  arrange(category, series, date)

write_pipeline_csv(abs_supply, "abs_supply_demand.csv")

cat("--- ABS supply & demand fetch complete ---\n")
cat("  Total series:", length(unique(abs_supply$series)), "\n")
if (nrow(abs_supply) > 0) {
  cat("  Date range:", as.character(min(abs_supply$date, na.rm = TRUE)),
      "to", as.character(max(abs_supply$date, na.rm = TRUE)), "\n")
}

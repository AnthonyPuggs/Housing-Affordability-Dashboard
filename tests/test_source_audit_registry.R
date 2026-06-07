repo_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
failures <- character()

check <- function(condition, message) {
  if (!isTRUE(condition)) {
    failures <<- c(failures, message)
  }
}

audit_path <- file.path(repo_root, "R", "source_audit_registry.R")
check(file.exists(audit_path), "R/source_audit_registry.R does not exist")

if (file.exists(audit_path)) {
  parsed <- tryCatch({
    parse(audit_path)
    TRUE
  }, error = function(e) conditionMessage(e))
  check(identical(parsed, TRUE),
        paste(audit_path, "does not parse:", parsed))

  source(audit_path)
  check(exists("source_audit_registry", mode = "function"),
        "source_audit_registry() must be defined")
  check(exists("source_audit_status_levels", mode = "function"),
        "source_audit_status_levels() must be defined")
  check(exists("source_audit_methodology_table", mode = "function"),
        "source_audit_methodology_table() must be defined")

  if (exists("source_audit_registry", mode = "function")) {
    audit <- source_audit_registry()
    required_columns <- c(
      "source_id", "gap", "source_name", "provider", "status",
      "reason", "coverage", "update_frequency", "dashboard_implication",
      "url"
    )
    missing_columns <- setdiff(required_columns, names(audit))
    check(length(missing_columns) == 0,
          paste("Source audit registry missing columns:",
                paste(missing_columns, collapse = ", ")))

    expected_gaps <- c(
      "household_disposable_income",
      "new_tenancy_rent",
      "residual_income_living_cost"
    )
    check(all(expected_gaps %in% audit$gap),
          "Source audit registry must cover income, new-tenancy rent and residual-income/living-cost gaps")
    check(any(audit$status == "candidate"),
          "At least one source audit row must be marked candidate")
    check(any(audit$status == "not suitable yet"),
          "At least one source audit row must be marked not suitable yet")
    check(!any(duplicated(audit$source_id)),
          "Source audit source_id values must be unique")
    check(all(grepl("^https://", audit$url)),
          "Source audit URLs must be HTTPS official-source links")

    required_sources <- c(
      "abs_household_income_wealth",
      "abs_housing_occupancy_costs",
      "abs_cpi_slci_housing_faq",
      "abs_selected_living_cost_indexes",
      "nsw_rental_bond_lodgements",
      "qld_rta_median_rents"
    )
    missing_sources <- setdiff(required_sources, audit$source_id)
    check(length(missing_sources) == 0,
          paste("Source audit missing required source IDs:",
                paste(missing_sources, collapse = ", ")))
  }

  if (exists("source_audit_status_levels", mode = "function")) {
    check(identical(source_audit_status_levels(),
                    c("ready", "candidate", "not suitable yet")),
          "source_audit_status_levels() must expose the accepted status levels in display order")
  }

  if (exists("source_audit_methodology_table", mode = "function")) {
    table <- source_audit_methodology_table()
    check(all(c("Gap", "Candidate Source", "Status", "Coverage",
                "Dashboard Implication") %in% names(table)),
          "source_audit_methodology_table() must return dashboard-display columns")
  }
}

if (length(failures) > 0) {
  stop(
    paste(c("Source audit registry checks failed:", paste0("- ", failures)),
          collapse = "\n"),
    call. = FALSE
  )
}

cat("Source audit registry checks passed.\n")

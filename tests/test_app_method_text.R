# Runs standalone via `Rscript tests/test_app_method_text.R` (repo root) and under
# testthat::test_dir("tests"); see tests/helper-contracts.R.
if (!exists("contracts_harness_loaded", mode = "function")) {
  source(file.path(if (basename(getwd()) == "tests") "." else "tests",
                   "helper-contracts.R"))
}

test_that("app_method_text contracts", {
  repo_root <- repo_root_path()
  app_path <- file.path(repo_root, "app.R")
  readme_path <- file.path(repo_root, "README.md")
  registry_path <- file.path(repo_root, "R", "indicator_registry.R")
  helper_path <- file.path(repo_root, "R", "app_ui_helpers.R")
  module_path <- file.path(repo_root, "R", "methodology_module.R")
  module_paths <- list.files(file.path(repo_root, "R"),
                             pattern = "_module[.]R$",
                             full.names = TRUE)

  for (path in c(app_path, readme_path, registry_path, helper_path,
                 module_path)) {
    check(file.exists(path), paste(path, "not found"))
  }

  app_text <- paste(readLines(app_path, warn = FALSE), collapse = "\n")
  readme_text <- paste(readLines(readme_path, warn = FALSE), collapse = "\n")
  registry_text <- paste(readLines(registry_path, warn = FALSE), collapse = "\n")
  helper_text <- paste(readLines(helper_path, warn = FALSE), collapse = "\n")
  module_text <- paste(vapply(unique(c(module_path, module_paths)), function(path) {
    paste(readLines(path, warn = FALSE), collapse = "\n")
  }, character(1)), collapse = "\n")
  method_text <- paste(app_text, readme_text, registry_text, helper_text,
                       module_text, sep = "\n")

  required_text <- c(
    "higher = less affordable",
    "Stylised scenario, not an official ABS measure or lender assessment",
    "Assessment buffer and expense inputs are sensitivity assumptions, not a lender assessment",
    "ABS Survey of Income and Housing",
    "ABS CPI",
    "RBA",
    "Modelled Mortgage Cost Pressure",
    "Stylised Deposit Gap (Years)",
    "R/market_entry_scenarios.R",
    "KPI colours encode economic interpretation",
    "better, worse or neutral/contextual",
    "relative standard error",
    "margin of error",
    "interpret with caution"
  )

  missing <- required_text[!vapply(required_text, grepl, logical(1), method_text,
                                   fixed = TRUE)]
  check(length(missing) == 0,
        paste("Methodology surfaces are missing required text:",
              paste(missing, collapse = "; ")))

  registry_reference_text <- paste(app_text, readme_text, sep = "\n")
  registry_contract_parts <- c(
    "R/indicator_registry.R",
    "source of truth for derived indicator formulas, source series, units, interpretation direction and caveats"
  )
  missing_contract <- registry_contract_parts[
    !vapply(registry_contract_parts, grepl, logical(1), registry_reference_text,
            fixed = TRUE)
  ]
  check(length(missing_contract) == 0,
        "Methodology text must point to R/indicator_registry.R as the formula/source metadata location.")
})

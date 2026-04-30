app_path <- file.path(getwd(), "app.R")
if (!file.exists(app_path)) {
  stop("app.R not found; run this test from the project root.", call. = FALSE)
}
readme_path <- file.path(getwd(), "README.md")
if (!file.exists(readme_path)) {
  stop("README.md not found; run this test from the project root.", call. = FALSE)
}
registry_path <- file.path(getwd(), "R", "indicator_registry.R")
if (!file.exists(registry_path)) {
  stop("R/indicator_registry.R not found; run this test from the project root.",
       call. = FALSE)
}

app_text <- paste(readLines(app_path, warn = FALSE), collapse = "\n")
readme_text <- paste(readLines(readme_path, warn = FALSE), collapse = "\n")
registry_text <- paste(readLines(registry_path, warn = FALSE), collapse = "\n")
method_text <- paste(app_text, readme_text, registry_text, sep = "\n")

required_text <- c(
  "higher = less affordable",
  "Stylised scenario, not an official ABS measure or lender assessment",
  "ABS Survey of Income and Housing",
  "ABS CPI",
  "RBA",
  "Modelled Mortgage Cost Pressure",
  "Stylised Deposit Gap (Years)"
)

missing <- required_text[!vapply(required_text, grepl, logical(1), method_text,
                                 fixed = TRUE)]

if (length(missing) > 0) {
  stop(
    "Methodology surfaces are missing required text: ",
    paste(missing, collapse = "; "),
    call. = FALSE
  )
}

registry_reference_text <- paste(app_text, readme_text, sep = "\n")
registry_contract_parts <- c(
  "R/indicator_registry.R",
  "source of truth for derived indicator formulas, source series, units, interpretation direction and caveats"
)
missing_contract <- registry_contract_parts[
  !vapply(registry_contract_parts, grepl, logical(1), registry_reference_text,
          fixed = TRUE)
]
if (length(missing_contract) > 0) {
  stop(
    "Methodology text must point to R/indicator_registry.R as the formula/source metadata location.",
    call. = FALSE
  )
}

cat("App methodology text checks passed.\n")

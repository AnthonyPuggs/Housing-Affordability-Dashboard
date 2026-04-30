app_path <- file.path(getwd(), "app.R")
if (!file.exists(app_path)) {
  stop("app.R not found; run this test from the project root.", call. = FALSE)
}

app_text <- paste(readLines(app_path, warn = FALSE), collapse = "\n")

required_text <- c(
  "higher = less affordable",
  "Stylised scenario, not an official ABS measure or lender assessment",
  "ABS Survey of Income and Housing",
  "ABS CPI",
  "RBA",
  "Modelled Mortgage Cost Pressure",
  "Stylised Deposit Gap (Years)"
)

missing <- required_text[!vapply(required_text, grepl, logical(1), app_text,
                                 fixed = TRUE)]

if (length(missing) > 0) {
  stop(
    "app.R is missing required methodology text: ",
    paste(missing, collapse = "; "),
    call. = FALSE
  )
}

cat("App methodology text checks passed.\n")

repo_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
scenario_path <- file.path(repo_root, "R", "market_entry_scenarios.R")
module_path <- file.path(repo_root, "R", "affordability_module.R")
failures <- character()

check <- function(condition, message) {
  if (!isTRUE(condition)) {
    failures <<- c(failures, message)
  }
}

check(file.exists(scenario_path), "R/market_entry_scenarios.R does not exist")
check(file.exists(module_path), "R/affordability_module.R does not exist")

if (file.exists(scenario_path)) {
  parsed <- tryCatch({
    parse(scenario_path)
    TRUE
  }, error = function(e) conditionMessage(e))
  check(identical(parsed, TRUE),
        paste("R/market_entry_scenarios.R does not parse:", parsed))

  source(scenario_path)

  price_ts <- data.frame(
    date = as.Date(c("2024-01-01", "2024-04-01")),
    price_k = c(800, 820)
  )
  income_ts <- data.frame(
    date = as.Date(c("2024-01-01", "2024-04-01")),
    awe = c(2300, 2320)
  )
  rate_ts <- data.frame(
    date = as.Date(c("2024-01-01", "2024-04-01")),
    rate = c(6, 6.25)
  )

  base <- tryCatch(
    market_entry_serviceability_series(
      price_ts = price_ts,
      income_ts = income_ts,
      rate_ts = rate_ts,
      deposit_pct = 20,
      term_years = 30,
      assessment_buffer_pp = 3
    ),
    error = function(e) e
  )
  check(!inherits(base, "error"),
        paste("deposit_pct serviceability call failed:",
              if (inherits(base, "error")) conditionMessage(base) else ""))

  if (!inherits(base, "error")) {
    check(nrow(base) == 2 * nrow(price_ts),
          "Default serviceability output must keep two scenario rows per date")
    check(all(c("Nominal rate", "Assessed rate") %in% base$scenario),
          "Serviceability output must keep nominal and assessed scenarios")

    lower_deposit <- market_entry_serviceability_series(
      price_ts = price_ts,
      income_ts = income_ts,
      rate_ts = rate_ts,
      deposit_pct = 10,
      term_years = 30,
      assessment_buffer_pp = 3
    )
    base_nominal <- base$serviceability_pct[base$scenario == "Nominal rate"]
    lower_deposit_nominal <- lower_deposit$serviceability_pct[
      lower_deposit$scenario == "Nominal rate"
    ]
    check(all(lower_deposit_nominal > base_nominal),
          "Lower deposit / higher LVR must raise nominal repayment burden")

    shorter_term <- market_entry_serviceability_series(
      price_ts = price_ts,
      income_ts = income_ts,
      rate_ts = rate_ts,
      deposit_pct = 20,
      term_years = 20,
      assessment_buffer_pp = 3
    )
    shorter_term_nominal <- shorter_term$serviceability_pct[
      shorter_term$scenario == "Nominal rate"
    ]
    check(all(shorter_term_nominal > base_nominal),
          "Shorter loan term must raise nominal repayment burden")

    no_buffer <- market_entry_serviceability_series(
      price_ts = price_ts,
      income_ts = income_ts,
      rate_ts = rate_ts,
      deposit_pct = 20,
      term_years = 30,
      assessment_buffer_pp = 0
    )
    no_buffer_nominal <- no_buffer$serviceability_pct[
      no_buffer$scenario == "Nominal rate"
    ]
    no_buffer_assessed <- no_buffer$serviceability_pct[
      no_buffer$scenario == "Assessed rate"
    ]
    base_assessed <- base$serviceability_pct[base$scenario == "Assessed rate"]
    check(all(abs(no_buffer_nominal - no_buffer_assessed) < 1e-8),
          "Zero assessment buffer must make nominal and assessed burdens equal")
    check(all(base_assessed > base_nominal),
          "Positive assessment buffer must raise assessed repayment burden")
  }

  invalid_cases <- list(
    function() market_entry_serviceability_series(
      price_ts = price_ts,
      income_ts = income_ts,
      rate_ts = rate_ts,
      deposit_pct = 0
    ),
    function() market_entry_serviceability_series(
      price_ts = price_ts,
      income_ts = income_ts,
      rate_ts = rate_ts,
      deposit_pct = 100
    ),
    function() market_entry_serviceability_series(
      price_ts = price_ts,
      income_ts = income_ts,
      rate_ts = rate_ts,
      term_years = 0
    )
  )
  invalid_messages <- vapply(invalid_cases, function(case) {
    tryCatch({
      case()
      ""
    }, error = function(e) conditionMessage(e))
  }, character(1))
  check(all(nzchar(invalid_messages)),
        "Invalid serviceability scenario controls must fail clearly")
}

if (file.exists(module_path)) {
  module_text <- paste(readLines(module_path, warn = FALSE), collapse = "\n")
  required_module_text <- c(
    'sliderInput(ns("serviceability_deposit_pct")',
    'sliderInput(ns("serviceability_term")',
    "deposit_pct = input$serviceability_deposit_pct",
    "term_years = input$serviceability_term",
    "uses AWE individual earnings as the income proxy",
    "not an official ABS measure or lender assessment",
    "bindCache(input$afford_indices, input$afford_dates, input$serviceability_deposit_pct, input$serviceability_term, input$serviceability_buffer, is_dark())"
  )
  missing_module_text <- required_module_text[
    !vapply(required_module_text, grepl, logical(1), module_text, fixed = TRUE)
  ]
  check(length(missing_module_text) == 0,
        paste("Affordability module missing serviceability scenario controls:",
              paste(missing_module_text, collapse = "; ")))
}

if (length(failures) > 0) {
  stop(
    paste(c("Serviceability scenario control checks failed:",
            paste0("- ", failures)),
          collapse = "\n"),
    call. = FALSE
  )
}

cat("Serviceability scenario control checks passed.\n")

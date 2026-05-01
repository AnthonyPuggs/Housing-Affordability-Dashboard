repo_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
failures <- character()

check <- function(condition, message) {
  if (!isTRUE(condition)) {
    failures <<- c(failures, message)
  }
}

app_path <- file.path(repo_root, "app.R")
description_path <- file.path(repo_root, "DESCRIPTION")
readme_path <- file.path(repo_root, "README.md")

check(file.exists(app_path), "app.R does not exist")
check(file.exists(description_path), "DESCRIPTION does not exist")
check(file.exists(readme_path), "README.md does not exist")

if (file.exists(app_path)) {
  app_text <- paste(readLines(app_path, warn = FALSE), collapse = "\n")

  check(!grepl("font_google\\s*\\(", app_text, perl = TRUE),
        "app.R must not use Google-hosted fonts")
  check(grepl("input_dark_mode\\s*\\(\\s*id\\s*=\\s*\"theme_mode\"",
              app_text, perl = TRUE),
        'app.R must use bslib::input_dark_mode(id = "theme_mode")')

  forbidden_theme_js <- c(
    "window.localStorage.getItem('afford_theme')",
    "window.localStorage.setItem('afford_theme'",
    "function setTheme(theme)",
    "document.documentElement.setAttribute('data-theme'",
    "document.getElementById('theme_toggle')",
    "$(document).on('click', '#theme_toggle'"
  )
  present_forbidden <- forbidden_theme_js[
    vapply(forbidden_theme_js, grepl, logical(1), app_text, fixed = TRUE)
  ]
  check(length(present_forbidden) == 0,
        paste("Custom theme persistence/toggling script remains:",
              paste(present_forbidden, collapse = ", ")))

  check(grepl("is_dark\\s*<-\\s*reactive", app_text, perl = TRUE),
        "app.R must keep the is_dark reactive contract")

  required_module_calls <- c(
    'affordabilityPageServer("affordability", is_dark = is_dark)',
    'rentalMarketPageServer("rental_market", is_dark = is_dark)',
    'housingSupplyPageServer("housing_supply", is_dark = is_dark)',
    'priceTrendsPageServer("price_trends", is_dark = is_dark)',
    'marketContextPageServer("market_context", is_dark = is_dark)',
    'overviewPageServer("overview", is_dark = is_dark)'
  )
  missing_module_calls <- required_module_calls[
    !vapply(required_module_calls, grepl, logical(1), app_text, fixed = TRUE)
  ]
  check(length(missing_module_calls) == 0,
        paste("Module calls must still receive is_dark:",
              paste(missing_module_calls, collapse = ", ")))
}

if (file.exists(description_path)) {
  desc_text <- paste(readLines(description_path, warn = FALSE), collapse = "\n")
  forbidden_direct_dependencies <- c(
    "shinyjs",
    "htmltools",
    "waiter",
    "shinycssloaders",
    "DT",
    "showtext",
    "sysfonts"
  )
  unexpected <- forbidden_direct_dependencies[
    vapply(forbidden_direct_dependencies, function(pkg) {
      grepl(paste0("(^|\\n)\\s*", pkg, "\\s*,?\\s*($|\\n)"),
            desc_text, perl = TRUE)
    }, logical(1))
  ]
  check(length(unexpected) == 0,
        paste("DESCRIPTION must not add new direct dependencies:",
              paste(unexpected, collapse = ", ")))
}

if (file.exists(readme_path)) {
  readme_text <- paste(readLines(readme_path, warn = FALSE), collapse = "\n")
  check(grepl("bslib-native dark/light mode", readme_text, fixed = TRUE),
        "README.md must document bslib-native dark/light mode")
  check(grepl("system font", readme_text, ignore.case = TRUE),
        "README.md must document the system/local font stack")
  check(grepl("Rscript tests/test_theme_infrastructure.R",
              readme_text, fixed = TRUE),
        "README.md must document the theme infrastructure test command")
}

if (length(failures) > 0) {
  stop(
    paste(c("Theme infrastructure checks failed:", paste0("- ", failures)),
          collapse = "\n"),
    call. = FALSE
  )
}

cat("Theme infrastructure checks passed.\n")

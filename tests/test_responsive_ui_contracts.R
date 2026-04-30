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

  page_navbar_head <- regmatches(
    app_text,
    regexpr("page_navbar\\s*\\([^\\)]{0,300}", app_text, perl = TRUE)
  )
  check(grepl('id\\s*=\\s*"main_nav"', page_navbar_head, perl = TRUE),
        'page_navbar() must use id = "main_nav"')

  required_nav_script <- c(
    "bootstrap.Collapse",
    ".navbar-collapse.show",
    "window.innerWidth < 992",
    "main_nav"
  )
  missing_nav_script <- required_nav_script[
    !vapply(required_nav_script, grepl, logical(1), app_text, fixed = TRUE)
  ]
  check(length(missing_nav_script) == 0,
        paste("Mobile navbar collapse script missing:",
              paste(missing_nav_script, collapse = ", ")))

  required_mobile_css <- c(
    "@media (max-width: 768px)",
    ".navbar-collapse.show",
    "max-height: 70vh",
    "overflow-y: auto",
    ".chart-wide",
    ".chart-square",
    "min-height: 320px"
  )
  missing_mobile_css <- required_mobile_css[
    !vapply(required_mobile_css, grepl, logical(1), app_text, fixed = TRUE)
  ]
  check(length(missing_mobile_css) == 0,
        paste("Responsive mobile CSS missing:",
              paste(missing_mobile_css, collapse = ", ")))

  rental_start <- regexpr('nav_panel\\(\\s*"Rental Market"', app_text,
                          perl = TRUE)
  rental_segment <- if (rental_start[1] == -1L) {
    ""
  } else {
    substr(app_text, rental_start[1], nchar(app_text))
  }
  check(grepl('layout_column_wrap\\(\\s*width\\s*=\\s*"420px"',
              rental_segment, perl = TRUE),
        'Rental Market layout_column_wrap() must use width = "420px"')
}

if (file.exists(description_path)) {
  desc_text <- paste(readLines(description_path, warn = FALSE), collapse = "\n")
  forbidden_direct_dependencies <- c(
    "shinyjs",
    "htmltools",
    "waiter",
    "shinycssloaders",
    "DT"
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
  check(grepl("Rscript tests/test_responsive_ui_contracts.R",
              readme_text, fixed = TRUE),
        "README.md must document the responsive UI test command")
}

if (length(failures) > 0) {
  stop(
    paste(c("Responsive UI contract checks failed:", paste0("- ", failures)),
          collapse = "\n"),
    call. = FALSE
  )
}

cat("Responsive UI contract checks passed.\n")

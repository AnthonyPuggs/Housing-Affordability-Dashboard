repo_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
failures <- character()

check <- function(condition, message) {
  if (!isTRUE(condition)) {
    failures <<- c(failures, message)
  }
}

helper_path <- file.path(repo_root, "R", "plotly_helpers.R")
plot_setup_path <- file.path(repo_root, "plot_setup.R")
description_path <- file.path(repo_root, "DESCRIPTION")

check(file.exists(helper_path), "R/plotly_helpers.R does not exist")
check(file.exists(plot_setup_path), "plot_setup.R does not exist")
check(file.exists(description_path), "DESCRIPTION does not exist")

if (file.exists(helper_path)) {
  parsed <- tryCatch({
    parse(helper_path)
    TRUE
  }, error = function(e) conditionMessage(e))
  check(identical(parsed, TRUE), paste("R/plotly_helpers.R does not parse:", parsed))
}

if (all(file.exists(c(helper_path, plot_setup_path)))) {
  source(plot_setup_path)
  source(helper_path)

  check(exists("dashboard_ggplotly", mode = "function"),
        "dashboard_ggplotly() must be defined")

  p <- ggplot2::ggplot(
    data.frame(x = 1:3, y = c(1, 4, 9)),
    ggplot2::aes(x = x, y = y)
  ) +
    ggplot2::geom_line()

  light_fig <- dashboard_ggplotly(p, dark = FALSE, tooltip = c("x", "y"))
  dark_fig <- dashboard_ggplotly(p, dark = TRUE, tooltip = c("x", "y"))

  for (fig_name in c("light_fig", "dark_fig")) {
    fig <- get(fig_name)
    check(inherits(fig, "plotly"), paste(fig_name, "must inherit from plotly"))
    check(inherits(fig, "htmlwidget"), paste(fig_name, "must inherit from htmlwidget"))
    check(length(fig$x$data) > 0, paste(fig_name, "must contain at least one trace"))
    check(!is.null(fig$x$layout), paste(fig_name, "must contain a Plotly layout"))
  }
}

if (file.exists(description_path)) {
  desc_text <- paste(readLines(description_path, warn = FALSE), collapse = "\n")
  forbidden_direct_dependencies <- c("memoise", "cachem", "DT", "shinycssloaders")
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

if (length(failures) > 0) {
  stop(
    paste(c("Plotly helper checks failed:", paste0("- ", failures)),
          collapse = "\n"),
    call. = FALSE
  )
}

cat("Plotly helper checks passed.\n")

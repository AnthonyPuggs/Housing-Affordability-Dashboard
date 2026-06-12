# Runs standalone via `Rscript tests/test_app_smoke.R` (repo root) and under
# testthat::test_dir("tests"); see tests/helper-contracts.R.
#
# One shinytest2::AppDriver smoke test (review TEST-05): boots the real app on
# the frozen fixture data, visits every top-level nav panel and asserts no
# Shiny output error is rendered. Headless Chrome is required; the test skips
# (rather than fails) where the environment cannot boot the app so the rest of
# the suite stays runnable on minimal machines. Two environmental cases skip:
# (a) chromote cannot find a browser; (b) AppDriver cannot start the app child
# at all — e.g. when the local R is older than the R the renv packages were
# built with, shinytest2's strict (warn=2) app process promotes shiny's benign
# "package 'shiny' was built under R version x" startup warning to a fatal
# error. CI installs packages under its own R so versions match and neither
# case fires there; a genuine app-boot regression raises a different message
# and still fails.
if (!exists("contracts_harness_loaded", mode = "function")) {
  source(file.path(if (basename(getwd()) == "tests") "." else "tests",
                   "helper-contracts.R"))
}

test_that("app boots and every nav panel renders without shiny errors", {
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("chromote")
  chrome <- tryCatch(chromote::find_chrome(), error = function(e) NULL)
  if (is.null(chrome)) {
    # Chromium-based Edge ships with Windows and drives chromote fine.
    edge_paths <- c(
      file.path(Sys.getenv("ProgramFiles", "C:/Program Files"),
                "Microsoft/Edge/Application/msedge.exe"),
      file.path(Sys.getenv("ProgramFiles(x86)", "C:/Program Files (x86)"),
                "Microsoft/Edge/Application/msedge.exe")
    )
    edge <- edge_paths[file.exists(edge_paths)][1]
    if (!is.na(edge)) {
      withr::local_envvar(c(CHROMOTE_CHROME = edge))
      chrome <- edge
    }
  }
  skip_if(is.null(chrome), "No Chrome/Chromium available for chromote")

  repo_root <- repo_root_path()
  # The AppDriver child process inherits this environment variable, so the
  # app under test reads tests/fixtures/data, not live data/.
  use_fixture_data()

  # Environmental boot failures (no working browser, or a local R/renv-package
  # version drift that turns shiny's "built under R version" startup warning
  # fatal under shinytest2's warn=2 app process) should skip, not fail. A real
  # app-boot regression produces a different message and is re-raised below.
  app <- tryCatch(
    shinytest2::AppDriver$new(
      app_dir = repo_root,
      name = "smoke",
      load_timeout = 120 * 1000,
      timeout = 60 * 1000
    ),
    error = function(e) {
      msg <- conditionMessage(e)
      env_signatures <- c(
        "built under R version",  # local R older than the package build R
        "chromote", "chrome", "msedge", "browser",  # no headless browser
        "DevTools", "WebSocket"                      # browser launch/connect
      )
      if (any(vapply(env_signatures, grepl, logical(1), msg,
                     ignore.case = TRUE))) {
        skip(paste("Shiny app cannot boot under shinytest2 in this environment:",
                   msg))
      }
      stop(e)
    }
  )
  on.exit(app$stop(), add = TRUE)

  nav_panels <- c(
    "Overview",
    "Price Trends",
    "Affordability",
    "Recent Buyers",
    "Geographic Affordability",
    "Market Context",
    "Housing Supply",
    "Rental Market",
    "Methodology"
  )

  for (panel in nav_panels) {
    app$set_inputs(main_nav = panel)
    idle <- tryCatch({
      app$wait_for_idle(timeout = 60 * 1000)
      TRUE
    }, error = function(e) conditionMessage(e))
    if (!isTRUE(idle)) {
      cat("\n--- shiny app log (", panel, ") ---\n", sep = "")
      print(app$get_logs())
    }
    check(isTRUE(idle),
          paste0("Shiny must reach idle after switching to '", panel,
                 "': ", idle))

    check(identical(app$get_value(input = "main_nav"), panel),
          paste("Navbar must switch to panel:", panel))

    errors <- app$get_html(".shiny-output-error", outer_html = TRUE)
    rendered_errors <- errors[nzchar(trimws(gsub("<[^>]+>", "", errors)))]
    check(length(rendered_errors) == 0,
          paste0("Panel '", panel, "' rendered shiny output errors: ",
                 paste(rendered_errors, collapse = " | ")))
  }
})

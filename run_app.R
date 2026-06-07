# Launch the Shiny dashboard from the project root, regardless of the current
# Positron/R console working directory.

script_path <- function() {
  file_args <- commandArgs(trailingOnly = FALSE)
  file_args <- file_args[startsWith(file_args, "--file=")]
  if (length(file_args) > 0) {
    return(sub("^--file=", "", file_args[[1]]))
  }

  frames <- sys.frames()
  for (i in rev(seq_along(frames))) {
    frame <- frames[[i]]
    if (exists("ofile", envir = frame, inherits = FALSE)) {
      return(get("ofile", envir = frame, inherits = FALSE))
    }
  }

  "run_app.R"
}

project_root <- dirname(normalizePath(script_path(), winslash = "/", mustWork = TRUE))
setwd(project_root)

host <- Sys.getenv("DASHBOARD_HOST", "127.0.0.1")
default_port <- suppressWarnings(as.integer(Sys.getenv("DASHBOARD_PORT", "49340")))
if (is.na(default_port) || default_port <= 0) {
  default_port <- 49340L
}

launch_browser <- !tolower(Sys.getenv("DASHBOARD_LAUNCH_BROWSER", "true")) %in%
  c("0", "false", "no")

port_candidates <- default_port + 0:20
last_error <- NULL

for (port in port_candidates) {
  message(sprintf(
    "Starting Australian Housing Affordability Dashboard at http://%s:%d/",
    host,
    port
  ))

  result <- tryCatch(
    shiny::runApp(
      ".",
      host = host,
      port = port,
      launch.browser = launch_browser
    ),
    error = function(error) error
  )

  if (!inherits(result, "error")) {
    last_error <- NULL
    break
  }

  last_error <- result
  server_unavailable <- grepl(
    "Failed to create server|address already in use",
    conditionMessage(result),
    ignore.case = TRUE
  )
  if (!server_unavailable) {
    stop(result)
  }

  message(sprintf("Port %d is unavailable; trying the next port.", port))
}

if (inherits(last_error, "error")) {
  stop(
    sprintf(
      "Could not start the dashboard on ports %d-%d.",
      min(port_candidates),
      max(port_candidates)
    ),
    call. = FALSE
  )
}

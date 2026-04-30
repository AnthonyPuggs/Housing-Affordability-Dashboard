# ==============================================================================
# Project root and path helpers
# ==============================================================================
# Shared by the Shiny app and data pipeline so entrypoints can be launched from
# outside the repository root without relying on getwd().
# ==============================================================================

.current_source_file <- function() {
  frames <- sys.frames()
  for (i in rev(seq_along(frames))) {
    frame <- frames[[i]]
    if (exists("ofile", envir = frame, inherits = FALSE)) {
      path <- get("ofile", envir = frame, inherits = FALSE)
      if (is.character(path) && length(path) == 1 && nzchar(path)) {
        return(normalizePath(path, winslash = "/", mustWork = TRUE))
      }
    }
  }

  args <- commandArgs(trailingOnly = FALSE)
  script_arg <- args[grepl("^--file=", args)]
  if (length(script_arg) > 0) {
    return(normalizePath(sub("^--file=", "", script_arg[[1]]),
                         winslash = "/", mustWork = TRUE))
  }

  NULL
}

.is_project_root <- function(path) {
  file.exists(file.path(path, "app.R")) &&
    file.exists(file.path(path, "plot_setup.R")) &&
    dir.exists(file.path(path, "data")) &&
    dir.exists(file.path(path, "pipeline"))
}

.walk_to_project_root <- function(start) {
  if (is.null(start) || !nzchar(start)) {
    return(NULL)
  }

  start <- normalizePath(start, winslash = "/", mustWork = TRUE)
  if (!dir.exists(start)) {
    start <- dirname(start)
  }

  repeat {
    if (.is_project_root(start)) {
      return(start)
    }
    parent <- dirname(start)
    if (identical(parent, start)) {
      return(NULL)
    }
    start <- parent
  }
}

.find_project_root <- function() {
  env_root <- Sys.getenv("HOUSING_DASHBOARD_ROOT", unset = "")
  if (nzchar(env_root)) {
    root <- normalizePath(env_root, winslash = "/", mustWork = TRUE)
    if (!.is_project_root(root)) {
      stop("HOUSING_DASHBOARD_ROOT does not point to the housing dashboard root: ",
           root, call. = FALSE)
    }
    return(root)
  }

  source_file <- .current_source_file()
  starts <- unique(c(
    if (!is.null(source_file)) dirname(source_file),
    getwd()
  ))

  for (start in starts) {
    root <- .walk_to_project_root(start)
    if (!is.null(root)) {
      return(root)
    }
  }

  stop("Could not locate the housing dashboard project root. Set ",
       "HOUSING_DASHBOARD_ROOT to the repository path.", call. = FALSE)
}

.housing_dashboard_root <- .find_project_root()

project_root <- function() {
  .housing_dashboard_root
}

project_path <- function(...) {
  file.path(project_root(), ...)
}

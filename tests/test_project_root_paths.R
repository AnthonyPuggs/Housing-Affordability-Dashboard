repo_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
failures <- character()

check <- function(condition, message) {
  if (!isTRUE(condition)) {
    failures <<- c(failures, message)
  }
}

expected_columns <- function(object, columns, label) {
  missing <- setdiff(columns, names(object))
  check(length(missing) == 0,
        paste(label, "is missing columns:", paste(missing, collapse = ", ")))
}

paths_file <- file.path(repo_root, "R", "project_paths.R")
check(file.exists(paths_file), "R/project_paths.R does not exist")

if (file.exists(paths_file)) {
  paths_env <- new.env(parent = globalenv())
  source(paths_file, local = paths_env)
  project_root_fn <- get("project_root", envir = paths_env)
  project_path_fn <- get("project_path", envir = paths_env)
  check(identical(normalizePath(project_root_fn(), winslash = "/", mustWork = TRUE), repo_root),
        "project_root() does not resolve to the repository root from project cwd")
  check(identical(normalizePath(project_path_fn("data"), winslash = "/", mustWork = TRUE),
                  file.path(repo_root, "data")),
        "project_path('data') does not resolve to the data directory")
}

old_wd <- getwd()
on.exit(setwd(old_wd), add = TRUE)

plot_env <- new.env(parent = globalenv())
setwd("/private/tmp")
plot_result <- tryCatch(
  {
    source(file.path(repo_root, "plot_setup.R"), local = plot_env)
    TRUE
  },
  error = function(e) conditionMessage(e)
)
check(identical(plot_result, TRUE),
      paste("plot_setup.R failed when sourced from /private/tmp:", plot_result))

if (identical(plot_result, TRUE)) {
  check(exists("abs_ts", envir = plot_env), "plot_setup.R did not create abs_ts")
  check(exists("afford_idx", envir = plot_env), "plot_setup.R did not create afford_idx")
  if (exists("abs_ts", envir = plot_env)) {
    expected_columns(get("abs_ts", envir = plot_env),
                     c("date", "value", "series", "series_id", "category", "unit", "frequency"),
                     "abs_ts")
  }
  if (exists("afford_idx", envir = plot_env)) {
    expected_columns(get("afford_idx", envir = plot_env),
                     c("date", "value", "indicator", "geography", "unit", "frequency"),
                     "afford_idx")
  }
}

app_env <- new.env(parent = globalenv())
setwd("/private/tmp")
app_result <- tryCatch(
  {
    source(file.path(repo_root, "app.R"), local = app_env)
    TRUE
  },
  error = function(e) conditionMessage(e)
)
check(identical(app_result, TRUE),
      paste("app.R failed when sourced from /private/tmp:", app_result))
check(exists("ui", envir = app_env), "app.R did not create ui when sourced from /private/tmp")
check(exists("server", envir = app_env), "app.R did not create server when sourced from /private/tmp")

config_env <- new.env(parent = globalenv())
setwd("/private/tmp")
config_result <- tryCatch(
  {
    source(file.path(repo_root, "pipeline", "00_config.R"), local = config_env)
    TRUE
  },
  error = function(e) conditionMessage(e)
)
check(identical(config_result, TRUE),
      paste("pipeline/00_config.R failed when sourced from /private/tmp:", config_result))

if (identical(config_result, TRUE)) {
  data_dir <- normalizePath(get("DATA_DIR", envir = config_env), winslash = "/", mustWork = TRUE)
  check(identical(data_dir, file.path(repo_root, "data")),
        paste("DATA_DIR resolved to", data_dir, "instead of project data directory"))
}

if (length(failures) > 0) {
  stop(
    paste(c("Project root path checks failed:", paste0("- ", failures)),
          collapse = "\n"),
    call. = FALSE
  )
}

cat("Project root path checks passed.\n")

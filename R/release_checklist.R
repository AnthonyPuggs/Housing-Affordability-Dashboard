# ==============================================================================
# Public-release readiness checklist
# ==============================================================================
# Lightweight, read-only checks for deciding whether the current dashboard state
# is suitable for publication.
# ==============================================================================

if (!exists("project_root", mode = "function") ||
    !exists("project_path", mode = "function")) {
  source(file.path("R", "project_paths.R"))
}
if (!exists("pipeline_external_sources", mode = "function")) {
  pipeline_contracts_path <- file.path("R", "pipeline_contracts.R")
  if (file.exists(pipeline_contracts_path)) {
    source(pipeline_contracts_path)
  }
}

release_checklist_row <- function(check_id, category, status, detail,
                                  recommendation) {
  data.frame(
    check_id = check_id,
    category = category,
    status = status,
    detail = detail,
    recommendation = recommendation,
    stringsAsFactors = FALSE
  )
}

release_relative_path <- function(path, root = project_root()) {
  path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  sub(paste0("^", gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", root), "/?"),
      "", path)
}

release_read_csv <- function(path) {
  tryCatch(
    read.csv(path, stringsAsFactors = FALSE, check.names = FALSE),
    error = function(e) e
  )
}

release_schema_check <- function(file, data_dir, required_columns) {
  path <- file.path(data_dir, file)
  rel <- file.path("data", file)

  if (!file.exists(path)) {
    return(release_checklist_row(
      paste0("data_", tools::file_path_sans_ext(file), "_exists"),
      "data", "fail",
      paste(rel, "is missing."),
      "Run Rscript pipeline/05_driver.R before public release."
    ))
  }

  df <- release_read_csv(path)
  if (inherits(df, "error")) {
    return(release_checklist_row(
      paste0("data_", tools::file_path_sans_ext(file), "_parse"),
      "data", "fail",
      paste(rel, "could not be parsed as CSV."),
      "Regenerate the file from the pipeline and inspect the parser error."
    ))
  }

  if (nrow(df) == 0) {
    return(release_checklist_row(
      paste0("data_", tools::file_path_sans_ext(file), "_rows"),
      "data", "fail",
      paste(rel, "has zero rows."),
      "Regenerate the file from the pipeline."
    ))
  }

  missing_columns <- setdiff(required_columns, names(df))
  if (length(missing_columns) > 0) {
    return(release_checklist_row(
      paste0("data_", tools::file_path_sans_ext(file), "_schema"),
      "data", "fail",
      paste(rel, "is missing columns:",
            paste(missing_columns, collapse = ", ")),
      "Regenerate the file or update the parser/schema contract."
    ))
  }

  release_checklist_row(
    paste0("data_", tools::file_path_sans_ext(file), "_ready"),
    "data", "pass",
    paste(rel, "exists, parses and has", nrow(df), "rows."),
    "No action required."
  )
}

release_date_freshness_check <- function(file, data_dir, warn_days = 180) {
  path <- file.path(data_dir, file)
  rel <- file.path("data", file)

  if (!file.exists(path)) {
    return(release_checklist_row(
      paste0("data_", tools::file_path_sans_ext(file), "_date_max"),
      "data", "fail",
      paste(rel, "is missing, so max date cannot be checked."),
      "Run Rscript pipeline/05_driver.R before public release."
    ))
  }

  df <- release_read_csv(path)
  if (inherits(df, "error") || !("date" %in% names(df))) {
    return(release_checklist_row(
      paste0("data_", tools::file_path_sans_ext(file), "_date_max"),
      "data", "warn",
      paste(rel, "has no parseable date column for freshness reporting."),
      "Confirm the data vintage manually before public release."
    ))
  }

  dates <- suppressWarnings(as.Date(df$date))
  max_date <- suppressWarnings(max(dates, na.rm = TRUE))
  if (!is.finite(max_date)) {
    return(release_checklist_row(
      paste0("data_", tools::file_path_sans_ext(file), "_date_max"),
      "data", "warn",
      paste(rel, "has no parseable max date for freshness reporting."),
      "Confirm the data vintage manually before public release."
    ))
  }

  age_days <- as.integer(Sys.Date() - max_date)
  status <- if (age_days > warn_days) "warn" else "pass"
  detail <- paste(rel, "max date is", format(max_date),
                  paste0("(", age_days, " days old)."))
  recommendation <- if (identical(status, "warn")) {
    "Accept only if the data vintage is deliberate; otherwise refresh the pipeline."
  } else {
    "No action required."
  }

  release_checklist_row(
    paste0("data_", tools::file_path_sans_ext(file), "_date_max"),
    "data", status, detail, recommendation
  )
}

release_file_surface_check <- function(check_id, category, file, repo_root,
                                       recommendation) {
  rel <- file
  status <- if (file.exists(file.path(repo_root, file))) "pass" else "fail"
  detail <- if (identical(status, "pass")) {
    paste(rel, "exists.")
  } else {
    paste(rel, "is missing.")
  }

  release_checklist_row(
    check_id, category, status, detail,
    if (identical(status, "pass")) "No action required." else recommendation
  )
}

release_text_check <- function(check_id, category, file, needles, repo_root,
                               recommendation) {
  path <- file.path(repo_root, file)
  if (!file.exists(path)) {
    return(release_checklist_row(
      check_id, category, "fail",
      paste(file, "is missing."),
      recommendation
    ))
  }

  text <- paste(readLines(path, warn = FALSE), collapse = "\n")
  missing <- needles[!vapply(needles, grepl, logical(1), text, fixed = TRUE)]
  if (length(missing) > 0) {
    return(release_checklist_row(
      check_id, category, "fail",
      paste(file, "is missing required text:",
            paste(missing, collapse = "; ")),
      recommendation
    ))
  }

  release_checklist_row(
    check_id, category, "pass",
    paste(file, "contains required release text."),
    "No action required."
  )
}

release_git_output <- function(args, repo_root) {
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(repo_root)
  result <- system2("git", args, stdout = TRUE, stderr = TRUE)
  status <- attr(result, "status")
  if (is.null(status)) {
    status <- 0L
  }
  list(status = status, output = result)
}

release_checklist <- function(repo_root = project_root(),
                              data_dir = project_path("data")) {
  repo_root <- normalizePath(repo_root, winslash = "/", mustWork = TRUE)
  data_dir <- normalizePath(data_dir, winslash = "/", mustWork = TRUE)

  time_series_schema <- c(
    "date", "value", "series", "series_id", "category", "unit", "frequency"
  )
  affordability_schema <- c(
    "date", "value", "indicator", "geography", "unit", "frequency"
  )
  sih_schema <- c(
    "survey_year", "value", "metric", "tenure", "breakdown_var",
    "breakdown_val", "geography", "stat_type"
  )
  sih_quality_schema <- c(
    "source_file", "source_table", "survey_year", "metric", "tenure",
    "breakdown_var", "breakdown_val", "geography", "stat_type",
    "quality_measure", "quality_value", "quality_unit", "reliability_flag",
    "reliability_note"
  )

  checks <- list(
    release_schema_check("abs_timeseries.csv", data_dir, time_series_schema),
    release_schema_check("rba_rates.csv", data_dir, time_series_schema),
    release_schema_check("abs_supply_demand.csv", data_dir, time_series_schema),
    release_schema_check("affordability_indices.csv", data_dir,
                         affordability_schema),
    release_schema_check("sih_timeseries_national.csv", data_dir, sih_schema),
    release_schema_check("sih_state_timeseries.csv", data_dir, sih_schema),
    release_schema_check("sih_costs_2020.csv", data_dir, sih_schema),
    release_schema_check("sih_cost_ratios_2020.csv", data_dir, sih_schema),
    release_schema_check("sih_stress_bands_2020.csv", data_dir, sih_schema),
    release_schema_check("sih_lower_income_states.csv", data_dir, sih_schema),
    release_schema_check("sih_geographic_2020.csv", data_dir, sih_schema),
    release_schema_check("sih_nhha_rental_stress.csv", data_dir, sih_schema),
    release_schema_check("sih_estimate_quality.csv", data_dir,
                         sih_quality_schema),
    release_date_freshness_check("abs_timeseries.csv", data_dir),
    release_date_freshness_check("rba_rates.csv", data_dir),
    release_date_freshness_check("abs_supply_demand.csv", data_dir),
    release_date_freshness_check("affordability_indices.csv", data_dir),

    release_file_surface_check(
      "methodology_indicator_registry", "methodology",
      file.path("R", "indicator_registry.R"), repo_root,
      "Restore the indicator registry before public release."
    ),
    release_file_surface_check(
      "methodology_provenance_report", "methodology",
      file.path("R", "provenance_report.R"), repo_root,
      "Restore the provenance report helper before public release."
    ),
    release_file_surface_check(
      "methodology_pipeline_contracts", "methodology",
      file.path("R", "pipeline_contracts.R"), repo_root,
      "Restore pipeline stage contracts and the external-source manifest before public release."
    ),
    {
      if (!exists("pipeline_external_sources", mode = "function")) {
        release_checklist_row(
          "methodology_external_source_manifest", "methodology", "fail",
          "R/pipeline_contracts.R is unavailable, so external sources cannot be listed.",
          "Restore R/pipeline_contracts.R before public release."
        )
      } else {
        sources <- pipeline_external_sources()
        source_text <- paste(apply(sources, 1, paste, collapse = " "),
                             collapse = "\n")
        required_sources <- c("ABS 6432.0", "ABS 6401.0", "ABS SDMX CPI",
                              "RBA F1", "RBA F5", "RBA F6")
        missing_sources <- required_sources[
          !vapply(required_sources, grepl, logical(1), source_text,
                  fixed = TRUE)
        ]
        status <- if (length(missing_sources) == 0 &&
                      !grepl("/Users/", source_text, fixed = TRUE)) {
          "pass"
        } else {
          "fail"
        }
        detail <- if (identical(status, "pass")) {
          "External ABS/RBA source manifest is present and path-safe."
        } else if (length(missing_sources) > 0) {
          paste("External source manifest is missing:",
                paste(missing_sources, collapse = ", "))
        } else {
          "External source manifest contains a local absolute user path."
        }
        recommendation <- if (identical(status, "pass")) {
          "No action required."
        } else {
          "Update R/pipeline_contracts.R before public release."
        }
        release_checklist_row(
          "methodology_external_source_manifest", "methodology",
          status, detail, recommendation
        )
      }
    },
    release_text_check(
      "methodology_page_wiring", "methodology", "app.R",
      c('source(project_path("R", "methodology_module.R"), local = TRUE)',
        'methodologyPageUI("methodology")',
        'methodologyPageServer("methodology")'),
      repo_root,
      "Restore the Methodology page wiring before public release."
    ),
    release_text_check(
      "methodology_readme_text", "methodology", "README.md",
      c("R/indicator_registry.R", "Methodology page",
        "stylised scenarios"),
      repo_root,
      "Document methodology, provenance and caveats in README.md."
    ),

    release_file_surface_check(
      "reproducibility_description", "reproducibility", "DESCRIPTION",
      repo_root, "Restore DESCRIPTION before public release."
    ),
    release_file_surface_check(
      "reproducibility_renv_lock", "reproducibility", "renv.lock",
      repo_root, "Run renv::snapshot() after reviewing dependency changes."
    ),
    release_file_surface_check(
      "reproducibility_rprofile", "reproducibility", ".Rprofile",
      repo_root, "Restore .Rprofile so renv activates from the repo root."
    ),
    release_file_surface_check(
      "reproducibility_renv_activate", "reproducibility",
      file.path("renv", "activate.R"), repo_root,
      "Restore renv/activate.R before public release."
    )
  )

  readme_path <- file.path(repo_root, "README.md")
  readme_text <- if (file.exists(readme_path)) {
    paste(readLines(readme_path, warn = FALSE), collapse = "\n")
  } else {
    ""
  }
  checks <- c(checks, list(
    {
      status <- if (grepl("/Users/", readme_text, fixed = TRUE)) "fail" else "pass"
      detail <- if (identical(status, "fail")) {
        "README.md contains an absolute user path."
      } else {
        "README.md has no absolute local user paths."
      }
      recommendation <- if (identical(status, "fail")) {
        "Replace local absolute paths with repository-relative commands."
      } else {
        "No action required."
      }
      release_checklist_row(
        "hygiene_readme_no_absolute_user_path", "hygiene",
        status, detail, recommendation
      )
    }
  ))

  ignored_tracked <- release_git_output(
    c("ls-files", "-ci", "--exclude-standard"), repo_root
  )
  checks <- c(checks, list(
    {
      status <- if (ignored_tracked$status == 0L &&
                    length(ignored_tracked$output) == 0) "pass" else "fail"
      detail <- if (identical(status, "pass")) {
        "No tracked files are ignored by .gitignore."
      } else {
        paste("Tracked ignored files:",
              paste(ignored_tracked$output, collapse = ", "))
      }
      recommendation <- if (identical(status, "pass")) {
        "No action required."
      } else {
        "Update .gitignore so tracked release assets are not ignored."
      }
      release_checklist_row(
        "hygiene_no_tracked_ignored_files", "hygiene",
        status, detail, recommendation
      )
    }
  ))

  staged <- release_git_output(c("diff", "--cached", "--name-only"), repo_root)
  local_artifacts <- c(
    ".DS_Store", "AGENTS.md", "quality_reports/housing_dashboard_full_review.md"
  )
  staged_artifacts <- intersect(staged$output, local_artifacts)
  checks <- c(checks, list(
    {
      status <- if (staged$status == 0L &&
                    length(staged_artifacts) == 0) "pass" else "fail"
      detail <- if (identical(status, "pass")) {
        "No known local artefacts are staged."
      } else {
        paste("Known local artefacts are staged:",
              paste(staged_artifacts, collapse = ", "))
      }
      recommendation <- if (identical(status, "pass")) {
        "No action required."
      } else {
        "Unstage local artefacts before committing a public-release change."
      }
      release_checklist_row(
        "hygiene_no_local_artifacts_staged", "hygiene",
        status, detail, recommendation
      )
    }
  ))

  out <- do.call(rbind, checks)
  rownames(out) <- NULL
  out$status <- factor(out$status, levels = c("pass", "warn", "fail"))
  out$status <- as.character(out$status)
  out
}

validate_release_checklist <- function(fail_on = "fail") {
  fail_on <- match.arg(fail_on, c("fail", "warn"))
  checks <- release_checklist()
  blocking_statuses <- if (identical(fail_on, "warn")) {
    c("warn", "fail")
  } else {
    "fail"
  }
  blocking <- checks[checks$status %in% blocking_statuses, , drop = FALSE]

  print(checks, row.names = FALSE)

  if (nrow(blocking) > 0) {
    stop(
      paste(
        c("Release checklist has blocking items:",
          paste0("- ", blocking$check_id, " [", blocking$status, "]: ",
                 blocking$detail)),
        collapse = "\n"
      ),
      call. = FALSE
    )
  }

  invisible(checks)
}

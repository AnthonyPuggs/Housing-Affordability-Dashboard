# Runs standalone via `Rscript tests/test_data_refresh_workflow.R` (repo root) and under
# testthat::test_dir("tests"); see tests/helper-contracts.R.
#
# Structural workflow contract (TEST-09): the data-refresh workflow is parsed
# with yaml::read_yaml() and asserted on its structure (triggers, permissions,
# concurrency, SHA-pinned actions, a failure-reporting step) rather than pinned
# byte-for-byte. Free-text shell inside `run:` blocks is inherently unstructured,
# so the commands that implement the hardening (rebase, diff-threshold, PR/issue
# routing) are still matched as substrings of the concatenated run text.
if (!exists("contracts_harness_loaded", mode = "function")) {
  source(file.path(if (basename(getwd()) == "tests") "." else "tests",
                   "helper-contracts.R"))
}

# GitHub Actions uses the YAML key `on:`, which libyaml resolves to the boolean
# true; the yaml package therefore stores the triggers block under the name
# "TRUE". Fetch it by either spelling so the test is parser-agnostic.
workflow_triggers <- function(wf) {
  wf[[if ("on" %in% names(wf)) "on" else "TRUE"]]
}

# All `uses:` action references in a parsed workflow (across every job/step).
workflow_uses <- function(wf) {
  unlist(lapply(wf$jobs, function(job) {
    vapply(job$steps, function(step) {
      if (is.null(step$uses)) NA_character_ else step$uses
    }, character(1))
  }), use.names = FALSE)
}

test_that("data_refresh_workflow structural contracts", {
  repo_root <- repo_root_path()
  workflow_path <- file.path(repo_root, ".github", "workflows", "data-refresh.yml")
  check(file.exists(workflow_path),
        ".github/workflows/data-refresh.yml does not exist")
  skip_if_not(file.exists(workflow_path))

  wf <- yaml::read_yaml(workflow_path)
  raw_lines <- readLines(workflow_path, warn = FALSE)
  workflow_text <- paste(raw_lines, collapse = "\n")

  # --- Triggers -------------------------------------------------------------
  triggers <- workflow_triggers(wf)
  check("workflow_dispatch" %in% names(triggers),
        "data-refresh must keep a manual workflow_dispatch trigger")
  crons <- vapply(triggers$schedule, function(s) s$cron %||% NA_character_,
                  character(1))
  check("0 21 * * 0-4" %in% crons,
        "data-refresh schedule must keep the 07:00 AEST cron '0 21 * * 0-4'")
  # 'timezone:' is not a GitHub Actions schedule key (schedules are UTC-only);
  # a stray key silently shifts the run to the wrong local time.
  schedule_keys <- unlist(lapply(triggers$schedule, names))
  check(!("timezone" %in% schedule_keys),
        "data-refresh schedule must not use the invalid 'timezone:' key")
  check(!grepl("timezone:", workflow_text, fixed = TRUE),
        "data-refresh workflow must not use the invalid 'timezone:' schedule key")

  # --- Permissions ----------------------------------------------------------
  perms <- wf$permissions
  check(identical(perms$contents, "write"),
        "data-refresh needs contents: write to commit refreshed data")
  check(identical(perms$issues, "write"),
        "data-refresh needs issues: write to open/comment a failure issue")
  check(identical(perms[["pull-requests"]], "write"),
        "data-refresh needs pull-requests: write to open a PR for large revisions")

  # --- Concurrency: queue, never cancel a refresh mid-flight ----------------
  check(identical(wf$concurrency$group, "data-refresh-${{ github.ref }}"),
        "data-refresh concurrency group changed")
  check(isFALSE(wf$concurrency[["cancel-in-progress"]]),
        "data-refresh must set cancel-in-progress: false so refreshes queue")

  # --- Actions pinned to commit SHAs with a version comment -----------------
  uses <- workflow_uses(wf)
  uses <- uses[!is.na(uses)]
  check(length(uses) >= 3,
        "data-refresh should use checkout + setup-r + setup-renv actions")
  unpinned <- uses[!grepl("@[0-9a-fA-F]{40}$", uses)]
  check(length(unpinned) == 0,
        paste("data-refresh actions must be pinned to a full commit SHA:",
              paste(unpinned, collapse = "; ")))
  uses_lines <- grep("uses:", raw_lines, value = TRUE)
  uncommented <- uses_lines[!grepl("#", uses_lines)]
  check(length(uncommented) == 0,
        paste("each pinned `uses:` needs a version comment:",
              paste(trimws(uncommented), collapse = " | ")))

  # --- A step reports failure (opens/comments a GitHub issue) ---------------
  steps <- wf$jobs$refresh$steps
  failure_steps <- Filter(function(s) {
    !is.null(s$`if`) && grepl("failure", s$`if`, fixed = TRUE)
  }, steps)
  check(length(failure_steps) >= 1,
        "data-refresh must have an if: failure() step to report breakage")

  # --- Run-block commands (free-text shell, matched as substrings) ----------
  run_text <- paste(
    unlist(lapply(steps, function(s) s$run %||% character(0))),
    collapse = "\n"
  )
  required_run <- c(
    "Rscript pipeline/05_driver.R",
    "Rscript tests/test_data_vintage.R",
    "Rscript tests/test_abs_labour_api_contracts.R",
    "Rscript tests/test_pipeline_driver_stage_gates.R",
    "Rscript tests/test_pipeline_outputs.R",
    "Rscript tests/test_provenance_report.R",
    "Rscript tests/test_ui_smoke_contracts.R",
    "':(exclude)data/data_vintage.csv'",
    "data: refresh dashboard inputs",
    "git pull --rebase",     # rebase onto any concurrent main advance before push
    "git diff --numstat",    # size the revision for the diff-threshold guard
    "gh pr create",          # large revisions route to a PR instead of direct push
    "gh pr merge",           # the PR is auto-merged as an audit trail, not a manual gate
    "gh issue"               # failure path opens/comments an issue
  )
  missing_run <- required_run[
    !vapply(required_run, grepl, logical(1), run_text, fixed = TRUE)
  ]
  check(length(missing_run) == 0,
        paste("data-refresh run steps missing commands:",
              paste(missing_run, collapse = "; ")))

  # --- No deployment secrets / manual deploy leaked in ----------------------
  forbidden_text <- c(
    "CONNECT_API_KEY", "CONNECT_SERVER",
    "rsconnect::deployApp", "rstudio/actions/connect-publish"
  )
  present_forbidden <- forbidden_text[
    vapply(forbidden_text, grepl, logical(1), workflow_text, fixed = TRUE)
  ]
  check(length(present_forbidden) == 0,
        paste("data-refresh workflow must not embed deployment secrets or manual deployment:",
              paste(present_forbidden, collapse = "; ")))
})

test_that("ci workflow actions are pinned to commit SHAs", {
  repo_root <- repo_root_path()
  ci_path <- file.path(repo_root, ".github", "workflows", "ci.yml")
  check(file.exists(ci_path), ".github/workflows/ci.yml does not exist")
  skip_if_not(file.exists(ci_path))

  wf <- yaml::read_yaml(ci_path)
  uses <- workflow_uses(wf)
  uses <- uses[!is.na(uses)]
  unpinned <- uses[!grepl("@[0-9a-fA-F]{40}$", uses)]
  check(length(unpinned) == 0,
        paste("ci actions must be pinned to a full commit SHA:",
              paste(unpinned, collapse = "; ")))

  ci_uses_lines <- grep("uses:", readLines(ci_path, warn = FALSE), value = TRUE)
  uncommented <- ci_uses_lines[!grepl("#", ci_uses_lines)]
  check(length(uncommented) == 0,
        paste("each pinned `uses:` in ci.yml needs a version comment:",
              paste(trimws(uncommented), collapse = " | ")))
})

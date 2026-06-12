# Runs standalone via `Rscript tests/test_abs_labour_api_contracts.R` (repo root) and under
# testthat::test_dir("tests"); see tests/helper-contracts.R.
#
# Structural endpoint contract (TEST-09): rather than grepping the fetch script
# for endpoint URL/version *strings*, parse pipeline/00_config.R and evaluate
# only the pinned-constant assignments, then assert their values. This catches a
# silent dataflow-version bump (the actual risk PIPE-09 guards) instead of just
# asserting a string is present somewhere in the source.
if (!exists("contracts_harness_loaded", mode = "function")) {
  source(file.path(if (basename(getwd()) == "tests") "." else "tests",
                   "helper-contracts.R"))
}

# Evaluate the requested top-level `name <- value` assignments from an R script
# in an isolated environment, without sourcing the whole file (no package loads,
# path resolution or network). Only simple constant assignments are evaluated.
# Safe by construction: the input is the project's own checked-in config file
# (trusted source, not user data), only assignments whose LHS name is in the
# requested allow-list are eval'd, and the environment is parented on baseenv()
# so evaluation cannot reach package or pipeline state.
eval_config_constants <- function(path, names) {
  env <- new.env(parent = baseenv())
  exprs <- parse(file = path)
  for (e in exprs) {
    if (is.call(e) &&
        (identical(e[[1]], as.name("<-")) || identical(e[[1]], as.name("="))) &&
        is.name(e[[2]]) && as.character(e[[2]]) %in% names) {
      eval(e, envir = env)
    }
  }
  env
}

test_that("ABS SDMX endpoints are pinned to explicit dataflow versions", {
  repo_root <- repo_root_path()
  config_path <- file.path(repo_root, "pipeline", "00_config.R")
  check(file.exists(config_path), "pipeline/00_config.R does not exist")
  skip_if_not(file.exists(config_path))

  wanted <- c("ABS_SDMX_DATA_URL", "ABS_SDMX_CPI_FLOW", "ABS_SDMX_LF_FLOW",
              "ABS_SDMX_LF_UNDER_FLOW", "ABS_SDMX_CPI_RENTS_KEY",
              "ABS_SDMX_CPI_ALL_GROUPS_KEY")
  cfg <- eval_config_constants(config_path, wanted)

  missing_consts <- wanted[!vapply(wanted, exists, logical(1), envir = cfg,
                                   inherits = FALSE)]
  check(length(missing_consts) == 0,
        paste("00_config.R is missing pinned SDMX constants:",
              paste(missing_consts, collapse = ", ")))
  skip_if(length(missing_consts) > 0)

  check(identical(cfg$ABS_SDMX_DATA_URL,
                  "https://data.api.abs.gov.au/rest/data"),
        "ABS_SDMX_DATA_URL endpoint changed unexpectedly")

  # Flows must be pinned as "ABS,<NAME>,<major>.<minor>.<patch>"; an unversioned
  # or differently-versioned flow would let an ABS recode ship silently.
  flows <- c(CPI = cfg$ABS_SDMX_CPI_FLOW, LF = cfg$ABS_SDMX_LF_FLOW,
             LF_UNDER = cfg$ABS_SDMX_LF_UNDER_FLOW)
  unversioned <- flows[!grepl("^ABS,[A-Z_]+,[0-9]+\\.[0-9]+\\.[0-9]+$", flows)]
  check(length(unversioned) == 0,
        paste("ABS SDMX flow not pinned to an explicit version:",
              paste(sprintf("%s=%s", names(unversioned), unversioned),
                    collapse = "; ")))

  # Exact pinned versions in force; bump these deliberately, never silently.
  check(identical(cfg$ABS_SDMX_CPI_FLOW, "ABS,CPI,2.0.0"),
        paste("ABS CPI dataflow version changed to", cfg$ABS_SDMX_CPI_FLOW))
  check(identical(cfg$ABS_SDMX_LF_FLOW, "ABS,LF,1.0.0"),
        paste("ABS LF dataflow version changed to", cfg$ABS_SDMX_LF_FLOW))
  check(identical(cfg$ABS_SDMX_LF_UNDER_FLOW, "ABS,LF_UNDER,1.0.1"),
        paste("ABS LF_UNDER dataflow version changed to",
              cfg$ABS_SDMX_LF_UNDER_FLOW))

  # CPI series keys must be present and shaped MEASURE.INDEX.TSEST.REGION.FREQ.
  cpi_keys <- c(rents = cfg$ABS_SDMX_CPI_RENTS_KEY,
                all_groups = cfg$ABS_SDMX_CPI_ALL_GROUPS_KEY)
  malformed <- cpi_keys[!grepl("^[0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]+\\.[A-Z]$",
                               cpi_keys)]
  check(length(malformed) == 0,
        paste("ABS CPI SDMX key malformed:",
              paste(sprintf("%s=%s", names(malformed), malformed),
                    collapse = "; ")))
})

test_that("ABS labour fetch uses the pinned config constants", {
  repo_root <- repo_root_path()
  script_path <- file.path(repo_root, "pipeline", "02_fetch_abs_timeseries.R")
  check(file.exists(script_path), "pipeline/02_fetch_abs_timeseries.R does not exist")

  script_text <- if (file.exists(script_path)) {
    paste(readLines(script_path, warn = FALSE), collapse = "\n")
  } else {
    ""
  }

  # The fetch must route through the pinned-flow constants and abs_sdmx_csv()
  # (which asserts the echoed dataflow), and pass the specific labour series
  # keys; the keys themselves are an API contract that has no config constant.
  required_text <- c(
    "fetch_abs_lf_series <- function",
    "abs_sdmx_csv(",
    "ABS_SDMX_LF_FLOW",
    "ABS_SDMX_LF_UNDER_FLOW",
    "M12.3.1599.20.AUS.M",
    "M13.3.1599.20.AUS.M",
    "M23.3.1599.20.AUS.M",
    "M24.3.1599.20.AUS.M"
  )

  missing_text <- required_text[
    !vapply(required_text, grepl, logical(1), script_text, fixed = TRUE)
  ]
  check(length(missing_text) == 0,
        paste("ABS labour SDMX fetch contract missing text:",
              paste(missing_text, collapse = "; ")))

  forbidden_text <- c(
    'read_abs(cat_no = "6202.0", tables = "1")',
    'read_abs(cat_no = "6202.0", tables = "22")'
  )
  present_forbidden <- forbidden_text[
    vapply(forbidden_text, grepl, logical(1), script_text, fixed = TRUE)
  ]
  check(length(present_forbidden) == 0,
        paste("ABS labour fetch must not depend on brittle readabs catalogue lookups:",
              paste(present_forbidden, collapse = "; ")))
})

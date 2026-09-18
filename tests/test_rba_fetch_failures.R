# Runs standalone via `Rscript tests/test_rba_fetch_failures.R` (repo root) and
# under testthat::test_dir("tests"); see tests/helper-contracts.R.
if (!exists("contracts_harness_loaded", mode = "function")) {
  source(file.path(if (basename(getwd()) == "tests") "." else "tests",
                   "helper-contracts.R"))
}

source(file.path(repo_root_path(), "pipeline", "00_config.R"), local = TRUE)

valid_rba_csv <- function(table_id = "F1", series_name = NULL,
                          series_id = NULL) {
  contracts <- list(
    F1 = c("Cash Rate Target", "FIRMMCRTD"),
    F5 = c("Lending rates; Housing loans; Banks; Variable; Discounted; Owner-occupier",
           "FILRHLBVD"),
    F6 = c("Lending rates; Housing credit; New loans funded in the month; Owner-occupied; All loans; All institutions",
           "FLRHOFTA"),
    E2 = c("Household debt to income", "BHFDDIT")
  )
  contract <- contracts[[toupper(table_id)]]
  if (is.null(series_name)) series_name <- contract[[1]]
  if (is.null(series_id)) series_id <- contract[[2]]
  c(
    paste("Title", series_name, sep = ","),
    "Description,Test fixture",
    paste("Series ID", series_id, sep = ","),
    "Units,Per cent",
    "01-Jan-2024,4.35",
    "02-Jan-2024,4.35"
  )
}

fake_dependencies <- function(get, validator = function(path, table_id) TRUE,
                              promoter = promote_rba_candidate) {
  list(
    get = get,
    write_disk = function(path, overwrite = FALSE) list(path = path),
    http_error = function(response) response$status >= 400L,
    validator = validator,
    promoter = promoter
  )
}

test_that("CSV HTTP failure falls back to XLSX without damaging a known-good cache", {
  cache_dir <- tempfile("rba-cache-")
  dir.create(cache_dir)
  cache_csv <- file.path(cache_dir, "rba_f1_raw.csv")
  writeLines(valid_rba_csv("F1"), cache_csv)
  old_time <- Sys.time() - 48 * 60 * 60
  Sys.setFileTime(cache_csv, old_time)
  old_bytes <- readBin(cache_csv, "raw", n = file.info(cache_csv)$size)
  old_mtime <- file.info(cache_csv)$mtime

  requests <- character()
  fake_get <- function(url, config) {
    requests <<- c(requests, url)
    is_csv <- grepl("/csv/", url, fixed = TRUE)
    writeLines(if (is_csv) "HTTP 500 error body" else "validated fixture",
               config$path)
    list(status = if (is_csv) 500L else 200L)
  }
  fixture_validator <- function(path, table_id) {
    if (!identical(readLines(path, warn = FALSE), "validated fixture")) {
      stop("invalid fixture")
    }
    TRUE
  }
  deps <- fake_dependencies(fake_get, fixture_validator)

  result <- fetch_rba_table("f1", cache_dir = cache_dir,
                            dependencies = deps)
  expect_identical(result, file.path(cache_dir, "rba_f1_raw.xlsx"))
  expect_identical(readBin(cache_csv, "raw", n = file.info(cache_csv)$size),
                   old_bytes)
  expect_equal(file.info(cache_csv)$mtime, old_mtime, tolerance = 1)
  expect_false(any(grepl("HTTP 500 error body",
                         readLines(cache_csv, warn = FALSE), fixed = TRUE)))

  request_count <- length(requests)
  result_again <- fetch_rba_table("f1", cache_dir = cache_dir,
                                  dependencies = deps)
  expect_identical(result_again, result)
  expect_length(requests, request_count)
})

test_that("HTTP failures and transport interruptions fail loudly without a cache", {
  cache_dir <- tempfile("rba-cache-")
  dir.create(cache_dir)
  http_get <- function(url, config) {
    writeLines("HTTP error body", config$path)
    list(status = if (grepl("/csv/", url, fixed = TRUE)) 404L else 500L)
  }
  expect_error(
    fetch_rba_table("f1", cache_dir, dependencies = fake_dependencies(http_get),
                    strict = TRUE),
    "RBA F1 acquisition failed.*HTTP"
  )
  expect_length(list.files(cache_dir, pattern = "^rba_f1_"), 0L)

  transport_get <- function(url, config) stop("connection reset")
  expect_error(
    fetch_rba_table("f1", cache_dir,
                    dependencies = fake_dependencies(transport_get),
                    strict = TRUE),
    "RBA F1 acquisition failed.*connection reset"
  )
})

test_that("XLSX fallback uses the real parser and is reused without another request", {
  cache_dir <- tempfile("rba-xlsx-")
  dir.create(cache_dir)
  fixture <- file.path(repo_root_path(), "tests", "fixtures", "rba_f1_minimal.xlsx")
  requests <- 0L
  get <- function(url, config) {
    requests <<- requests + 1L
    if (grepl("/csv/", url, fixed = TRUE)) {
      writeLines("HTTP error", config$path)
      return(list(status = 500L))
    }
    file.copy(fixture, config$path, overwrite = TRUE)
    list(status = 200L)
  }
  deps <- fake_dependencies(get, validate_rba_candidate)
  path <- fetch_rba_table("f1", cache_dir, dependencies = deps, strict = TRUE)
  parsed <- parse_rba_file(path, "F1", strict = TRUE)
  expect_equal(parsed$date, as.Date(c("2024-01-01", "2024-01-02", "2024-01-03")))
  expect_equal(parsed$value, rep(4.35, 3))
  count <- requests
  expect_identical(fetch_rba_table("f1", cache_dir, dependencies = deps, strict = TRUE), path)
  expect_equal(requests, count)
  expect_false(file.exists(file.path(cache_dir, "rba_f1_raw.csv")))
})

test_that("malformed HTTP 200 candidates and invalid caches are rejected", {
  cache_dir <- tempfile("rba-cache-")
  dir.create(cache_dir)
  invalid_cache <- file.path(cache_dir, "rba_f1_raw.csv")
  writeLines("not an RBA table", invalid_cache)

  requests <- 0L
  fake_get <- function(url, config) {
    requests <<- requests + 1L
    writeLines("still not an RBA table", config$path)
    list(status = 200L)
  }
  expect_error(
    fetch_rba_table("f1", cache_dir,
                    dependencies = fake_dependencies(
                      fake_get,
                      function(path, table_id) stop("missing metadata")
                    ), strict = TRUE),
    "RBA F1 acquisition failed.*missing metadata"
  )
  expect_gt(requests, 0L)
  expect_identical(readLines(invalid_cache, warn = FALSE), "not an RBA table")
})

test_that("expired valid cache is retained when every refresh attempt fails", {
  cache_dir <- tempfile("rba-cache-")
  dir.create(cache_dir)
  cache_csv <- file.path(cache_dir, "rba_f1_raw.csv")
  writeLines(valid_rba_csv("F1"), cache_csv)
  Sys.setFileTime(cache_csv, Sys.time() - 48 * 60 * 60)
  original <- readBin(cache_csv, "raw", n = file.info(cache_csv)$size)

  failed_get <- function(url, config) {
    writeLines("failure", config$path)
    list(status = 500L)
  }
  result <- expect_warning(
    fetch_rba_table("f1", cache_dir, strict = FALSE,
                    dependencies = fake_dependencies(
                      failed_get,
                      function(path, table_id) {
                        if (grepl("raw\\.csv$", path)) return(TRUE)
                        stop("invalid candidate")
                      }
                    )),
    "refresh failed.*using validated stale cache"
  )
  expect_identical(result, cache_csv)
  expect_identical(readBin(cache_csv, "raw", n = file.info(cache_csv)$size),
                   original)
})

test_that("strict refresh failure errors while retaining validated stale cache", {
  cache_dir <- tempfile("rba-cache-")
  dir.create(cache_dir)
  cache_csv <- file.path(cache_dir, "rba_f1_raw.csv")
  writeLines(valid_rba_csv("F1"), cache_csv)
  Sys.setFileTime(cache_csv, Sys.time() - 48 * 60 * 60)
  original <- readBin(cache_csv, "raw", n = file.info(cache_csv)$size)
  original_time <- file.info(cache_csv)$mtime

  failed_get <- function(url, config) {
    writeLines("failure", config$path)
    list(status = 500L)
  }
  validator <- function(path, table_id) {
    if (identical(path, cache_csv)) return(TRUE)
    stop("invalid candidate")
  }

  expect_error(
    fetch_rba_table(
      "f1", cache_dir,
      dependencies = fake_dependencies(failed_get, validator),
      strict = TRUE
    ),
    "RBA F1 acquisition failed.*validated stale cache remains"
  )
  expect_identical(readBin(cache_csv, "raw", n = file.info(cache_csv)$size),
                   original)
  expect_equal(file.info(cache_csv)$mtime, original_time, tolerance = 1)
})

test_that("expired cache is refreshed only after a candidate validates", {
  cache_dir <- tempfile("rba-cache-")
  dir.create(cache_dir)
  cache_csv <- file.path(cache_dir, "rba_f1_raw.csv")
  writeLines(valid_rba_csv("F1"), cache_csv)
  Sys.setFileTime(cache_csv, Sys.time() - 48 * 60 * 60)
  old_bytes <- readBin(cache_csv, "raw", n = file.info(cache_csv)$size)

  refreshed <- valid_rba_csv("F1")
  refreshed[length(refreshed)] <- "03-Jan-2024,4.35"
  fake_get <- function(url, config) {
    writeLines(refreshed, config$path)
    list(status = 200L)
  }
  validator <- function(path, table_id) {
    if (grepl("raw\\.csv$", path)) return(TRUE)
    if (!any(grepl("03-Jan-2024", readLines(path, warn = FALSE),
                   fixed = TRUE))) {
      stop("candidate did not contain the refreshed observation")
    }
    TRUE
  }

  result <- fetch_rba_table(
    "f1", cache_dir,
    dependencies = fake_dependencies(fake_get, validator),
    strict = TRUE
  )
  expect_identical(result, cache_csv)
  expect_false(identical(
    readBin(cache_csv, "raw", n = file.info(cache_csv)$size), old_bytes
  ))
  expect_true(any(grepl("03-Jan-2024", readLines(cache_csv, warn = FALSE),
                        fixed = TRUE)))
})

test_that("failed promotion restores the known-good destination", {
  cache_dir <- tempfile("rba-cache-")
  dir.create(cache_dir)
  destination <- file.path(cache_dir, "rba_f1_raw.csv")
  candidate <- file.path(cache_dir, "candidate.csv")
  writeLines("known good", destination)
  writeLines("replacement", candidate)
  before_time <- file.info(destination)$mtime

  move_count <- 0L
  failing_rename <- function(from, to) {
    move_count <<- move_count + 1L
    if (move_count == 2L) return(FALSE)
    file.rename(from, to)
  }
  expect_error(
    promote_rba_candidate(candidate, destination,
                          rename_fun = failing_rename),
    "promotion failed"
  )
  expect_identical(readLines(destination, warn = FALSE), "known good")
  expect_equal(file.info(destination)$mtime, before_time, tolerance = 1)
})

test_that("source parser reports missing and empty inputs by table and operation", {
  expect_error(parse_rba_file(NULL, "F1", strict = TRUE),
               "RBA F1 parsing failed: no source file was supplied")
  expect_error(parse_rba_file(file.path(tempdir(), "missing-rba.csv"), "F5",
                              strict = TRUE),
               "RBA F5 parsing failed: source file does not exist")

  empty <- tempfile(fileext = ".csv")
  file.create(empty)
  expect_error(parse_rba_file(empty, "E2", strict = TRUE),
               "RBA E2 parsing failed: table is empty")

  expect_warning(result <- parse_rba_file(NULL, "F6", strict = FALSE),
                 "RBA F6 parsing failed: no source file was supplied")
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0L)
})

test_that("non-empty F6 input must contain the exact required series and ID", {
  wrong_name <- tempfile(fileext = ".csv")
  writeLines(valid_rba_csv("F6", series_name = "Some housing rate"), wrong_name)
  wrong_id <- tempfile(fileext = ".csv")
  writeLines(valid_rba_csv("F6", series_id = "WRONGID"), wrong_id)

  expect_error(parse_rba_file(wrong_name, "F6", strict = TRUE),
               "RBA F6 parsing failed: required series.*FLRHOFTA")
  expect_error(parse_rba_file(wrong_id, "F6", strict = TRUE),
               "RBA F6 parsing failed: required series.*FLRHOFTA")

  expect_warning(result <- parse_rba_file(wrong_id, "F6", strict = FALSE),
                 "RBA F6 parsing failed: required series")
  expect_equal(nrow(result), 0L)
})

test_that("required series observations must be finite", {
  nonfinite <- tempfile(fileext = ".csv")
  fixture <- valid_rba_csv("F6")
  fixture[length(fixture)] <- "02-Jan-2024,Inf"
  writeLines(fixture, nonfinite)

  expect_error(
    parse_rba_file(nonfinite, "F6", strict = TRUE),
    "RBA F6 parsing failed: required series.*non-finite observations"
  )
  expect_warning(
    result <- parse_rba_file(nonfinite, "F6", strict = FALSE),
    "RBA F6 parsing failed: required series.*non-finite observations"
  )
  expect_equal(nrow(result), 0L)
})

test_that("all returned observations must be finite while source blanks are omitted", {
  secondary <- tempfile(fileext = ".csv")
  fixture <- paste0(valid_rba_csv("F6"), c(
    ",Other housing rate", ",Secondary fixture", ",FLRTEST", ",Per cent",
    ",Inf", ",5.10"
  ))
  writeLines(fixture, secondary)
  expect_error(parse_rba_file(secondary, "F6", strict = TRUE),
               "RBA F6 parsing failed: parsed output contains non-finite observations")
  fixture[5] <- "01-Jan-2024,4.35,"
  writeLines(fixture, secondary)
  parsed <- parse_rba_file(secondary, "F6", strict = TRUE)
  expect_gt(nrow(parsed), 0L)
  expect_true(all(is.finite(parsed$value)))
  expect_false(any(parsed$series_id == "FLRTEST" & parsed$date == as.Date("2024-01-01")))
})

test_that("all acquisition fallbacks warn and return NULL outside strict mode", {
  cache_dir <- tempfile("rba-cache-")
  dir.create(cache_dir)
  failed_get <- function(url, config) {
    writeLines("failure", config$path)
    list(status = 500L)
  }
  expect_warning(
    result <- fetch_rba_table(
      "e2", cache_dir,
      dependencies = fake_dependencies(failed_get),
      strict = FALSE
    ),
    "RBA E2 acquisition failed"
  )
  expect_null(result)
})

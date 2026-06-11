# ==============================================================================
# Manual CPI diagnostic
# ==============================================================================
# Legacy scratch diagnostic for checking historical ABS CPI workbook URLs and
# rent series parsing. This script is not part of the production pipeline and
# should not write project files; it only downloads temporary workbooks via
# tempfile() during manual investigation.
# ==============================================================================

library(httr)

# ABS URL pattern for previous releases of CPI Table 10
# The re-basing happened with the Dec quarter 2025 release (Jan 2026)
# So the Sep quarter 2025 release should still have 2011-12 base values
base_url <- "https://www.abs.gov.au/statistics/economy/price-indexes-and-inflation/consumer-price-index-australia"
slugs <- c(
  "sep-quarter-2025",
  "september-quarter-2025",
  "sep-2025",
  "september-2025",
  "jun-quarter-2025",
  "june-quarter-2025",
  "jun-2025",
  "mar-quarter-2025",
  "latest-release"
)

cat("Testing Table 10 download URLs:\n")
for (slug in slugs) {
  url <- paste0(base_url, "/", slug, "/6401010.xlsx")
  resp <- HEAD(url)
  cat(sprintf("  %-30s -> %d", slug, status_code(resp)))
  if (status_code(resp) == 200) {
    size <- as.numeric(headers(resp)$`content-length`)
    cat(sprintf(" (%s KB)", round(size/1024)))
  }
  cat("\n")
}

# If we find a working URL, download and check if it has non-NA Rents data
# Try the first successful slug
for (slug in slugs[slugs != "latest-release"]) {
  url <- paste0(base_url, "/", slug, "/6401010.xlsx")
  resp <- HEAD(url)
  if (status_code(resp) == 200) {
    cat("\nDownloading old Table 10 from:", slug, "\n")
    tmp <- tempfile(fileext = ".xlsx")
    download.file(url, tmp, mode = "wb", quiet = TRUE)
    
    library(readxl)
    library(dplyr)
    library(stringr)
    
    # Read like readabs does - Data1 sheet, skip header rows
    sheets <- excel_sheets(tmp)
    cat("Sheets:", paste(sheets, collapse=", "), "\n")
    
    # Try reading Data1
    d <- read_excel(tmp, sheet = "Data1", skip = 9)
    cat("Data1 rows:", nrow(d), "cols:", ncol(d), "\n")
    
    # The first column is "Series ID" (date), rest are series
    # Look for column headers that match Rents series IDs
    idx_sheet <- tryCatch(
      read_excel(tmp, sheet = "Index", col_names = FALSE),
      error = function(e) NULL
    )
    if (!is.null(idx_sheet)) {
      rents_rows <- idx_sheet %>% filter(
        str_detect(as.character(`...1`), regex("Rents", ignore_case = TRUE)) |
        str_detect(as.character(`...2`), regex("Rents", ignore_case = TRUE))
      )
      if (nrow(rents_rows) > 0) {
        cat("\nRents-related rows in Index sheet:\n")
        print(rents_rows, n = 10, width = 200)
      }
    }
    
    # Better approach: use readabs::read_abs_local to parse it properly
    library(readabs)
    old_data <- read_abs_local(tmp)
    cat("\nParsed rows:", nrow(old_data), "\n")
    
    rents <- old_data %>%
      filter(str_detect(series, regex("^Index Numbers ;\\s*Rents ;", ignore_case = TRUE)))
    cat("Rents rows:", nrow(rents), "\n")
    
    if (nrow(rents) > 0) {
      rents %>%
        mutate(city = str_trim(str_extract(series, ";\\s*([^;]+)\\s*;?$") %>% str_remove_all(";") %>% str_trim())) %>%
        group_by(city) %>%
        summarise(
          n = n(),
          n_non_na = sum(!is.na(value)),
          min_date = as.character(min(date)),
          max_date = as.character(max(date)),
          min_date_data = ifelse(any(!is.na(value)), as.character(min(date[!is.na(value)])), "all NA"),
          first_val = ifelse(any(!is.na(value)), first(na.omit(value)), NA_real_),
          .groups = "drop"
        ) %>%
        as.data.frame() %>%
        print()
    }
    
    unlink(tmp)
    break
  }
}

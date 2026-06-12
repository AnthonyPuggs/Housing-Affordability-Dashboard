# ==============================================================================
# sih_layouts.R — Header-anchored layout engine for ABS SIH workbooks
# ==============================================================================
# Sourced by pipeline/01_process_sih.R (PIPE-12). Parsers resolve structure
# from header text and block markers instead of positional skip= offsets and
# hard-coded column indices, so a republished workbook with a shifted layout
# fails loudly instead of parsing the wrong cells:
#
#   * read_sheet_raw()          — whole-sheet text read, no skip
#   * require_label_row()       — find a structural marker row or fail
#   * find_block_bounds()       — bound the ESTIMATES block so RSE/MOE rows can
#                                 never be parsed as estimates (the historical
#                                 duplicate-row root cause); quality parsers
#                                 reuse it with swapped start/stop patterns
#   * anchor_columns()          — resolve logical columns from header text
#   * sih_parse_columns_down()  — row-scanning engine for cross-section sheets
#                                 (label rows down, data columns across)
#
# Assertions route through sih_assert() -> pipeline_problem(): under
# PIPELINE_STRICT (driver/CI) any layout drift is a hard stage failure.

# 7-column logical key shared by every SIH estimate output. Each engine asserts
# zero duplicates on this key before returning, so duplicate rows can never
# reach data/ again.
SIH_ESTIMATE_KEY <- c("survey_year", "metric", "tenure", "breakdown_var",
                      "breakdown_val", "geography", "stat_type")

# Rows that end an ESTIMATES block: sampling-error sub-blocks and footnote
# furniture. Mirrors the retired estimate_block_rows() stop set.
SIH_ESTIMATE_STOP_PATTERN <- paste0(
  "^95% margin of error|^relative standard error|",
  "^Source|^Exclud|^NA\\b|^#|^Cells"
)

sih_assert <- function(cond, file, sheet, what) {
  if (!isTRUE(cond)) {
    pipeline_problem("SIH layout check failed [", basename(file), " / ", sheet,
                     "]: ", what)
  }
  invisible(TRUE)
}

read_sheet_raw <- function(file, sheet) {
  read_excel(file, sheet = sheet, col_names = FALSE, col_types = "text")
}

# Label column as squished text with NA -> "" so marker regexes are safe.
sih_label_column <- function(raw, label_col = 1L) {
  labels <- str_squish(as.character(raw[[label_col]]))
  labels[is.na(labels)] <- ""
  labels
}

# Header cell text normalised for anchoring: squish whitespace (merged/wrapped
# cells) and strip a trailing single-letter footnote marker like "(a)".
sih_normalise_header <- function(x) {
  x <- str_squish(as.character(x))
  str_remove(x, "\\s*\\([a-z]\\)$")
}

require_label_row <- function(raw, pattern, file, sheet, what) {
  hits <- which(str_detect(sih_label_column(raw),
                           regex(pattern, ignore_case = TRUE)))
  sih_assert(length(hits) >= 1L, file, sheet,
             paste0("no row label matches '", pattern, "' (", what, ")"))
  hits[[1L]]
}

# First/last data-row indices of a labelled block. Defaults bound the
# ESTIMATES block; quality parsers swap the patterns to bound MOE/RSE blocks.
find_block_bounds <- function(raw, file, sheet,
                              start_pattern = "^ESTIMATES",
                              stop_pattern = SIH_ESTIMATE_STOP_PATTERN,
                              what = "ESTIMATES block") {
  labels <- sih_label_column(raw)
  start <- require_label_row(raw, start_pattern, file, sheet, what)
  stops <- which(seq_along(labels) > start &
                   str_detect(labels, regex(stop_pattern, ignore_case = TRUE)))
  last <- if (length(stops) > 0) stops[[1L]] - 1L else nrow(raw)
  sih_assert(last >= start + 1L, file, sheet, paste0(what, " contains no rows"))
  c(first = start + 1L, last = last)
}

# One logical column: matched against normalised header text. `group`
# disambiguates repeated leaf headers (e.g. New/Established/Total under each
# buyer type) against a forward-filled group header row.
sih_col <- function(name, pattern, group = NULL) {
  list(name = name, pattern = pattern, group = group)
}

# Resolve every spec entry to exactly one physical column, in sheet order.
# `header_rows` is the band of rows searched for leaf headers; `group_row`
# (optional) holds merged group headers, forward-filled across columns.
anchor_columns <- function(raw, header_rows, column_spec, file, sheet,
                           group_row = NULL) {
  band <- lapply(header_rows, function(r) {
    sih_normalise_header(as.character(raw[r, ]))
  })

  groups <- NULL
  if (!is.null(group_row)) {
    groups <- sih_normalise_header(as.character(raw[group_row, ]))
    for (k in seq_along(groups)) {
      if ((is.na(groups[k]) || groups[k] == "") && k > 1L) {
        groups[k] <- groups[k - 1L]
      }
    }
  }

  resolved <- integer(length(column_spec))
  for (s in seq_along(column_spec)) {
    spec <- column_spec[[s]]
    hits <- sort(unique(unlist(lapply(band, function(h) {
      which(!is.na(h) & str_detect(h, spec$pattern))
    }))))
    if (!is.null(spec$group)) {
      sih_assert(!is.null(groups), file, sheet,
                 paste0("column '", spec$name,
                        "' has a group pattern but no group_row was supplied"))
      hits <- hits[!is.na(groups[hits]) & str_detect(groups[hits], spec$group)]
    }
    sih_assert(length(hits) == 1L, file, sheet,
               paste0("header pattern '", spec$pattern, "' for column '",
                      spec$name, "' matched ", length(hits),
                      " columns (expected exactly 1)"))
    resolved[s] <- hits[[1L]]
  }

  sih_assert(!is.unsorted(resolved, strictly = TRUE), file, sheet,
             "anchored columns are not in left-to-right sheet order")
  stats::setNames(resolved, vapply(column_spec, function(s) s$name,
                                   character(1)))
}

sih_assert_no_duplicates <- function(df, file, sheet,
                                     key_cols = SIH_ESTIMATE_KEY) {
  if (nrow(df) == 0) {
    return(invisible(df))
  }
  keys <- intersect(key_cols, names(df))
  dup_count <- sum(duplicated(df[keys]))
  sih_assert(dup_count == 0, file, sheet,
             paste0(dup_count, " duplicate estimate keys (",
                    paste(keys, collapse = ", "), ")"))
  invisible(df)
}

# ------------------------------------------------------------------------------
# Engine: columns-down cross-section sheets (Files 3, 4, 5, 6, 9, 11)
# ------------------------------------------------------------------------------
# Layout family: a header band directly above the ESTIMATES marker names the
# data columns; below it, label rows run down column A with section headers
# (rows with no numeric data) setting breakdown context.
#
# `emit(label, unit, section, values)` maps one data row to output rows;
# `values` is a named numeric vector in column_spec order. Section handling
# matches the legacy parsers exactly: any non-skipped row without numeric data
# becomes the current section.
sih_parse_columns_down <- function(file, sheet, column_spec, emit,
                                   label_skip_pattern,
                                   header_offset = 1L,
                                   group_offset = NULL,
                                   key_cols = SIH_ESTIMATE_KEY) {
  raw <- read_sheet_raw(file, sheet)
  est_row <- require_label_row(raw, "^ESTIMATES", file, sheet,
                               "ESTIMATES block marker")
  header_rows <- est_row - header_offset
  sih_assert(all(header_rows >= 1L), file, sheet,
             "header band sits above the top of the sheet")
  group_row <- if (!is.null(group_offset)) est_row - group_offset
  cols <- anchor_columns(raw, header_rows, column_spec, file, sheet,
                         group_row = group_row)

  bounds <- find_block_bounds(raw, file, sheet)
  block <- raw[bounds[["first"]]:bounds[["last"]], , drop = FALSE]

  results <- list()
  current_section <- NA_character_

  for (i in seq_len(nrow(block))) {
    row <- block[i, ]
    label <- str_trim(as.character(row[[1]]))
    unit_val <- if (ncol(block) >= 2L) {
      str_trim(as.character(row[[2]]))
    } else {
      NA_character_
    }

    if (is.na(label) || label == "" || label == "NA") next
    if (str_detect(label, label_skip_pattern)) next

    values <- as_numeric_clean(
      vapply(cols, function(k) as.character(row[[k]]), character(1))
    )
    names(values) <- names(cols)

    if (all(is.na(values))) {
      current_section <- label
      next
    }

    emitted <- emit(label = label, unit = unit_val,
                    section = current_section, values = values)
    if (!is.null(emitted) && nrow(emitted) > 0) {
      results[[length(results) + 1L]] <- emitted
    }
  }

  out <- bind_rows(results)
  sih_assert(nrow(out) > 0, file, sheet, "no estimate rows parsed")
  sih_assert_no_duplicates(out, file, sheet, key_cols)
  out
}

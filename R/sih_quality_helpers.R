# SIH estimate-quality helper functions.

sih_quality_key_cols <- function() {
  c(
    "survey_year",
    "metric",
    "tenure",
    "breakdown_var",
    "breakdown_val",
    "geography",
    "stat_type"
  )
}

sih_reliability_marker <- function(reliability_flag) {
  flags <- as.character(reliability_flag)
  flags[is.na(flags)] <- ""
  ifelse(flags %in% c("use_with_caution", "too_unreliable"), "\u2020", "")
}

sih_reliability_label <- function(reliability_flag) {
  flags <- as.character(reliability_flag)
  flags[is.na(flags) | !nzchar(flags)] <- "not_available"

  dplyr::case_when(
    flags == "standard" ~ "standard",
    flags == "use_with_caution" ~ "interpret with caution",
    flags == "too_unreliable" ~ "too unreliable",
    TRUE ~ "not available"
  )
}

sih_quality_hover_text <- function(rse_pct = NA_real_,
                                   moe_95 = NA_real_,
                                   reliability_flag = NA_character_) {
  rse_pct <- as.numeric(rse_pct)
  moe_95 <- as.numeric(moe_95)
  reliability_flag <- as.character(reliability_flag)

  n <- max(length(rse_pct), length(moe_95), length(reliability_flag), 1)
  rse_pct <- rep_len(rse_pct, n)
  moe_95 <- rep_len(moe_95, n)
  reliability_flag <- rep_len(reliability_flag, n)

  vapply(seq_len(n), function(i) {
    has_rse <- !is.na(rse_pct[[i]])
    has_moe <- !is.na(moe_95[[i]])

    if (!has_rse && !has_moe) {
      return("SIH quality metadata: not available for this estimate")
    }

    parts <- character()
    if (has_rse) {
      parts <- c(parts, sprintf("RSE: %.1f%%", rse_pct[[i]]))
    }
    if (has_moe) {
      parts <- c(parts, sprintf("95%% margin of error: %.1f pp", moe_95[[i]]))
    }

    label <- sih_reliability_label(reliability_flag[[i]])
    if (!identical(label, "not available")) {
      parts <- c(parts, paste("Reliability:", label))
    }

    paste(parts, collapse = "<br>")
  }, character(1))
}

sih_quality_empty_join <- function(estimates) {
  n <- nrow(estimates)
  estimates$rse_pct <- rep(NA_real_, n)
  estimates$rse_reliability_flag <- rep(NA_character_, n)
  estimates$rse_reliability_note <- rep(NA_character_, n)
  estimates$moe_95 <- rep(NA_real_, n)
  estimates$has_quality_metadata <- rep(FALSE, n)
  estimates$reliability_marker <- rep("", n)
  estimates$quality_hover <- sih_quality_hover_text(
    estimates$rse_pct,
    estimates$moe_95,
    estimates$rse_reliability_flag
  )
  estimates
}

join_sih_quality <- function(estimates, quality = NULL) {
  if (!is.data.frame(estimates)) {
    stop("estimates must be a data frame.", call. = FALSE)
  }

  if (is.null(quality)) {
    if (exists("sih_quality", inherits = TRUE)) {
      quality <- get("sih_quality", inherits = TRUE)
    } else {
      quality <- data.frame()
    }
  }
  if (!is.data.frame(quality)) {
    stop("quality must be a data frame.", call. = FALSE)
  }

  helper_cols <- c(
    "rse_pct",
    "rse_reliability_flag",
    "rse_reliability_note",
    "moe_95",
    "has_quality_metadata",
    "reliability_marker",
    "quality_hover"
  )
  estimates <- estimates[, setdiff(names(estimates), helper_cols), drop = FALSE]

  keys <- sih_quality_key_cols()
  required_quality_cols <- c(
    keys,
    "quality_measure",
    "quality_value",
    "reliability_flag",
    "reliability_note"
  )
  if (!all(keys %in% names(estimates)) ||
      !all(required_quality_cols %in% names(quality))) {
    return(sih_quality_empty_join(estimates))
  }

  rse_quality <- quality[quality$quality_measure %in% "rse_pct",
                         c(keys, "quality_value", "reliability_flag",
                           "reliability_note"),
                         drop = FALSE]
  if (nrow(rse_quality) > 0) {
    rse_quality <- rse_quality[!duplicated(rse_quality[keys]), , drop = FALSE]
    names(rse_quality)[names(rse_quality) == "quality_value"] <- "rse_pct"
    names(rse_quality)[names(rse_quality) == "reliability_flag"] <- "rse_reliability_flag"
    names(rse_quality)[names(rse_quality) == "reliability_note"] <- "rse_reliability_note"
  } else {
    rse_quality <- data.frame()
  }

  moe_quality <- quality[quality$quality_measure %in% "moe_95",
                         c(keys, "quality_value"),
                         drop = FALSE]
  if (nrow(moe_quality) > 0) {
    moe_quality <- moe_quality[!duplicated(moe_quality[keys]), , drop = FALSE]
    names(moe_quality)[names(moe_quality) == "quality_value"] <- "moe_95"
  } else {
    moe_quality <- data.frame()
  }

  joined <- estimates
  if (nrow(rse_quality) > 0) {
    joined <- dplyr::left_join(joined, rse_quality, by = keys)
  } else {
    joined$rse_pct <- NA_real_
    joined$rse_reliability_flag <- NA_character_
    joined$rse_reliability_note <- NA_character_
  }
  if (nrow(moe_quality) > 0) {
    joined <- dplyr::left_join(joined, moe_quality, by = keys)
  } else {
    joined$moe_95 <- NA_real_
  }

  joined$has_quality_metadata <- !is.na(joined$rse_pct) | !is.na(joined$moe_95)
  joined$reliability_marker <- sih_reliability_marker(joined$rse_reliability_flag)
  joined$quality_hover <- sih_quality_hover_text(
    joined$rse_pct,
    joined$moe_95,
    joined$rse_reliability_flag
  )

  joined
}

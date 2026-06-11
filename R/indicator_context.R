# Indicator confidence, provenance and quality-coverage helpers.

if (!exists("indicator_registry", mode = "function", inherits = TRUE)) {
  registry_path <- if (exists("project_path", mode = "function", inherits = TRUE)) {
    project_path("R", "indicator_registry.R")
  } else {
    file.path("R", "indicator_registry.R")
  }
  if (!file.exists(registry_path)) {
    stop("Could not locate R/indicator_registry.R for indicator context.",
         call. = FALSE)
  }
  source(registry_path, local = environment())
}

if (!exists("read_data_vintage", mode = "function", inherits = TRUE)) {
  vintage_path <- if (exists("project_path", mode = "function", inherits = TRUE)) {
    project_path("R", "data_vintage.R")
  } else {
    file.path("R", "data_vintage.R")
  }
  if (file.exists(vintage_path)) {
    source(vintage_path, local = environment())
  }
}

indicator_measure_class_label <- function(measure_class) {
  labels <- c(
    official_survey = "Official survey",
    official_aggregate = "Official aggregate",
    derived_index = "Derived index",
    stylised_scenario = "Stylised scenario",
    context_series = "Context series"
  )
  unname(ifelse(measure_class %in% names(labels),
                labels[measure_class],
                measure_class))
}

indicator_context_table <- function(data_dir = project_path("data"),
                                    vintage = read_data_vintage(data_dir, fallback = TRUE)) {
  registry <- indicator_registry()
  if (nrow(registry) == 0) {
    return(registry)
  }

  vintage_lookup <- vintage
  if (nrow(vintage_lookup) == 0) {
    vintage_lookup <- data.frame(
      dataset = registry$vintage_dataset,
      period_min = "",
      period_max = "",
      source_group = "",
      stringsAsFactors = FALSE
    )
  }
  vintage_lookup <- vintage_lookup[
    !duplicated(vintage_lookup$dataset),
    c("dataset", "period_min", "period_max", "source_group"),
    drop = FALSE
  ]

  context <- merge(
    registry,
    vintage_lookup,
    by.x = "vintage_dataset",
    by.y = "dataset",
    all.x = TRUE,
    sort = FALSE
  )
  context <- context[match(registry$indicator, context$indicator), ,
                     drop = FALSE]

  for (column in c("period_min", "period_max", "source_group")) {
    context[[column]][is.na(context[[column]])] <- ""
  }
  context$measure_class_label <- indicator_measure_class_label(
    context$measure_class
  )
  context
}

indicator_confidence_note <- function(indicator,
                                      data_dir = project_path("data"),
                                      context = indicator_context_table(data_dir)) {
  row <- context[context$indicator == indicator, , drop = FALSE]
  if (nrow(row) == 0) {
    stop("Unknown indicator context: ", indicator, call. = FALSE)
  }

  paste0(
    row$measure_class_label[[1]],
    " | ",
    row$methodology_version[[1]],
    " | Latest observation: ",
    row$period_max[[1]],
    " | ",
    row$quality_note[[1]],
    " ",
    row$public_caveat[[1]]
  )
}

indicator_context_badges <- function(indicator,
                                     data_dir = project_path("data"),
                                     context = indicator_context_table(data_dir)) {
  note <- indicator_confidence_note(indicator, data_dir, context)
  row <- context[context$indicator == indicator, , drop = FALSE]

  if (exists("tags", mode = "function", inherits = TRUE) ||
      "package:shiny" %in% search()) {
    return(tags$span(
      class = paste("indicator-confidence-badge",
                    paste0("indicator-confidence-", row$measure_class[[1]])),
      title = note,
      `aria-label` = note,
      row$measure_class_label[[1]]
    ))
  }

  note
}

indicator_quality_coverage_summary <- function(data_dir = project_path("data"),
                                               context = indicator_context_table(data_dir)) {
  if (nrow(context) == 0) {
    return(data.frame(
      measure_class = character(),
      indicators = integer(),
      latest_period = character(),
      quality_note = character(),
      stringsAsFactors = FALSE
    ))
  }

  split_context <- split(context, context$measure_class)
  rows <- lapply(names(split_context), function(measure_class) {
    d <- split_context[[measure_class]]
    latest_periods <- d$period_max[!is.na(d$period_max) & nzchar(d$period_max)]
    latest_period <- if (length(latest_periods) == 0) "" else max(latest_periods)
    data.frame(
      measure_class = measure_class,
      indicators = nrow(d),
      latest_period = latest_period,
      quality_note = paste(unique(d$quality_note), collapse = " | "),
      stringsAsFactors = FALSE
    )
  })

  summary <- do.call(rbind, rows)
  summary[order(match(summary$measure_class, c(
    "official_survey",
    "official_aggregate",
    "derived_index",
    "stylised_scenario",
    "context_series"
  ))), , drop = FALSE]
}

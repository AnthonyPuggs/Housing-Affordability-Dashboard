# Public-policy report UI helpers.

policy_compact_class <- function(...) {
  classes <- unlist(list(...), use.names = FALSE)
  classes <- classes[!is.na(classes) & nzchar(classes)]
  paste(unique(classes), collapse = " ")
}

policy_page_header <- function(title, subtitle = NULL, actions = NULL) {
  div(
    class = "policy-page-header",
    div(
      class = "policy-page-header-copy",
      tags$h2(title, class = "policy-page-title"),
      if (!is.null(subtitle)) {
        tags$p(subtitle, class = "policy-page-subtitle")
      }
    ),
    if (!is.null(actions)) {
      div(class = "policy-page-actions", actions)
    }
  )
}

policy_source_note <- function(...) {
  tags$p(..., class = "source-note policy-source-note")
}

policy_kpi_box <- function(title, value, subtitle = NULL, change = NULL,
                           accent = c("blue", "teal", "navy", "purple"),
                           class = NULL) {
  accent <- match.arg(accent)
  value_box(
    title = title,
    value = value,
    if (!is.null(subtitle)) subtitle,
    if (!is.null(change)) change,
    class = policy_compact_class("policy-kpi", paste0("policy-kpi-", accent),
                                 class),
    fill = FALSE
  )
}

policy_card <- function(title, ..., note = NULL, footer = NULL, fill = FALSE,
                        class = NULL) {
  note_tag <- if (is.null(note)) NULL else if (
    inherits(note, "shiny.tag") || inherits(note, "shiny.tag.list")
  ) note else policy_source_note(note)

  card(
    fill = fill,
    class = policy_compact_class("policy-card", class),
    card_header(tags$span(title, class = "policy-card-title"),
                class = "policy-card-header"),
    card_body(
      note_tag,
      ...,
      class = policy_compact_class(
        "policy-card-body",
        if (!is.null(note_tag)) "policy-card-body-with-note"
      )
    ),
    if (!is.null(footer)) footer
  )
}

policy_chart_card <- function(title, ..., note = NULL, footer = NULL,
                              fill = FALSE, class = NULL) {
  policy_card(
    title = title,
    ...,
    note = note,
    footer = footer,
    fill = fill,
    class = policy_compact_class("policy-chart-card", class)
  )
}

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

# Bootstrap tooltip via bslib::tooltip() (UX-12): dismissible and positioned
# by Popper, replacing the old CSS ::before/::after hover tooltip that could
# not be dismissed without moving the pointer (WCAG 1.4.13).
policy_info_icon <- function(label, tooltip, class = NULL) {
  bslib::tooltip(
    tags$span(
      class = policy_compact_class("policy-info-icon", class),
      tabindex = "0",
      `aria-label` = paste0(label, ": ", tooltip),
      tags$i(class = "fa-solid fa-circle-info", `aria-hidden` = "true")
    ),
    tooltip,
    placement = "bottom"
  )
}

# Official-vs-stylised marker (UX-08): the dashboard's core credibility rule is
# that official ABS/NHHA pass-through cells and stylised modelled scenarios must
# never read as the same thing. data_class adds a small labelled badge and a
# screen-reader description so the distinction is carried on the KPI itself, not
# only in surrounding prose. Default "none" leaves existing KPIs unchanged.
policy_kpi_box <- function(title, value, subtitle = NULL, change = NULL,
                           accent = c("blue", "teal", "navy", "purple"),
                           data_class = c("none", "official", "stylised"),
                           class = NULL) {
  accent <- match.arg(accent)
  data_class <- match.arg(data_class)

  title_tag <- if (identical(data_class, "none")) {
    title
  } else {
    aria <- if (identical(data_class, "official")) {
      "Official ABS or NHHA measure"
    } else {
      "Stylised modelled scenario, not an official ABS measure or lender assessment"
    }
    tagList(
      tags$span(title),
      tags$span(
        if (identical(data_class, "official")) "Official" else "Stylised",
        class = paste0("policy-kpi-badge policy-kpi-badge-", data_class),
        `aria-label` = aria
      )
    )
  }

  value_box(
    title = title_tag,
    value = value,
    if (!is.null(subtitle)) subtitle,
    if (!is.null(change)) change,
    class = policy_compact_class(
      "policy-kpi", paste0("policy-kpi-", accent),
      if (!identical(data_class, "none")) paste0("policy-kpi-", data_class),
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

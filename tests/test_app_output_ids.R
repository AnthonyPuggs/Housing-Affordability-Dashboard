app_path <- file.path(getwd(), "app.R")
if (!file.exists(app_path)) {
  stop("app.R not found; run this test from the project root.", call. = FALSE)
}

app_text <- paste(readLines(app_path, warn = FALSE), collapse = "\n")

extract_matches <- function(pattern, text) {
  matches <- gregexpr(pattern, text, perl = TRUE)
  values <- regmatches(text, matches)[[1]]
  if (identical(values, character(0)) || length(values) == 0) {
    return(character())
  }
  sub(pattern, "\\1", values, perl = TRUE)
}

ui_plotly_ids <- unique(extract_matches(
  "plotlyOutput\\(\\s*[\"']([A-Za-z0-9_]+)[\"']",
  app_text
))

server_plotly_ids <- unique(extract_matches(
  "output\\$([A-Za-z0-9_]+)\\s*<-\\s*renderPlotly\\s*\\(",
  app_text
))

missing_server <- setdiff(ui_plotly_ids, server_plotly_ids)

if (length(missing_server) > 0) {
  stop(
    "plotlyOutput IDs without matching renderPlotly outputs: ",
    paste(sort(missing_server), collapse = ", "),
    call. = FALSE
  )
}

cat("App Plotly output ID check passed for", length(ui_plotly_ids), "outputs.\n")

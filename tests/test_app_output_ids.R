app_path <- file.path(getwd(), "app.R")
if (!file.exists(app_path)) {
  stop("app.R not found; run this test from the project root.", call. = FALSE)
}

module_paths <- list.files(file.path(getwd(), "R"),
                           pattern = "_module[.]R$",
                           full.names = TRUE)
source_paths <- c(app_path, module_paths)
app_text <- paste(vapply(source_paths, function(path) {
  paste(readLines(path, warn = FALSE), collapse = "\n")
}, character(1)), collapse = "\n")

extract_matches <- function(pattern, text) {
  matches <- gregexpr(pattern, text, perl = TRUE)
  values <- regmatches(text, matches)[[1]]
  if (identical(values, character(0)) || length(values) == 0) {
    return(character())
  }
  sub(pattern, "\\1", values, perl = TRUE)
}

ui_plotly_ids <- unique(c(extract_matches(
  "plotlyOutput\\(\\s*[\"']([A-Za-z0-9_]+)[\"']",
  app_text
), extract_matches(
  "plotlyOutput\\(\\s*ns\\(\\s*[\"']([A-Za-z0-9_]+)[\"']",
  app_text
)))

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

cat("App Plotly output ID check passed for", length(ui_plotly_ids),
    "outputs across app and modules.\n")

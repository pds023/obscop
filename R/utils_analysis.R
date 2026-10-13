# Independently testable transformations shared by the server.
keep_choice <- function(current, choices, default = choices[1]) {
  if (length(current) == 1 && !is.na(current) && current %in% choices) current else if (length(default) && !is.na(default) && default %in% choices) default else if (length(choices)) choices[1] else character()
}
encode_item <- function(x) ifelse(nzchar(x), x, "__whole_question__")
decode_item <- function(x) ifelse(x == "__whole_question__", "", x)
format_number <- function(x, digits = 0) {
  ifelse(is.na(x), "—", format(round(x, digits), trim = TRUE, big.mark = " ", decimal.mark = ",", scientific = FALSE))
}
format_value <- function(x, unit) paste0(format_number(x, ifelse(unit == "%", 0, 2)), ifelse(unit == "%", " %", ""))
metric_card <- function(label, value, note) {
  shiny::tags$div(class = "metric-card", shiny::tags$div(class = "metric-label", label), shiny::tags$div(class = "metric-value", value), shiny::tags$div(class = "metric-note", note))
}
response_rows <- function(data, unit, include_totals = FALSE) {
  data[data$unit == unit & (include_totals | !data$is_subtotal), , drop = FALSE]
}
compare_values <- function(data, country_a, country_b) {
  a <- data[data$group_label == country_a, c("label", "value", "unit"), drop = FALSE]
  b <- data[data$group_label == country_b, c("label", "value", "unit"), drop = FALSE]
  if (anyDuplicated(a$label) || anyDuplicated(b$label)) stop("La comparaison exige une seule valeur par réponse et par pays.")
  names(a)[2] <- "a"
  names(b)[2] <- "b"
  d <- merge(a, b, by = c("label", "unit"))
  d$delta <- d$a - d$b
  d$a_label <- rep(country_a, nrow(d))
  d$b_label <- rep(country_b, nrow(d))
  d[order(abs(d$delta), decreasing = TRUE), , drop = FALSE]
}
pair_indicators <- function(x, y) {
  if (anyDuplicated(x$group_label) || anyDuplicated(y$group_label)) stop("Les indicateurs doivent contenir une seule ligne par pays.")
  a <- x[c("group_label", "value", "n", "base", "unit")]
  b <- y[c("group_label", "value", "n", "base", "unit")]
  names(a) <- c("group_label", "x", "n", "base_x", "x_unit")
  names(b) <- c("group_label", "y", "n_y", "base_y", "y_unit")
  if (all(c("question", "item", "answer") %in% names(x))) a$x_label <- paste(x$question, x$item, x$answer, sep = " · ")
  if (all(c("question", "item", "answer") %in% names(y))) b$y_label <- paste(y$question, y$item, y$answer, sep = " · ")
  merge(a, b, by = "group_label")
}
export_rows <- function(data, attribution = TRUE) {
  cols <- c(year = "Année", group_label = "Population", filter = "Filtre", theme_label = "Thématique", question_label = "Question / base", item = "Détail", answer = "Réponse", value = "Valeur", unit = "Unité", n = "Échantillon", base = "Base", question_id = "Identifiant question", series = "Série", source_question = "Question source", question_en = "Question (EN)", item_en = "Détail (EN)", answer_en = "Réponse (EN)", population_en = "Population EN source", source_unit = "Unité source", source_base = "Base source", source_n = "Échantillon source", source_answer = "Réponse source", source_value = "Valeur source", source_row = "Ligne source", quality_note = "Note qualité")
  cols <- cols[names(cols) %in% names(data)]
  result <- data[names(cols)]
  names(result) <- unname(cols)
  if (attribution) {
    result$Source <- rep(obscop_source(), nrow(result))
    result$Licence <- rep("CC BY-NC-SA 4.0", nrow(result))
  }
  result
}
write_export <- function(data, path) {
  data.table::fwrite(export_rows(data), file = path, sep = ";", dec = ",", bom = TRUE, na = "", quote = "auto")
}


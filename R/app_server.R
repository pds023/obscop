#' The application server-side
#' @param input,output,session Shiny session objects.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  data <- obscop_data()
  countries <- sort(unique(data$group_label[data$is_country]))
  geographies <- c("Monde", countries, sort(unique(data$group_label[data$is_region])))
  themes <- unique(data[c("theme_code", "theme_label")])
  questions <- unique(data[c("question_id", "question_label", "question_code", "theme_code")])
  default_question <- questions$question_id[questions$question_code == "B1"][1]
  default_y <- questions$question_id[questions$question_code == "B17"][1]
  updateSelectInput(session, "year", choices = sort(unique(data$year)), selected = max(data$year))
  updateSelectInput(session, "theme", choices = stats::setNames(themes$theme_code, themes$theme_label), selected = "B")
  updateSelectizeInput(session, "countries", choices = countries, selected = countries, server = TRUE)
  updateSelectInput(session, "explore_country", choices = geographies, selected = "France")
  updateSelectizeInput(session, "y_question", choices = stats::setNames(questions$question_id, questions$question_label), selected = default_y, server = TRUE)

  observeEvent(input$theme, {
    q <- questions[questions$theme_code == input$theme, ]
    selected <- keep_choice(isolate(input$question), q$question_id, if (input$theme == "B") default_question else q$question_id[1])
    updateSelectizeInput(session, "question", choices = stats::setNames(q$question_id, q$question_label), selected = selected, server = TRUE)
  })
  question_data <- reactive({
    req(input$year, input$question)
    data[data$year == as.integer(input$year) & data$question_id == input$question, , drop = FALSE]
  })
  observeEvent(question_data(), {
    items <- unique(question_data()$item)
    choices <- stats::setNames(encode_item(items), ifelse(nzchar(items), items, "Question entière"))
    updateSelectInput(session, "detail", choices = choices, selected = keep_choice(isolate(input$detail), unname(choices)))
  })
  item_data <- reactive({
    req(input$detail)
    d <- question_data()
    d[d$item == decode_item(input$detail), , drop = FALSE]
  })
  observeEvent(item_data(), {
    answers <- unique(item_data()$answer)
    preferred <- answers[grepl("^(SI OUI|ST OUI|ST Favorable|ST D'accord|ST 9 à 10)$", answers, ignore.case = TRUE)]
    updateSelectInput(session, "answer", choices = answers, selected = keep_choice(isolate(input$answer), answers, if (length(preferred)) preferred[1] else answers[1]))
  })
  indicator_data <- reactive({
    req(input$answer)
    d <- item_data()
    d[d$answer == input$answer, , drop = FALSE]
  })
  country_data <- reactive({
    d <- indicator_data()
    d[d$is_country & d$group_label %in% input$countries, , drop = FALSE]
  })
  reference_data <- reactive({
    d <- indicator_data()
    d[d$is_world, , drop = FALSE]
  })
  unit <- reactive({
    u <- unique(indicator_data()$unit)
    req(length(u) == 1)
    u
  })
  reference <- reactive({
    d <- reference_data()
    if (nrow(d) == 1) d$value else NULL
  })
  observeEvent(input$countries, {
    choices <- countries[countries %in% input$countries]
    updateSelectInput(session, "compare_a", choices = choices, selected = keep_choice(isolate(input$compare_a), choices, if ("France" %in% choices) "France" else choices[1]))
    updateSelectInput(session, "compare_b", choices = choices, selected = keep_choice(isolate(input$compare_b), choices, if ("Allemagne" %in% choices) "Allemagne" else choices[min(2, length(choices))]))
  }, ignoreNULL = FALSE)
  observeEvent(input$select_all_countries, updateSelectizeInput(session, "countries", selected = countries))
  observeEvent(input$clear_countries, updateSelectizeInput(session, "countries", selected = character()))
  observeEvent(input$reset, {
    updateSelectInput(session, "theme", selected = "B")
    updateSelectizeInput(session, "question", selected = default_question)
    updateSelectInput(session, "detail", selected = "__whole_question__")
    updateSelectInput(session, "answer", selected = "SI OUI")
    updateSelectizeInput(session, "countries", selected = countries)
    updateSelectInput(session, "explore_country", selected = "France")
    updateRadioButtons(session, "explore_mode", selected = "answers")
    updateCheckboxInput(session, "include_totals", value = FALSE)
    updateRadioButtons(session, "heatmap_axis", selected = "answers")
    updateSelectInput(session, "profile_filter", selected = "Âge")
    updateSelectInput(session, "compare_a", selected = "France")
    updateSelectInput(session, "compare_b", selected = "Allemagne")
    updateSelectizeInput(session, "y_question", selected = default_y)
    updateSelectInput(session, "y_detail", selected = "__whole_question__")
    updateSelectInput(session, "y_answer", selected = "ST 9 à 10")
  })
  output$selection_note <- renderUI({
    tags$small(sprintf("%s pays disponibles. Les profils de population se trouvent dans l’onglet Profils.", nrow(country_data())))
  })
  output$context <- renderUI({
    d <- indicator_data()
    req(nrow(d))
    tagList(tags$span(class = "context-chip", d$theme_label[1]), tags$span(class = "context-chip", paste("Base :", if (nzchar(d$base[1])) d$base[1] else "non renseignée")), tags$span(class = "context-chip", paste("Édition", input$year)))
  })
  output$indicator_title <- renderUI({
    d <- indicator_data()
    req(nrow(d))
    tagList(tags$h2(d$question[1]), if (nzchar(d$item[1])) tags$p(class = "indicator-detail", d$item[1]), tags$p(class = "indicator-answer", d$answer[1]), if (any(nzchar(d$quality_note))) tags$p(class = "quality-note", unique(d$quality_note[nzchar(d$quality_note)])[1]))
  })
  output$metrics <- renderUI({
    d <- country_data()
    r <- reference_data()
    top <- if (nrow(d)) d[which.max(d$value), , drop = FALSE] else NULL
    spread <- if (nrow(d) >= 2) diff(range(d$value, na.rm = TRUE)) else NA_real_
    tags$div(class = "metric-grid",
      metric_card("Ensemble international", if (nrow(r)) format_value(r$value[1], unit()) else "—", if (nrow(r) && !is.na(r$n[1])) paste("Base publiée :", format_number(r$n[1]), "répondants") else "Effectif non renseigné ou ambigu"),
      metric_card("Valeur la plus élevée", if (!is.null(top)) format_value(top$value, unit()) else "—", if (!is.null(top)) paste(top$group_label, "· parmi les pays sélectionnés") else "Sélectionnez au moins un pays"),
      metric_card("Écart entre pays", if (is.finite(spread)) paste(format_number(spread, 1), if (unit() == "%") "pts" else "unités") else "—", "Maximum − minimum de la sélection"),
      metric_card("Pays disponibles", paste(nrow(d), "/", length(input$countries)), "Une absence de donnée n’est pas un zéro")
    )
  })
  output$world_map <- plotly::renderPlotly({
    d <- country_data()
    validate(need(nrow(d) > 0, "Sélectionnez des pays disposant de cet indicateur."))
    obscop_map(d)
  })
  output$ranking <- plotly::renderPlotly({
    d <- country_data()
    validate(need(nrow(d) > 0, "Aucun pays disponible pour cette sélection."))
    obscop_bars(d, reference = reference())
  })
  output$overview_insight <- renderUI({
    d <- country_data()
    req(nrow(d))
    high <- d[which.max(d$value), ]
    low <- d[which.min(d$value), ]
    tags$div(class = "insight-note", tags$strong("Dans votre sélection. "), sprintf("%s affiche la valeur la plus élevée (%s), %s la plus faible (%s). ", high$group_label, format_value(high$value, unit()), low$group_label, format_value(low$value, unit())), "La référence internationale est celle publiée dans le fichier, sans recalcul à partir des pays.")
  })
  distribution_data <- reactive({
    req(input$explore_country, input$explore_mode)
    if (input$explore_mode == "details") {
      d <- question_data()
      d <- d[d$answer == input$answer & d$unit == unit(), , drop = FALSE]
      d$display_label <- ifelse(nzchar(d$item), d$item, d$answer)
    } else {
      d <- response_rows(item_data(), unit(), isTRUE(input$include_totals))
      d$display_label <- d$answer
    }
    d[d$group_label == input$explore_country & (d$is_country | d$is_world | d$is_region), , drop = FALSE]
  })
  output$distribution <- plotly::renderPlotly({
    d <- distribution_data()
    validate(need(nrow(d) > 0, "Aucune réponse disponible. Essayez un autre territoire ou activez les sous-totaux."))
    obscop_bars(d, label = "display_label")
  })
  output$distribution_note <- renderUI({
    d <- distribution_data()
    req(nrow(d))
    tags$div(class = "insight-note", tags$strong(paste(input$explore_country, "·", d$base[1])), tags$p("Les barres représentent les valeurs publiées. Les questions à choix multiples peuvent dépasser 100 % au total. Les sous-totaux recouvrent d’autres réponses et ne doivent pas leur être additionnés. Seules les réponses de même unité que l’indicateur sélectionné sont affichées."))
  })
  output$heatmap <- plotly::renderPlotly({
    if (identical(input$heatmap_axis, "details")) {
      d <- question_data()
      d <- d[d$answer == input$answer & d$unit == unit(), , drop = FALSE]
      d$answer <- ifelse(nzchar(d$item), d$item, d$answer)
    } else {
      d <- response_rows(item_data(), unit(), isTRUE(input$include_totals))
    }
    d <- d[d$is_country & d$group_label %in% input$countries, , drop = FALSE]
    validate(need(nrow(d) > 0, "Aucune réponse disponible pour ces pays et cette unité."))
    obscop_heatmap(d)
  })
  gap_data <- reactive({
    req(input$compare_a, input$compare_b)
    d <- question_data()
    if (length(unique(d$item)) > 1) {
      d <- d[d$answer == input$answer & d$unit == unit(), , drop = FALSE]
      d$label <- d$item
    } else {
      d <- response_rows(item_data(), unit(), isTRUE(input$include_totals))
      d$label <- d$answer
    }
    compare_values(d[d$is_country, , drop = FALSE], input$compare_a, input$compare_b)
  })
  output$gap_plot <- plotly::renderPlotly({
    validate(need(!identical(input$compare_a, input$compare_b), "Choisissez deux pays différents."))
    d <- gap_data()
    validate(need(nrow(d) > 0, "Aucune valeur commune entre ces deux pays."))
    obscop_gap(d)
  })
  observeEvent(indicator_data(), {
    d <- indicator_data()
    filters <- unique(d$filter[!d$is_country & !d$is_world & !d$is_region])
    updateSelectInput(session, "profile_filter", choices = filters, selected = keep_choice(isolate(input$profile_filter), filters, if ("Âge" %in% filters) "Âge" else filters[1]))
  })
  profile_data <- reactive({
    req(input$profile_filter)
    d <- indicator_data()
    d[d$filter == input$profile_filter, , drop = FALSE]
  })
  output$profile_plot <- plotly::renderPlotly({
    d <- profile_data()
    validate(need(nrow(d) > 0, "Ce profil n’est pas disponible pour l’indicateur sélectionné."))
    obscop_bars(d, reference = reference())
  })
  output$profile_note <- renderUI({
    d <- profile_data()
    req(nrow(d))
    tags$div(class = "insight-note", tags$strong("Profils de l’ensemble international. "), "Ces résultats sont déjà agrégés. La sélection des pays ne s’applique pas ici. Certaines catégories se recouvrent ; les filtres ne peuvent pas être croisés entre eux. Les différences sont descriptives et ne constituent pas des tests de significativité.", if (any(is.na(d$n) | d$n < 100)) tags$p("Au moins une catégorie a un effectif inférieur à 100 ou non renseigné : interprétez ces résultats avec prudence."))
  })
  y_question_data <- reactive({
    req(input$year, input$y_question)
    data[data$year == as.integer(input$year) & data$question_id == input$y_question, , drop = FALSE]
  })
  observeEvent(y_question_data(), {
    items <- unique(y_question_data()$item)
    choices <- stats::setNames(encode_item(items), ifelse(nzchar(items), items, "Question entière"))
    updateSelectInput(session, "y_detail", choices = choices, selected = keep_choice(isolate(input$y_detail), unname(choices)))
  })
  y_item_data <- reactive({
    req(input$y_detail)
    d <- y_question_data()
    d[d$item == decode_item(input$y_detail), , drop = FALSE]
  })
  observeEvent(y_item_data(), {
    answers <- unique(y_item_data()$answer)
    preferred <- answers[grepl("^(ST 9 à 10|SI OUI|ST OUI)$", answers, ignore.case = TRUE)]
    updateSelectInput(session, "y_answer", choices = answers, selected = keep_choice(isolate(input$y_answer), answers, if (length(preferred)) preferred[1] else answers[1]))
  })
  relation_data <- reactive({
    req(input$y_answer)
    y <- y_item_data()
    y <- y[y$is_country & y$answer == input$y_answer, , drop = FALSE]
    pair_indicators(country_data(), y)
  })
  output$scatter <- plotly::renderPlotly({
    d <- relation_data()
    validate(need(nrow(d) >= 2, "Il faut au moins deux pays avec des données pour les deux indicateurs."))
    y <- y_item_data()
    y <- y[y$answer == input$y_answer, , drop = FALSE]
    obscop_scatter(d, x_label = paste(input$answer, if (unit() == "%") "(%)" else "(nombre)"), y_label = paste(input$y_answer, if (y$unit[1] == "%") "(%)" else "(nombre)"), show_labels = isTRUE(input$show_labels))
  })
  output$correlation_note <- renderUI({
    d <- relation_data()
    enough <- nrow(d) >= 3 && stats::sd(d$x) > 0 && stats::sd(d$y) > 0
    txt <- if (enough) sprintf("Corrélation de Pearson : %s · %s pays en commun.", format_number(stats::cor(d$x, d$y), 2), nrow(d)) else "Corrélation non calculable : il faut au moins trois pays et deux indicateurs variables."
    tags$div(class = "insight-note", tags$strong(txt), tags$p("Chaque point représente un pays. Cette association entre résultats nationaux n’implique ni causalité ni relation au niveau individuel. Les pays ont le même poids dans ce calcul. Vérifiez les bases des deux questions avant toute interprétation."), tags$p(paste("X :", unique(indicator_data()$question_label)[1])), if (nzchar(decode_item(input$detail))) tags$p(paste("Détail X :", decode_item(input$detail))), tags$p(paste("Y :", unique(y_question_data()$question_label)[1])), if (nzchar(decode_item(input$y_detail))) tags$p(paste("Détail Y :", decode_item(input$y_detail))))
  })
  selection_data <- reactive({
    d <- indicator_data()
    profile <- if (length(input$profile_filter)) d$filter == input$profile_filter else rep(FALSE, nrow(d))
    d[d$is_world | (d$is_country & d$group_label %in% input$countries) | profile, , drop = FALSE]
  })
  table_data <- reactive({
    if (identical(input$data_scope, "all")) data else selection_data()
  })
  output$data_table <- DT::renderDT({
    visible_data <- export_rows(table_data(), attribution = FALSE)
    DT::datatable(visible_data, rownames = FALSE, filter = "top", escape = TRUE,
      extensions = "Buttons", options = list(pageLength = 15, scrollX = TRUE, deferRender = TRUE,
        dom = "Bfrtip", buttons = list(list(extend = "colvis", text = "Colonnes")),
        columnDefs = list(list(targets = seq.int(11, ncol(visible_data) - 1), visible = FALSE)),
        language = list(search = "Rechercher :", info = "_START_ à _END_ sur _TOTAL_ lignes", infoEmpty = "Aucune ligne", infoFiltered = "(sur _MAX_ au total)", zeroRecords = "Aucun résultat", emptyTable = "Aucune donnée disponible", paginate = list(first = "Début", previous = "Précédent", "next" = "Suivant", last = "Fin"))
      ))
  }, server = TRUE)
  output$data_note <- renderUI({
    tags$div(class = "insight-note", paste(format_number(nrow(table_data())), "lignes dans ce périmètre."), " La sélection courante contient la référence internationale, les pays choisis et le profil actif. La recherche du tableau est aussi appliquée à l’export. Le CSV conserve les libellés, les bases et les valeurs source.")
  })
  output$download_selection <- downloadHandler(filename = function() paste0("obscop-", input$year, "-indicateur.csv"), content = function(file) write_export(selection_data(), file))
  output$download_data <- downloadHandler(filename = function() paste0("obscop-", input$year, "-donnees.csv"), content = function(file) {
    d <- table_data()
    rows <- input$data_table_rows_all
    if (!is.null(rows)) d <- d[rows, , drop = FALSE]
    write_export(d, file)
  })
  output$method_content <- renderUI(obscop_method(data))
}


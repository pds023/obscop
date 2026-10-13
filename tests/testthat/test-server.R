test_that("B1 selection powers the overview, exploration, comparison and profiles", {
  shiny::testServer(app_server, {
    session$setInputs(theme = "B", year = "2025", question = "q006",
                     countries = c("France", "Allemagne", "Italie"))
    expect_equal(default_question, "q006")
    expect_true(nrow(question_data()) > 0)
    session$setInputs(detail = "__whole_question__")
    session$setInputs(answer = "SI OUI", explore_country = "France", explore_mode = "answers",
                     include_totals = FALSE, compare_a = "France", compare_b = "Allemagne",
                     profile_filter = "Genre", data_scope = "selection")
    expect_equal(reference(), 90)
    expect_equal(nrow(country_data()), 3)
    expect_equal(country_data()$value[country_data()$group_label == "France"], 91)
    expect_true(all(indicator_data()$answer == "SI OUI"))
    expect_equal(nrow(distribution_data()), 5)
    expect_false(any(distribution_data()$is_subtotal))
    expect_equal(nrow(gap_data()), 5)
    expect_setequal(unique(profile_data()$filter), "Genre")
    expect_false(any(profile_data()$is_country))
    expect_equal(nrow(table_data()), nrow(selection_data()))

    # Inspect the serialized chart: the reference remains the published 90%,
    # even when the country selection does not reproduce the international sample.
    ranking <- jsonlite::fromJSON(output$ranking, simplifyVector = FALSE)
    expect_equal(ranking$x$layout$shapes[[1]]$x0, 90)
    expect_equal(length(ranking$x$data[[1]]$x), 3)
    for (name in c("world_map", "distribution", "heatmap", "gap_plot", "profile_plot")) {
      chart <- jsonlite::fromJSON(output[[name]], simplifyVector = FALSE)
      expect_gt(length(chart$x$data), 0)
    }
    expect_match(output$metrics$html, "90 %", fixed = TRUE)
    expect_match(output$profile_note$html, "sélection des pays ne s’applique pas", fixed = TRUE)

    session$setInputs(countries = "France")
    expect_equal(nrow(country_data()), 1)
    expect_equal(nrow(profile_data()), 2)
    expect_equal(reference(), 90)
    session$setInputs(countries = character())
    expect_equal(nrow(country_data()), 0)
    expect_equal(reference(), 90)
  })
})

test_that("the correlation view supports numeric indicators without percent axes", {
  shiny::testServer(app_server, {
    session$setInputs(theme = "B", year = "2025", countries = c("France", "Allemagne", "Italie"))
    session$setInputs(question = "q006", y_question = "q009")
    session$setInputs(detail = "__whole_question__", y_detail = "__whole_question__")
    session$setInputs(answer = "SI OUI", y_answer = "Moyenne", show_labels = FALSE)
    expect_equal(nrow(relation_data()), 3)
    expect_equal(relation_data()$y[relation_data()$group_label == "France"], 7.2)
    expect_true(all(relation_data()$y_unit == "Nombre"))
    chart <- jsonlite::fromJSON(output$scatter, simplifyVector = FALSE)
    expect_equal(chart$x$layout$xaxis$ticksuffix, " %")
    expect_equal(chart$x$layout$yaxis$ticksuffix, "")
    expect_length(chart$x$layout$yaxis$range, 0)
    expect_equal(chart$x$data[[1]]$mode, "markers")
    expect_match(output$correlation_note$html, "3 pays en commun", fixed = TRUE)
    session$setInputs(countries = "France")
    expect_match(output$correlation_note$html, "non calculable", fixed = TRUE)
  })
})

test_that("numeric statistics and ambiguous heating series remain usable and labelled", {
  shiny::testServer(app_server, {
    session$setInputs(theme = "B", year = "2025", countries = c("France", "Allemagne"))
    session$setInputs(question = "q009")
    session$setInputs(detail = "__whole_question__")
    session$setInputs(answer = "Moyenne", explore_country = "Monde", explore_mode = "answers",
                     compare_a = "France", compare_b = "Allemagne", include_totals = FALSE)
    expect_equal(unit(), "Nombre")
    expect_equal(reference(), 7.6)
    expect_setequal(distribution_data()$answer, c("Moyenne", "Écart-type"))
    expect_true(all(gap_data()$unit == "Nombre"))
    expect_match(output$metrics$html, "7,6", fixed = TRUE)
    expect_false(grepl("7,6 %", output$metrics$html, fixed = TRUE))
    first_series <- unique(data$question_id[data$question_code == "V25Q4" & startsWith(data$series, "Série 1")])
    session$setInputs(theme = "D")
    session$setInputs(question = first_series)
    session$setInputs(detail = "__whole_question__")
    session$setInputs(answer = "Une pompe à chaleur électrique")
    expect_equal(reference(), 27)
    expect_true(all(is.na(reference_data()$n)))
    expect_true(all(is.na(country_data()$n)))
    expect_match(output$indicator_title$html, "Dénominateur non confirmé", fixed = TRUE)
    expect_match(output$metrics$html, "Effectif non renseigné ou ambigu", fixed = TRUE)
    expect_false(any(distribution_data()$is_base_count))
    session$setInputs(answer = unique(data$answer[data$is_base_count]))
    expect_equal(unit(), "Nombre")
    expect_equal(reference(), 3823)
    expect_true(all(distribution_data()$is_base_count))
  })
})

test_that("exploring a matrix question changes detail without mixing responses", {
  shiny::testServer(app_server, {
    session$setInputs(theme = "C", year = "2025", countries = c("France", "Allemagne"))
    c2 <- unique(data$question_id[data$question_code == "C2"])
    session$setInputs(question = c2)
    detail <- unique(data$item[data$question_id == c2])[1]
    answer <- unique(data$answer[data$question_id == c2])[1]
    session$setInputs(detail = encode_item(detail))
    session$setInputs(answer = answer, explore_country = "France", explore_mode = "details",
                     include_totals = FALSE, compare_a = "France", compare_b = "Allemagne")
    expect_equal(length(unique(distribution_data()$item)), 4)
    expect_setequal(distribution_data()$answer, answer)
    expect_equal(nrow(gap_data()), 4)
    chart <- jsonlite::fromJSON(output$distribution, simplifyVector = FALSE)
    expect_equal(length(chart$x$data[[1]]$x), 4)
    session$setInputs(heatmap_axis = "details")
    matrix_chart <- jsonlite::fromJSON(output$heatmap, simplifyVector = FALSE)
    columns <- unlist(matrix_chart$x$layout$xaxis$ticktext)
    rows <- unlist(matrix_chart$x$layout$yaxis$ticktext)
    values <- matrix_chart$x$data[[1]]$z
    expect_equal(length(unique(columns)), 4)
    expect_setequal(rows, c("France", "Allemagne"))
    expect_equal(lengths(values), c(4L, 4L))
    france <- unlist(values[[match("France", rows)]])
    expect_equal(france[grepl("autorités", columns)], 7)
    expect_equal(sort(france), c(7, 11, 11, 11))
  })
})

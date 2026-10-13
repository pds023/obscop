test_that("the published wave keeps all rows and geographical levels separate", {
  d <- obscop_data()
  meta <- obscop_metadata(d)
  expect_equal(nrow(d), 146275L)
  expect_equal(meta$world_sample, 23503L)
  expect_identical(unique(d$year), 2025L)
  expect_equal(nrow(obscop_countries(d)), 30L)
  expect_equal(length(unique(d$population[d$is_region])), 7L)
  expect_false(any(d$is_country & d$is_region))
  expect_true(all(nchar(d$iso3[d$is_country]) == 3L))
  expect_true(all(d$spatial == "Monde"))
  expect_equal(length(unique(d$filter)), 29L)
  expect_invisible(validate_obscop_data(d))
})

test_that("published percentages, subtotals and decimal statistics stay distinct", {
  d <- obscop_data()
  b1 <- d[d$is_world & d$question_code == "B1", ]
  expect_equal(b1$value[b1$answer == "SI OUI"], 90)
  expect_true(b1$is_subtotal[b1$answer == "SI OUI"])
  expect_equal(b1$value[b1$answer == "Oui certainement"], 61)
  b17 <- d[d$is_world & d$question_code == "B17" & d$answer == "Moyenne", ]
  expect_equal(b17$value, 7.6)
  expect_equal(b17$unit, "Nombre")
  expect_true(all(d$value[d$unit == "%"] >= 0 & d$value[d$unit == "%"] <= 100))
})

test_that("different bases and descriptive-statistic contexts never collide", {
  d <- obscop_data()
  b6 <- d[d$question_code == "B6b", ]
  expect_equal(length(unique(b6$question_id)), 2L)
  expect_equal(length(unique(d$question_id[d$question_code == "STAT"])), 4L)
  expect_equal(length(unique(d$question_id[d$question_code == "PLANE2"])), 2L)
  expect_equal(nrow(obscop_questions(d)), 52L)
})

test_that("ambiguous heating series retain source values and hide unreliable bases", {
  d <- obscop_data()
  first <- d[d$question_code == "V25Q4" & startsWith(d$series, "Série 1"), ]
  expect_true(all(is.na(first$n)))
  expect_true(all(!is.na(first$source_n)))
  expect_true(all(nzchar(first$quality_note)))
  counts <- first[first$is_base_count, ]
  expect_equal(nrow(counts), 181L)
  expect_true(all(counts$source_unit == "%" & counts$unit == "Nombre"))
  expect_equal(counts$value[counts$is_world], 3823)
  world <- d[d$is_world & d$question_code == "V25Q4" & d$answer == "Une pompe à chaleur électrique", ]
  expect_equal(world$value, c(27, 4))
  expect_equal(length(unique(world$question_id)), 2L)
})

test_that("the duplicated French B8 label is resolved from the English source", {
  d <- obscop_data()
  production <- d[d$is_world & d$question_code == "B8" &
                    grepl("production des entreprises", d$answer, fixed = TRUE), ]
  expect_equal(nrow(production), 1L)
  expect_equal(production$value, 22)
  expect_match(production$source_answer, "modes de vie", fixed = TRUE)
  expect_match(production$answer_en, "production methods", fixed = TRUE)
})

test_that("invalid schemas and duplicated analytical keys fail instead of aggregating", {
  invalid <- tempfile(fileext = ".csv")
  on.exit(unlink(invalid), add = TRUE)
  writeLines(c("Wrong;Header", "1;2"), invalid)
  expect_error(read_obscop_csv(invalid), "21 colonnes")
  d <- obscop_data()
  expect_error(validate_obscop_data(rbind(d[1L, ], d[1L, ])), "même clé")
  d <- d[1L, ]
  d$value <- 101
  expect_error(validate_obscop_data(d), "Valeurs préparées invalides")
})

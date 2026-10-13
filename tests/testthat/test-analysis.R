test_that("country comparisons use published values and preserve sign", {
  d <- obscop_data()
  b1 <- d[d$question_code == "B1" & d$is_country, ]
  b1$label <- b1$answer
  result <- compare_values(b1, "France", "Allemagne")
  yes <- result[result$label == "SI OUI", ]
  expect_equal(yes$a, 91)
  expect_equal(yes$b, 88)
  expect_equal(yes$delta, 3)
  expect_equal(compare_values(b1, "Allemagne", "France")$delta[
    compare_values(b1, "Allemagne", "France")$label == "SI OUI"], -3)
  expect_equal(nrow(compare_values(b1, "France", "Pays absent")), 0)
  expect_error(compare_values(rbind(b1, b1[1, ]), b1$group_label[1], "France"), "une seule valeur")
})

test_that("response distributions exclude overlapping totals and other units", {
  d <- obscop_data()
  b1 <- d[d$is_world & d$question_code == "B1", ]
  ordinary <- response_rows(b1, "%")
  expect_equal(nrow(ordinary), 5)
  expect_equal(sum(ordinary$value), 100)
  expect_false(any(ordinary$is_subtotal))
  expect_equal(nrow(response_rows(b1, "%", include_totals = TRUE)), 7)
  b17 <- d[d$is_world & d$question_code == "B17", ]
  statistics <- response_rows(b17, "Nombre")
  expect_setequal(statistics$answer, c("Moyenne", "Écart-type"))
  expect_equal(statistics$value[statistics$answer == "Moyenne"], 7.6)
  expect_true(all(response_rows(b17, "%")$unit == "%"))
})

test_that("cross-indicator joins preserve unequal units and missing countries", {
  d <- obscop_data()
  x <- d[d$is_country & d$question_code == "B1" & d$answer == "SI OUI", ]
  y <- d[d$is_country & d$question_code == "B17" & d$answer == "Moyenne", ]
  paired <- pair_indicators(x, y[y$group_label != "France", ])
  expect_equal(nrow(paired), 29)
  expect_false("France" %in% paired$group_label)
  expect_true(all(paired$x_unit == "%"))
  expect_true(all(paired$y_unit == "Nombre"))
  expect_equal(paired$y[paired$group_label == "Allemagne"], 6.7)
  expect_error(pair_indicators(rbind(x, x[1, ]), y), "une seule ligne")
})

test_that("CSV export roundtrips decimal values, missing samples and attribution", {
  d <- obscop_data()
  chosen <- d[d$is_world & ((d$question_code == "B17" & d$answer == "Moyenne") |
    (d$question_code == "V25Q4" & d$is_base_count)), ]
  csv <- tempfile(fileext = ".csv")
  on.exit(unlink(csv), add = TRUE)
  write_export(chosen, csv)
  exported <- as.data.frame(data.table::fread(csv, sep = ";", dec = ",", encoding = "UTF-8"))
  expect_equal(exported$Valeur, c(7.6, 3823))
  expect_equal(exported$Unité, c("Nombre", "Nombre"))
  expect_true(is.na(exported$Échantillon[2]))
  expect_equal(exported[["Échantillon source"]][2], 23503)
  expect_equal(exported[["Unité source"]][2], "%")
  expect_true(all(exported$Source == obscop_source()))
  expect_true(all(exported$Licence == "CC BY-NC-SA 4.0"))
  expect_equal(nrow(export_rows(chosen[FALSE, ])), 0)
})

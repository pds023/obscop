# Run from the project root: Rscript dev/prepare_data.R
# Rebuilds the compact, audited dataset without modifying raw-data.
if (.Platform$OS.type == "windows" && !l10n_info()[["UTF-8"]]) {
  invisible(Sys.setlocale("LC_CTYPE", ".UTF-8"))
}
source(file.path("R", "data_obscop.R"), encoding = "UTF-8")
input <- file.path("raw-data", "ObsCop2025.csv")
output <- file.path("inst", "extdata")
dir.create(output, recursive = TRUE, showWarnings = FALSE)
d <- read_obscop_csv(input)
saveRDS(d, file.path(output, "obscop2025.rds"), compress = "xz")
stopifnot(identical(d, readRDS(file.path(output, "obscop2025.rds"))))
write.csv(obscop_questions(d), file.path(output, "questions.csv"), row.names = FALSE, fileEncoding = "UTF-8")
indicators <- unique(d[c("question_id", "question_code", "question_label", "item", "answer", "unit", "base", "is_subtotal", "is_base_count")])
write.csv(indicators, file.path(output, "indicators.csv"), row.names = FALSE, fileEncoding = "UTF-8")
profiles <- unique(d[c("filter", "population", "is_world", "is_country", "is_region")])
write.csv(profiles, file.path(output, "profiles.csv"), row.names = FALSE, fileEncoding = "UTF-8")
metadata <- obscop_metadata(d)
writeLines(c(obscop_source(), "Licence : CC BY-NC-SA 4.0", "",
             paste("Lignes :", metadata$rows), paste("Pays :", metadata$countries),
             paste("Séries de questions :", metadata$question_series),
             paste("Échantillon Monde :", metadata$world_sample), "", metadata$notes),
           file.path(output, "data-quality.txt"), useBytes = TRUE)
cat(sprintf("Prepared %s records, %s countries, %s question series. RDS: %.2f MB\n",
            nrow(d), metadata$countries, metadata$question_series,
            file.info(file.path(output, "obscop2025.rds"))$size / 1024^2))

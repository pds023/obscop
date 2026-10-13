# Data are published aggregates, never individual respondents. Keep every
# published denominator and distinguish the source's repeated result series.
obscop_source <- function() {
  "Obs’COP – EDF / IPSOS – Septembre 2025"
}

obscop_country_codes <- function() {
  stats::setNames(
    c("AUS", "BEL", "BRA", "CAN", "CHL", "CHN", "COL", "CZE", "EGY", "FRA",
      "DEU", "IND", "IDN", "ITA", "JPN", "MEX", "MAR", "NGA", "NOR", "POL",
      "SAU", "SGP", "ZAF", "KOR", "ESP", "SWE", "TUR", "ARE", "GBR", "USA"),
    c("Australie", "Belgique", "Brésil", "Canada", "Chili", "Chine", "Colombie",
      "République Tchéque", "Égypte", "France", "Allemagne", "Inde", "Indonésie",
      "Italie", "Japon", "Mexique", "Maroc", "Nigeria", "Norvège", "Pologne",
      "Arabie Saoudite", "Singapour", "Afrique du Sud", "Corée du Sud", "Espagne",
      "Suède", "Turquie", "Émirats Arabes Unis", "Royaume-Uni", "États-Unis")
  )
}

read_obscop_csv <- function(path) {
  raw <- as.data.frame(data.table::fread(path, sep = ";", encoding = "UTF-8",
                                        colClasses = "character", na.strings = NULL))
  expected <- c("Année", "Périmètre spatial", "Spatial perimeter", "Filtre", "Filter",
                "Population", "Population_en", "Thématique", "Thematic", "Question",
                "Question_en", "Détails de la question", "Details of the question",
                "Réponse", "Answer", "Valeur", "Unité", "Unit", "Echantillon", "Base", "Base_en")
  if (!identical(names(raw), expected) || !nrow(raw)) {
    stop("Le fichier Obs’COP est vide ou son schéma ne correspond pas aux 21 colonnes attendues.")
  }
  names(raw) <- c("year", "spatial", "spatial_en", "filter", "filter_en", "population",
                  "population_en", "theme", "theme_en", "question", "question_en", "item",
                  "item_en", "answer", "answer_en", "value", "unit", "unit_en", "n", "base", "base_en")
  d <- raw
  d[] <- lapply(d, trimws)
  d$source_row <- seq_len(nrow(d))
  d$source_question <- d$question
  d$source_base <- d$base
  d$source_unit <- d$unit
  d$source_value <- d$value
  d$source_answer <- d$answer
  d$value <- suppressWarnings(as.numeric(sub(",", ".", d$value, fixed = TRUE)))
  d$year <- suppressWarnings(as.integer(d$year))
  d$n <- suppressWarnings(as.integer(d$n))
  d$source_n <- d$n
  if (anyNA(d$value) || anyNA(d$year) || any(d$value < 0) ||
      any(is.na(d$n) & nzchar(trimws(raw$n))) || any(d$n < 0, na.rm = TRUE) ||
      any(!d$unit %in% c("%", "Nombre"))) {
    stop("Valeurs numériques ou unités inattendues dans le fichier Obs’COP.")
  }
  d$theme_code <- sub("^PARTIE ([A-D]).*", "\\1", d$theme)
  labels <- c(A = "Préoccupations des populations", B = "Connaissance et perception",
              C = "Mobilisation des acteurs", D = "Climat au quotidien")
  d$theme_label <- unname(labels[d$theme_code])
  if (anyNA(d$theme_label)) stop("Thématique Obs’COP non reconnue.")
  d$is_world <- d$filter == "dans l'ensemble"
  d$is_region <- d$filter == "Pays" & startsWith(d$population, "ST ")
  d$is_country <- d$filter == "Pays" & !d$is_region
  d$group_label <- ifelse(d$is_world, "Monde", d$population)
  d$iso3 <- unname(obscop_country_codes()[d$population])
  if (any(is.na(d$iso3[d$is_country]))) stop("Un pays ne possède pas de code ISO3.")
  d$iso3[is.na(d$iso3)] <- ""
  d$is_subtotal <- startsWith(d$answer, "ST ") | d$answer == "SI OUI"
  d$is_base_count <- startsWith(d$answer, "Cible :")
  d$is_statistic <- d$answer %in% c("Moyenne", "Écart-type")
  d$quality_note <- ""
  d$series <- ""
  d$context <- ""

  # The English label resolves a duplicated French label in B8.
  production_methods <- startsWith(d$question, "B8.") &
    d$answer_en == "It is mainly through changes in the production methods companies use that climate change will be limited"
  d$answer[production_methods] <- "C’est principalement la modification des méthodes de production des entreprises qui permettra de limiter le changement climatique"
  d$quality_note[production_methods] <- "Libellé français corrigé à partir du libellé anglais : méthodes de production des entreprises. Le libellé source répétait à tort celui des modes de vie ; source_answer conservé."

  # Generic descriptive-statistic blocks have no question identifier in the CSV.
  # Their physical position identifies the preceding question, not a new estimate.
  starts <- which(c(TRUE, d$question[-1L] != d$question[-nrow(d)]))
  ends <- c(starts[-1L] - 1L, nrow(d))
  for (k in seq_along(starts)) {
    if (d$question[starts[k]] != "Nombre moyen de réponses") next
    if (k == 1L) stop("Statistiques descriptives sans question précédente.")
    rows <- starts[k]:ends[k]
    previous <- starts[k] - 1L
    d$context[rows] <- paste(d$question[previous], d$base[previous], sep = " · Base : ")
    d$question[rows] <- paste0("Nombre moyen de réponses — ", d$context[rows])
    d$quality_note[rows] <- "Le contexte est identifié par le bloc précédent dans le fichier source ; base et échantillon non renseignés."
  }

  # V25Q4 has a count labelled %, followed by two series with the same labels
  # and the same reported base. Do not silently merge or infer a denominator.
  q4 <- which(startsWith(d$question, "V25Q4."))
  if (length(q4)) {
    first_answer <- d$answer[q4[!d$is_base_count[q4]][1L]]
    boundaries <- q4[d$is_world[q4] & d$answer[q4] == first_answer]
    if (length(boundaries) != 2L) stop("La structure des deux séries V25Q4 a changé.")
    first <- q4[q4 < boundaries[2L]]
    second <- q4[q4 >= boundaries[2L]]
    d$series[first] <- "Série 1 — base ambiguë dans la source"
    d$series[second] <- "Série 2 — ensemble"
    d$quality_note[first] <- "La première série suit un effectif de cible, mais la source indique Tous et l'échantillon global. Dénominateur non confirmé ; échantillon masqué, source_n conservé."
    d$n[first] <- NA_integer_
  }
  d$unit[d$is_base_count] <- "Nombre"
  d$unit_en[d$is_base_count] <- "Number"
  d$quality_note[d$is_base_count] <- paste(d$quality_note[d$is_base_count],
    "Ligne d'effectif de cible publiée avec l'unité % : reclassée en Nombre, source_unit conservée.")
  if (any(d$value[d$unit == "%"] > 100)) stop("Pourcentage supérieur à 100 hors lignes d'effectif identifiées.")

  d$question_code <- sub("\\..*$", "", d$source_question)
  d$question_code[d$source_question == "Nombre moyen de réponses"] <- "STAT"
  # The base is part of the key: B6b, V25Q3 and PLANE2 have several populations.
  question_key <- paste(d$question, d$base, d$series, sep = "\r")
  d$question_id <- sprintf("q%03d", match(question_key, unique(question_key)))
  d$question_label <- d$question
  multi_base <- unique(d$question)[vapply(unique(d$question), function(q) {
    length(unique(d$base[d$question == q])) > 1L
  }, logical(1))]
  show_base <- d$question %in% multi_base
  base_label <- ifelse(nzchar(d$base), d$base, "non renseignée")
  d$question_label[show_base] <- paste0(d$question[show_base], " [Base : ", base_label[show_base], "]")
  show_series <- nzchar(d$series)
  d$question_label[show_series] <- paste0(d$question_label[show_series], " [", d$series[show_series], "]")
  d$multiple_choice <- d$question_code %in% c("A1BIS", "V25B3", "B6b", "B6dBis", "C3", "V25Q6", "V25Q7", "TRANSP1") |
    (d$question_code == "A3" & endsWith(d$source_question, "Total"))
  validate_obscop_data(d)
  attr(d, "obscop_metadata") <- list(
    source = obscop_source(), license = "CC BY-NC-SA 4.0",
    source_file = basename(path), source_md5 = unname(tools::md5sum(path)),
    raw_rows = nrow(raw), rows = nrow(d), years = sort(unique(d$year)),
    countries = length(unique(d$population[d$is_country])),
    regions = length(unique(d$population[d$is_region])), filters = length(unique(d$filter)),
    source_questions = length(unique(d$source_question)), question_series = length(unique(d$question_id)),
    world_sample = max(d$source_n[d$is_world], na.rm = TRUE),
    missing_sample_source = sum(is.na(d$source_n)),
    missing_sample_display = sum(is.na(d$n)), missing_base = sum(!nzchar(d$base)),
    reclassified_counts = sum(d$is_base_count), ambiguous_rows = sum(nzchar(d$series) & is.na(d$n)),
    corrected_answer_labels = sum(production_methods),
    source_exact_duplicates = sum(duplicated(raw)),
    notes = c(
      "Données agrégées publiées, sans microdonnées ni pondérations individuelles.",
      "Une seule vague : 2025. Aucune tendance historique ne peut être calculée.",
      "Les 30 pays et les 7 regroupements régionaux sont séparés ; les regroupements se recouvrent.",
      "Les profils démographiques et d'opinion portent sur le Monde ; aucun croisement pays × profil n'est disponible.",
      "Le Monde et les régions reprennent les résultats publiés, sans moyenne recalculée des pays.",
      "Les réponses multiples et les sous-totaux ST / SI OUI ne doivent pas être additionnés.",
      "Les bases peuvent varier selon les questions et les séries ; les arrondis peuvent empêcher une somme exacte de 100 %.",
      "V25Q4 : 181 effectifs de cible mal étiquetés % sont reclassés en Nombre. Deux séries sont conservées distinctement ; la base de la première est ambiguë.",
      "B8 : un libellé français répété à tort est corrigé à partir de l'anglais (méthodes de production des entreprises) ; le libellé source est conservé.",
      "Certains libellés anglais de population sont décalés dans la source ; les analyses utilisent les libellés français et les codes ISO des pays.",
      "Les échantillons absents ou ambigus restent manquants ; aucun intervalle de confiance n'est estimé sans plan de sondage."
    )
  )
  d
}

validate_obscop_data <- function(d) {
  required <- c("year", "question_id", "filter", "population", "item", "answer", "value", "unit", "n", "base")
  if (!is.data.frame(d) || !nrow(d) || !all(required %in% names(d))) {
    stop("Les données préparées Obs’COP sont invalides. Relancez dev/prepare_data.R.")
  }
  key <- c("year", "question_id", "filter", "population", "item", "answer", "unit")
  if (anyDuplicated(d[key])) stop("Plusieurs résultats partagent la même clé analytique ; aucune agrégation automatique n'est autorisée.")
  if (!is.numeric(d$value) || anyNA(d$value) || any(d$value < 0) ||
      any(d$value[d$unit == "%"] > 100)) stop("Valeurs préparées invalides.")
  invisible(TRUE)
}

obscop_data <- local({
  cached <- NULL
  cached_path <- NULL
  cached_time <- NULL
  function(path = NULL, refresh = FALSE) {
    if (is.null(path)) {
      candidates <- c(app_sys("extdata", "obscop2025.rds"),
                      file.path("inst", "extdata", "obscop2025.rds"),
                      file.path("raw-data", "ObsCop2025.csv"))
      candidates <- candidates[nzchar(candidates) & file.exists(candidates)]
      if (!length(candidates)) stop("Données Obs’COP introuvables. Exécutez Rscript dev/prepare_data.R depuis le projet.")
      path <- candidates[1L]
    }
    if (!file.exists(path)) stop("Fichier Obs’COP introuvable : ", path)
    path <- normalizePath(path, winslash = "/", mustWork = TRUE)
    stamp <- file.info(path)$mtime
    if (!refresh && identical(path, cached_path) && identical(stamp, cached_time)) return(cached)
    d <- if (endsWith(tolower(path), ".rds")) readRDS(path) else read_obscop_csv(path)
    validate_obscop_data(d)
    cached <<- d
    cached_path <<- path
    cached_time <<- stamp
    d
  }
})

obscop_questions <- function(data = obscop_data()) {
  cols <- c("question_id", "question_code", "question", "question_label", "question_en",
            "theme_code", "theme_label", "base", "series", "context", "multiple_choice")
  out <- unique(data[cols])
  out <- out[!duplicated(out$question_id), , drop = FALSE]
  rownames(out) <- NULL
  out
}

obscop_countries <- function(data = obscop_data()) {
  out <- unique(data[data$is_country, c("population", "population_en", "iso3"), drop = FALSE])
  # Some later English source labels are shifted; the opening country table is consistent.
  out <- out[!duplicated(out$population), , drop = FALSE]
  out <- out[order(out$population), , drop = FALSE]
  rownames(out) <- NULL
  out
}

obscop_metadata <- function(data = obscop_data()) {
  attr(data, "obscop_metadata")
}

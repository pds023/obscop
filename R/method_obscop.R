# The methodology travels with the prepared dataset and its audit metadata.
obscop_method <- function(data) {
  metadata <- obscop_metadata(data)
  tags <- shiny::tags
  number <- function(x) format(x, big.mark = " ", scientific = FALSE, trim = TRUE)

  shiny::tagList(
    tags$h3("Une photographie des opinions en 2025"),
    tags$p(
      "Cette application explore les résultats agrégés publiés de l’enquête Obs’COP 2025. ",
      "Une ligne correspond à un résultat pour une question, un sujet, une réponse et une population ; ",
      tags$strong("elle ne correspond pas à une personne interrogée"), ". ",
      "Les réponses individuelles et les pondérations ne sont pas présentes dans ces fichiers."
    ),
    tags$table(
      class = "method-facts",
      tags$caption("Périmètre du fichier chargé"),
      tags$tbody(
        tags$tr(tags$th(scope = "row", "Édition"), tags$td(paste(metadata$years, collapse = ", "))),
        tags$tr(tags$th(scope = "row", "Résultats agrégés"), tags$td(number(metadata$rows))),
        tags$tr(tags$th(scope = "row", "Pays distincts"), tags$td(number(metadata$countries))),
        tags$tr(tags$th(scope = "row", "Regroupements régionaux"), tags$td(number(metadata$regions))),
        tags$tr(tags$th(scope = "row", "Séries analytiques de questions"), tags$td(number(metadata$question_series))),
        tags$tr(tags$th(scope = "row", "Échantillon Monde maximal publié"), tags$td(paste(number(metadata$world_sample), "personnes")))
      )
    ),
    tags$p("L’échantillon ci-dessus décrit la base mondiale la plus large du fichier. Certaines questions portent sur une population plus restreinte. Les séries analytiques distinguent notamment les bases différentes d’une même question et les séries répétées dans la source."),
    tags$p("Le fichier ne contient qu’une vague, 2025. Il permet des comparaisons entre pays et profils, mais aucune évolution temporelle ne peut en être déduite."),

    tags$h3("Quatre thématiques à parcourir"),
    tags$ul(
      tags$li(tags$strong("A · Préoccupations des populations : "), "les sujets qui préoccupent et la place de l’environnement."),
      tags$li(tags$strong("B · Connaissance et perception : "), "la compréhension du réchauffement climatique et les opinions à son sujet."),
      tags$li(tags$strong("C · Mobilisation des acteurs : "), "les responsabilités, les actions et les attentes envers les acteurs."),
      tags$li(tags$strong("D · Climat au quotidien : "), "les comportements, les pratiques et les leviers de changement.")
    ),

    tags$h3("Pays, régions et profils : des périmètres distincts"),
    tags$ul(
      tags$li(tags$strong("Les comparaisons géographiques "), "portent sur les pays choisis dans la barre latérale. Retirer tous les pays produit une sélection vide ; cela ne rétablit pas tous les pays."),
      tags$li(tags$strong("Le Monde et les régions "), "reprennent les résultats publiés par l’étude. L’application ne les reconstruit pas par une moyenne simple des pays. Les régions se recouvrent et ne doivent pas être additionnées. Ces agrégats sont consultables dans Explorer."),
      tags$li(tags$strong("Les profils démographiques et d’opinion "), "sont publiés à l’échelle du Monde. Le fichier ne contient aucun croisement pays × profil : le choix de pays ne filtre donc pas l’onglet Profils.")
    ),

    tags$h3("Pourcentages, nombres et bases de répondants"),
    tags$ul(
      tags$li(tags$strong("L’unité % "), "représente une proportion publiée pour la base indiquée. Les écarts entre deux pourcentages se lisent en points de pourcentage, pas en variation relative."),
      tags$li(tags$strong("L’unité Nombre "), "peut désigner un effectif, une moyenne de réponses ou un écart-type. Elle ne doit pas être interprétée comme un pourcentage."),
      tags$li(tags$strong("La base et l’échantillon "), "précisent la population concernée et son effectif lorsqu’ils sont renseignés. Une base absente ou ambiguë reste non disponible. Un effectif de base ne compte pas les personnes ayant choisi chaque réponse."),
      tags$li(tags$strong("Les réponses multiples "), "peuvent produire un total supérieur à 100 %. Les sous-totaux « ST » et « SI OUI » regroupent des réponses déjà présentes : il ne faut pas les additionner à leurs composantes. Les arrondis peuvent aussi empêcher une somme exacte de 100 %.")
    ),
    tags$p("La comparaison doit porter sur des formulations, des unités et des bases compatibles. Les libellés de base et de série sont conservés dans les sélecteurs lorsqu’ils sont nécessaires pour distinguer des résultats."),

    tags$h3("Des relations descriptives, sans inférence individuelle"),
    tags$p("Dans Relations, chaque point représente un pays pour lequel les deux indicateurs sont disponibles. La corrélation résume leur association à cette échelle. Elle ne démontre pas une causalité, ni que les personnes exprimant une opinion sont celles qui en expriment une autre."),
    tags$p("L’application ne calcule pas d’intervalles de confiance ou de tests de significativité : les pondérations et le plan de sondage nécessaires ne sont pas fournis. Les différences affichées sont descriptives ; un petit écart ne permet pas à lui seul de conclure à une différence dans la population."),

    tags$h3("Contrôles et corrections documentés"),
    tags$ul(
      tags$li(
        tags$strong("V25Q4 · Unités et séries. "),
        paste0(number(metadata$reclassified_counts), " lignes d’effectif de cible étaient étiquetées « % » ; elles sont reclassées en « Nombre ». "),
        "Les deux séries portant des libellés identiques sont conservées distinctement. La base de la première série est ambiguë : son échantillon est masqué au lieu d’être déduit, et l’information source reste conservée."
      ),
      tags$li(
        tags$strong("B8 · Libellé de réponse. "),
        "Le libellé français relatif aux méthodes de production des entreprises répétait celui des modes de vie. Il est corrigé d’après la version anglaise, sur ",
        number(metadata$corrected_answer_labels), " lignes. Le libellé original est conservé."
      ),
      tags$li(
        tags$strong("Nombre moyen de réponses · Contexte. "),
        "Ces blocs descriptifs n’ont pas d’identifiant de question propre dans le fichier. Ils sont rattachés au bloc qui les précède dans l’ordre source. Ce rattachement est signalé et ne crée ni base ni échantillon supplémentaires."
      ),
      tags$li(tags$strong("Libellés anglais de population. "), "Certains libellés anglais sont décalés dans le fichier source. Les analyses s’appuient sur les libellés français et sur les codes ISO des pays pour éviter ces associations erronées."),
      tags$li(tags$strong("Valeurs absentes et doublons. "), "Les échantillons absents restent manquants ; aucune valeur n’est imputée. La préparation vérifie qu’une clé analytique ne correspond qu’à un résultat et refuse les doublons non résolus, sans addition automatique.")
    ),
    tags$details(
      tags$summary("Retrouver la trace d’un résultat dans les exports"),
      tags$p("Les tableaux d’export gardent les informations nécessaires pour revenir au fichier d’origine :"),
      tags$ul(
        tags$li(tags$code("Ligne source"), " : numéro de la ligne de données source, hors en-tête ;"),
        tags$li(tags$code("Question source"), ", ", tags$code("Réponse source"), ", ", tags$code("Valeur source"), ", ", tags$code("Unité source"), ", ", tags$code("Base source"), ", ", tags$code("Échantillon source"), " : valeurs publiées avant correction ou masquage ;"),
        tags$li(tags$code("Note qualité"), " : correction, ambiguïté ou rattachement contextuel associé à la ligne ;"),
        tags$li(tags$code("Identifiant question"), " et ", tags$code("Série"), " : identifiant analytique et éventuelle série distinguée.")
      ),
      tags$p("Fichier source : ", tags$code(metadata$source_file), ". Empreinte MD5 : ", tags$code(metadata$source_md5), ".")
    ),
    tags$h3("Fond cartographique"),
    tags$p(
      "Le fond de carte provient de ",
      tags$a("Natural Earth — Admin 0 Countries, 1:110m", href = "https://www.naturalearthdata.com/downloads/110m-cultural-vectors/110m-admin-0-countries/", target = "_blank", rel = "noopener noreferrer"),
      ", dont les données géographiques sont dans le ",
      tags$a("domaine public", href = "https://www.naturalearthdata.com/about/terms-of-use/", target = "_blank", rel = "noopener noreferrer"),
      ". La géométrie est intégrée à l’application pour permettre son affichage sans service cartographique distant."
    )
  )
}

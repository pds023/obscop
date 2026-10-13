#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  panel <- function(title, subtitle = NULL, ..., class = NULL) {
    tags$section(
      class = paste("visual-panel", class),
      tags$div(
        class = "panel-heading",
        tags$h2(title),
        if (!is.null(subtitle)) tags$p(subtitle)
      ),
      ...
    )
  }

  bslib::page_sidebar(
    golem_add_external_resources(),
    lang = "fr",
    window_title = "Obs’COP 2025 · Les opinions sur le climat",
    fillable = FALSE,
    theme = bslib::bs_theme(
      version = 5,
      bg = "#f5f6f2",
      fg = "#203d38",
      primary = "#177d70",
      secondary = "#61736e",
      base_font = "system-ui",
      border_radius = "0.65rem"
    ),
    sidebar = bslib::sidebar(
      width = 310,
      open = list(desktop = "open", mobile = "closed"),
      resizable = FALSE,
      class = "filter-sidebar",
      title = tags$div(
        class = "sidebar-brand",
        tags$img(class = "brand-logo", src = "www/logo-obscop.png", alt = ""),
        tags$span("Obs’COP", tags$small("CLIMAT & OPINIONS"))
      ),
      tags$div(
        class = "sidebar-intro",
        tags$h2("Votre sélection"),
        tags$p("Choisissez une question. Explorez ses nuances à travers le monde.")
      ),
      selectInput("year", "Édition", choices = NULL),
      selectInput("theme", "Thématique", choices = NULL),
      selectizeInput(
        "question", "Question", choices = NULL,
        options = list(placeholder = "Rechercher une question…")
      ),
      selectInput("detail", "Sujet / détail", choices = NULL),
      selectInput("answer", "Réponse observée", choices = NULL),
      tags$div(class = "filter-divider"),
      selectizeInput(
        "countries", "Pays comparés", choices = NULL, multiple = TRUE,
        options = list(
          placeholder = "Ajouter des pays…",
          plugins = list("remove_button")
        )
      ),
      tags$div(class = "country-actions", actionLink("select_all_countries", "Tout sélectionner"), actionLink("clear_countries", "Tout retirer")),
      tags$p(class = "input-help", "Retirez ou ajoutez des pays pour limiter les vues internationales. Les agrégats régionaux sont accessibles dans Explorer."),
      actionButton("reset", "Réinitialiser", class = "btn-reset"),
      uiOutput("selection_note"),
      tags$div(
        class = "sidebar-footnote",
        tags$span(class = "status-dot"),
        "Une exploration des résultats publiés"
      )
    ),
    tags$main(
      class = "app-main",
      tags$header(
        class = "app-hero",
        tags$div(
          class = "hero-copy",
          tags$div(class = "eyebrow", "L’OBSERVATOIRE INTERNATIONAL", tags$span(class = "edition-badge", "ÉDITION 2025")),
          tags$h1("Le climat,", tags$br(), tags$span("vu du monde.")),
          tags$p(class = "hero-description", "Comprendre les perceptions. Comparer les opinions. Explorer les leviers d’action."),
          uiOutput("context")
        ),
        tags$div(
          class = "hero-orbit", `aria-hidden` = "true",
          tags$div(class = "orbit-ring orbit-one"),
          tags$div(class = "orbit-ring orbit-two"),
          tags$div(class = "orbit-ring orbit-three"),
          tags$div(class = "orbit-equator"),
          tags$span(class = "orbit-dot dot-one"),
          tags$span(class = "orbit-dot dot-two"),
          tags$span(class = "orbit-dot dot-three")
        )
      ),
      uiOutput("metrics"),
      tags$div(
        class = "workspace-heading",
        tags$div(class = "indicator-heading", tags$span(class = "eyebrow", "L’INDICATEUR À LA LOUPE"), uiOutput("indicator_title")),
        downloadButton("download_selection", "Exporter la sélection", class = "btn-export")
      ),
      tags$div(
        class = "exploration-tabs",
        bslib::navset_pill(
          id = "view", selected = "overview",
          bslib::nav_panel(
            "Vue d’ensemble", value = "overview",
            tags$div(
              class = "overview-grid",
              panel(
                "Une lecture mondiale",
                "Survolez un pays pour consulter sa valeur et sa base de répondants.",
                plotly::plotlyOutput("world_map", height = "420px"),
                uiOutput("overview_insight"),
                class = "map-panel"
              ),
              panel(
                "Les pays en perspective",
            "Trait doré : référence de l’ensemble international.",
                plotly::plotlyOutput("ranking", height = "580px")
              )
            )
          ),
          bslib::nav_panel(
            "Explorer", value = "explore",
            panel(
              "Au-delà d’une seule réponse",
              "Décomposez les réponses à une question ou comparez ses différents sujets.",
              tags$div(
                class = "control-row",
                selectInput("explore_country", "Pays ou ensemble", choices = NULL),
                radioButtons("explore_mode", "Angle de lecture", choices = c("Réponses" = "answers", "Détails" = "details"), inline = TRUE),
                checkboxInput("include_totals", "Afficher aussi les sous-totaux", FALSE)
              ),
              plotly::plotlyOutput("distribution", height = "620px"),
              uiOutput("distribution_note"),
              tags$p(class = "chart-footnote", "Les réponses multiples et les sous-totaux ne s’additionnent pas. Le réglage des sous-totaux s’applique aussi aux comparaisons de réponses.")
            )
          ),
          bslib::nav_panel(
            "Comparer", value = "compare",
            panel(
              "Deux regards sur le climat",
              "Repérez les écarts entre deux pays, sur les sujets ou les réponses de la question choisie.",
              tags$div(
                class = "control-row compare-controls",
                selectInput("compare_a", "Premier pays", choices = NULL),
                tags$span(class = "compare-versus", `aria-hidden` = "true", "↔"),
                selectInput("compare_b", "Second pays", choices = NULL)
              ),
              plotly::plotlyOutput("gap_plot", height = "620px")
            ),
            panel(
              "Une matrice pour comparer",
              "Comparez les réponses à un même sujet, ou les sujets pour la réponse observée. Survolez les cellules pour lire les valeurs.",
              radioButtons("heatmap_axis", "Colonnes de la matrice", choices = c("Réponses" = "answers", "Sujets / détails" = "details"), inline = TRUE),
              plotly::plotlyOutput("heatmap", height = "650px")
            )
          ),
          bslib::nav_panel(
            "Profils", value = "profiles",
            panel(
              "Ce qui change selon les profils",
              "Explorez les résultats par catégorie sociodémographique, à l’échelle de l’ensemble international.",
              tags$div(
                class = "control-row",
                selectInput("profile_filter", "Dimension à comparer", choices = NULL),
                tags$span(class = "scope-badge", "Ensemble international")
              ),
              tags$p(class = "scope-note", "Ces ventilations sont publiées pour l’ensemble international. Elles ne décrivent pas les profils à l’intérieur de chaque pays ; la sélection géographique ne s’applique pas ici."),
              plotly::plotlyOutput("profile_plot", height = "600px"),
              uiOutput("profile_note")
            )
          ),
          bslib::nav_panel(
            "Relations", value = "relations",
            panel(
              "Quelles opinions sont associées ?",
              "L’indicateur sélectionné à gauche est placé sur l’axe horizontal. Choisissez un second indicateur pour l’axe vertical.",
              tags$div(
                class = "control-row relationship-controls",
                selectizeInput("y_question", "Seconde question", choices = NULL, options = list(placeholder = "Rechercher une question…")),
                selectInput("y_detail", "Sujet / détail", choices = NULL),
                selectInput("y_answer", "Réponse observée", choices = NULL)
              ),
              checkboxInput("show_labels", "Afficher le nom des pays", TRUE),
              plotly::plotlyOutput("scatter", height = "600px"),
              uiOutput("correlation_note"),
              tags$p(class = "chart-footnote", "Chaque point représente un pays. Une corrélation entre pays ne démontre ni une causalité ni une relation entre les réponses individuelles.")
            )
          ),
          bslib::nav_panel(
            "Données", value = "data",
            panel(
              "Les données, en toute transparence",
              "Recherchez, triez et exportez les résultats et les informations de base disponibles.",
              tags$div(
                class = "control-row data-controls",
                radioButtons("data_scope", "Périmètre du tableau", choices = c("Sélection courante" = "selection", "Toute la base" = "all"), inline = TRUE),
                downloadButton("download_data", "Exporter le tableau (CSV)", class = "btn-export")
              ),
              uiOutput("data_note"),
              DT::DTOutput("data_table")
            )
          ),
          bslib::nav_panel(
            "Méthode", value = "method",
            panel(
              "Bien lire les résultats",
              "Le contexte, les unités et les précautions qui donnent du sens aux chiffres.",
              uiOutput("method_content"),
              tags$div(
                class = "source-box",
                tags$h3("Source et conditions de réutilisation"),
                tags$p("Source à citer : « Obs’COP – EDF / IPSOS – Septembre 2025 ».", class = "source-citation"),
                tags$p("Les résultats sont mis à disposition sous licence Creative Commons Attribution – Pas d’Utilisation Commerciale – Partage dans les Mêmes Conditions 4.0 International. La reproduction des marques et logos est interdite."),
                tags$div(
                  class = "source-links",
                  tags$a("Documentation de l’étude ↗", href = "https://www.edf.fr/groupe-edf/observatoire-international-climat-et-opinions-publiques/documentation", target = "_blank", rel = "noopener noreferrer"),
                  tags$a("Licence CC BY-NC-SA 4.0 ↗", href = "https://creativecommons.org/licenses/by-nc-sa/4.0/deed.fr", target = "_blank", rel = "noopener noreferrer")
                )
              )
            )
          )
        )
      ),
      tags$footer(
        class = "app-footer",
        tags$span("Obs’COP – EDF / IPSOS – Septembre 2025"),
        tags$span("Données ouvertes · CC BY-NC-SA 4.0")
      )
    )
  )
}

#' Add external resources to the application
#' @import shiny
#' @noRd
golem_add_external_resources <- function() {
  golem::add_resource_path("www", app_sys("app/www"))
  tags$head(
    tags$meta(name = "description", content = "Explorez les opinions internationales sur le changement climatique avec les résultats Obs’COP 2025."),
    tags$link(rel = "stylesheet", type = "text/css", href = "www/app.css"),
    tags$link(rel = "icon", type = "image/png", href = "www/logo-obscop.png")
  )
}

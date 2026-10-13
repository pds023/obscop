# Obs’COP 2025 — explorer les opinions sur le climat

Application **Shiny / Golem** en français pour explorer les résultats agrégés de l’Observatoire international climat et opinions publiques 2025. L’interface associe une navigation sobre, des filtres communs et des graphiques interactifs.

Les données fournies contiennent **146 275 résultats agrégés**, **30 pays**, **7 regroupements régionaux** et **52 séries analytiques de questions**. Elles couvrent les préoccupations, la perception du réchauffement climatique, la mobilisation des acteurs et les comportements quotidiens.

## Démarrage

Ouvrir le projet dans RStudio, ou placer le répertoire de travail R à la racine de ce dépôt. Utiliser une version récente de R et installer les dépendances une première fois :

```r
install.packages(c(
  "shiny", "golem", "config", "bslib", "plotly", "DT",
  "data.table", "htmltools", "jsonlite", "pkgload", "testthat"
))
```

Lancer directement l’application grâce au fichier `app.R` :

```r
shiny::runApp()
```

On peut aussi charger le package local et utiliser son point d’entrée Golem :

```r
pkgload::load_all(".")
run_app()
```

Pour fixer l’adresse et le port :

```r
pkgload::load_all(".")
run_app(options = list(host = "127.0.0.1", port = 3838, launch.browser = TRUE))
```

Le fichier préparé `inst/extdata/obscop2025.rds` est inclus. Le lancement ne nécessite donc pas de reparcourir le CSV volumineux. Les données sont chargées une fois puis mises en cache dans le processus R. Les feuilles de style, la typographie système et le fond cartographique sont locaux ; l’interface n’utilise pas de service de carte distant.

### Installer comme package

Depuis la racine du projet, après installation des dépendances :

```sh
R CMD INSTALL .
```

Puis dans R :

```r
obscop::run_app()
```

## Publication sur Posit Connect Cloud et data.gouv.fr

Le `manifest.json` à la racine est le manifeste de déploiement **rsconnect** (ce n’est pas un manifeste de PWA). Il a été généré avec **R 4.5.1**, une version prise en charge par Posit Connect Cloud. Il référence les dépendances et les fichiers nécessaires, dont les données préparées, le fond cartographique et le logo. Le CSV brut et les outils locaux ne sont pas embarqués.

1. Placer le projet dans un dépôt GitHub accessible à Connect Cloud, avec `app.R`, `DESCRIPTION`, `NAMESPACE`, `LICENSE`, `.Rbuildignore`, les dossiers `R/` et `inst/`, ainsi que `manifest.json`.
2. Dans **Posit Connect Cloud**, créer un contenu Shiny à partir de ce dépôt et sélectionner `app.R` à la racine comme fichier principal.
3. Déployer, vérifier l’application, puis activer son accès public. Utiliser son URL HTTPS dans la fiche de réutilisation data.gouv.fr.
4. Copier le texte de `publication/description-datagouv.txt`, choisir `inst/app/www/logo-obscop.png` comme illustration et associer le jeu de données Obs’COP 2025 à la réutilisation.

Connect Cloud installe les dépendances à partir du manifeste avant de lancer l’application : **ne pas ajouter `renv::restore()` dans `app.R`**. Si un `renv.lock` et un manifeste coexistent, Connect Cloud donne priorité au manifeste. **Posit Cloud**, l’environnement RStudio hébergé, est un autre produit ; ces instructions concernent **Posit Connect Cloud**.

Après toute modification de fichier embarqué ou de dépendance, régénérer le manifeste depuis la racine, avec l’environnement R choisi pour le déploiement :

```r
install.packages("rsconnect") # une fois, dans l’environnement de préparation
source("dev/write_manifest.R")
```

Le lanceur charge le package local avec `pkgload`, sans charger les tests. Il appelle ensuite le point d’entrée dans le namespace du package. Les appels à `golem::with_golem_options()` et `shiny::shinyApp()` sont explicites, pour éviter une résolution des imports incorrecte quand Shiny source aussi le dossier `R/`.

Documentation : [dépendances R](https://docs.posit.co/connect-cloud/how-to/r/dependencies.html), [déploiement Shiny](https://docs.posit.co/connect-cloud/how-to/r/shiny-r.html), [versions de R disponibles](https://docs.posit.co/connect-cloud/user/platform/r.html).

Le logo est une création graphique originale générée avec l’outil intégré ImageGen ; il ne reprend pas les logos EDF ou Ipsos. Les prompts sont conservés dans `publication/logo-prompt.txt`. L’application est une réutilisation indépendante des données et ne constitue pas une application officielle de ces organismes.

## Parcours d’exploration

| Vue | Usage |
| --- | --- |
| **Vue d’ensemble** | Carte mondiale, classement des pays, indicateurs synthétiques et commentaire contextualisé. |
| **Explorer** | Distribution des réponses ou comparaison des sujets d’une question, pour un pays, une région ou le Monde. Les sous-totaux sont optionnels. |
| **Comparer** | Comparaison de deux pays et matrice des pays sélectionnés, avec les modes **Réponses** ou **Sujets / détails**. |
| **Profils** | Comparaison des catégories démographiques et d’opinion pour l’ensemble international. |
| **Relations** | Nuage de points entre deux indicateurs, identification des pays et association descriptive. |
| **Données** | Tableau filtrable et triable, consultation de la sélection ou de toute la base, export CSV. |
| **Méthode** | Périmètres, unités, bases, limites statistiques et corrections du fichier source. |

La barre latérale permet de choisir l’édition, la thématique, la question, le sujet, la réponse et les pays comparés. Les choix dépendants sont actualisés automatiquement. Une liste de pays vide produit une sélection géographique vide ; **Réinitialiser** restaure la sélection de départ. Les régions restent accessibles séparément dans **Explorer**.

Les graphiques proposent des infobulles et un export d’image via la barre Plotly. Les exports CSV servent à conserver les données utilisées dans l’analyse ; les informations de provenance et de qualité permettent de revenir aux résultats originaux.

## Données et préparation

Le CSV d’origine `raw-data/ObsCop2025.csv` reste inchangé. Pour reconstruire le jeu préparé et ses inventaires, depuis la racine du projet :

```sh
Rscript dev/prepare_data.R
```

Cette commande produit :

| Fichier | Contenu |
| --- | --- |
| `inst/extdata/obscop2025.rds` | Résultats préparés et métadonnées d’audit. |
| `inst/extdata/questions.csv` | Catalogue des questions et des séries analytiques. |
| `inst/extdata/indicators.csv` | Inventaire des indicateurs disponibles. |
| `inst/extdata/profiles.csv` | Populations et périmètres disponibles. |
| `inst/extdata/data-quality.txt` | Résumé du périmètre et des précautions de lecture. |

Les contrôles de préparation vérifient le schéma source, les valeurs numériques, les unités, les codes pays et l’unicité des clés analytiques. Le chargeur utilise le RDS fourni, avec repli sur le CSV local s’il est absent. Après modification du CSV, relancer la préparation puis redémarrer l’application pour utiliser le nouveau fichier préparé.

### Garde-fous de lecture

- **Agrégats uniquement.** Une ligne n’est pas une personne interrogée. Les microdonnées, les poids individuels et le plan de sondage ne figurent pas dans le fichier.
- **Une seule vague.** Les résultats portent sur 2025 ; l’application ne présente pas de tendance historique.
- **Bases conservées.** Deux résultats d’une même question portant sur des bases différentes sont conservés séparément. Les échantillons absents ou ambigus restent manquants.
- **Monde publié.** Les résultats du Monde et des régions sont ceux de l’étude ; ils ne sont pas recalculés par moyenne des pourcentages nationaux. Les régions se recouvrent et ne s’additionnent pas.
- **Profils internationaux.** Aucun croisement pays × profil n’est disponible. Le filtre de pays ne s’applique pas à la vue Profils.
- **Unités explicites.** Pourcentages, effectifs et nombres moyens de réponses restent distincts. L’écart entre deux pourcentages se lit en points de pourcentage.
- **Réponses multiples et sous-totaux.** Ils ne s’additionnent pas comme des catégories exclusives. Un total peut dépasser 100 % ; les arrondis peuvent aussi expliquer un écart à 100 %.
- **Relations descriptives.** Une corrélation entre pays n’est ni une causalité ni une relation établie entre individus. Aucun intervalle de confiance ou test de significativité n’est estimé sans le plan de sondage.

### Corrections traçables

**V25Q4 :** 181 lignes d’effectif de cible, incorrectement étiquetées en pourcentage, sont reclassées en `Nombre`. Les deux séries de résultats sont distinguées. La première présente une base ambiguë : son échantillon affiché est masqué, et la valeur source est conservée.

**B8 :** le libellé français de la réponse sur les méthodes de production des entreprises, qui répétait à tort celui des modes de vie, est corrigé à partir de l’anglais. Le libellé original reste disponible.

**Nombre moyen de réponses :** les blocs descriptifs sans identifiant propre reçoivent le contexte du bloc qui les précède dans le CSV. Ce rattachement est signalé et ne crée pas d’échantillon supplémentaire.

**Libellés anglais de population :** certains libellés anglais sont décalés dans la source. Les analyses utilisent les libellés français et les codes ISO des pays pour éviter les associations erronées.

Dans les exports CSV, les colonnes `Ligne source`, `Question source`, `Réponse source`, `Valeur source`, `Unité source`, `Base source` et `Échantillon source` conservent la provenance. `Note qualité` décrit les corrections et ambiguïtés ; `Identifiant question` et `Série` distinguent les résultats analytiques. Les métadonnées incluent aussi l’empreinte MD5 du CSV d’origine.

## Structure du projet

```text
app.R               Lancement direct avec shiny::runApp()
R/
  app_ui.R          Interface et navigation
  app_server.R      Filtres réactifs, sorties et téléchargements
  charts_obscop.R   Graphiques interactifs et carte
  data_obscop.R     Importation, contrôles, préparation et cache
  method_obscop.R   Contenu de la vue Méthode
  run_app.R         Point d’entrée Golem
inst/
  app/www/          Styles et ressources cartographiques locales
  extdata/          Données préparées et inventaires
raw-data/           Fichier CSV d’origine
dev/prepare_data.R  Reconstruction des données préparées
tests/testthat/     Tests des données et des comportements analytiques
```

Pour exécuter les tests locaux :

```r
pkgload::load_all(".")
testthat::test_dir("tests/testthat")
```

## Source, attribution et licences

**Source à citer : « Obs’COP – EDF / IPSOS – Septembre 2025 ».**

Les données et les résultats de l’étude sont mis à disposition sous [licence Creative Commons Attribution – Pas d’Utilisation Commerciale – Partage dans les Mêmes Conditions 4.0 International](https://creativecommons.org/licenses/by-nc-sa/4.0/deed.fr). Ces conditions s’appliquent à leur réutilisation, notamment aux tableaux et graphiques produits à partir de l’étude. La reproduction des marques et logos EDF / Ipsos est interdite ; l’application n’utilise pas ces logos.

La [documentation officielle de l’étude Obs’COP](https://www.edf.fr/groupe-edf/observatoire-international-climat-et-opinions-publiques/documentation) donne accès aux documents de l’enquête. Les microdonnées éventuellement proposées par cette source ne sont pas intégrées à cette application.

Le fond cartographique est issu de [Natural Earth — Admin 0 Countries, 1:110m](https://www.naturalearthdata.com/downloads/110m-cultural-vectors/110m-admin-0-countries/). Ses données géographiques sont dans le [domaine public](https://www.naturalearthdata.com/about/terms-of-use/) et sont intégrées localement à l’application.

La licence du **code de l’application** est indiquée dans `DESCRIPTION` et, le cas échéant, le fichier `LICENSE`. Elle ne remplace pas la licence propre aux données. Les dépendances R conservent leurs licences respectives.

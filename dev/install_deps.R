# Install only missing packages; no automatic changes to existing versions.
dependencies <- c("shiny", "golem", "bslib", "plotly", "DT", "data.table",
                  "jsonlite", "htmltools", "config", "pkgload", "testthat")
missing <- dependencies[!vapply(dependencies, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) install.packages(missing, repos = "https://cloud.r-project.org")
message("Dépendances disponibles. Lancez shiny::runApp() depuis le projet.")

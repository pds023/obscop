# Open this project in RStudio and click Run App, or run shiny::runApp().
if (!requireNamespace("pkgload", quietly = TRUE)) {
  stop("Installez les dépendances avec source('dev/install_deps.R').")
}
pkgload::load_all(
  ".", quiet = TRUE, export_all = FALSE,
  helpers = FALSE, attach_testthat = FALSE
)
# Shiny also sources R/; use the package function with its imported dependencies.
getExportedValue("obscop", "run_app")()

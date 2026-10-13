# Run from the project root with the R version selected for Connect Cloud.
# This is a preparation step; the deployed app never installs dependencies.
if (!requireNamespace("rsconnect", quietly = TRUE)) {
  stop("Installez rsconnect dans cet environnement R avant de générer le manifeste.")
}
if (!file.exists("app.R") || !file.exists("DESCRIPTION")) {
  stop("Exécutez ce script depuis la racine du projet obscop.")
}
options(repos = c(CRAN = "https://cloud.r-project.org"))
files <- c("app.R", "DESCRIPTION", "NAMESPACE", "LICENSE", ".Rbuildignore",
           list.files("R", pattern = "\\.R$", full.names = TRUE),
           list.files("inst", recursive = TRUE, full.names = TRUE))
rsconnect::writeManifest(
  appDir = ".", appFiles = files, appPrimaryDoc = "app.R", appMode = "shiny",
  quarto = FALSE, envManagementR = TRUE
)
manifest <- jsonlite::read_json("manifest.json", simplifyVector = FALSE)
stopifnot(identical(manifest$metadata$appmode, "shiny"),
          "pkgload" %in% names(manifest$packages),
          "golem" %in% names(manifest$packages),
          !"obscop" %in% names(manifest$packages),
          all(files %in% names(manifest$files)),
          all(unname(tools::md5sum(names(manifest$files))) ==
                vapply(manifest$files, `[[`, character(1), "checksum")))
message("Manifeste créé avec R ", getRversion(), " : ", length(manifest$packages),
        " packages, ", length(manifest$files), " fichiers.")

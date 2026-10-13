# Run from the project root. This preserves the user's R session.
options(golem.app.prod = FALSE)
pkgload::load_all(".", quiet = TRUE)
obscop::run_app()

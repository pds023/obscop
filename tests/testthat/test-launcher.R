test_that("the launcher resolves its dependencies outside the package namespace", {
  isolated <- new.env(parent = baseenv())
  isolated$app_ui <- app_ui
  isolated$app_server <- app_server
  launcher <- run_app
  environment(launcher) <- isolated

  app <- launcher(options = list(port = 3838), deployment = "cloud")

  expect_s3_class(app, "shiny.appobj")
  expect_equal(app$options$port, 3838)
  expect_true(is.function(app$serverFuncSource()))
})

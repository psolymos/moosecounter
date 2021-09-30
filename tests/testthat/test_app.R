library(shinytest)

test_that("shiny app tests pass", {
  skip_on_cran()

  # Use compareImages=FALSE if testing on a different machine (current images from UBUNTU)
  appdir <- system.file(package = "moosecounter", "shiny")
  expect_pass(testApp(appdir, compareImages = TRUE))
})

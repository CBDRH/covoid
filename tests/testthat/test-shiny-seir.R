# # This file is for testing the applications in the inst/ directory.
#
# test_that("seir shiny app works", {
#     # Don't run these tests on the CRAN build servers
#     skip_on_cran()
#
#     # Use compareImages=FALSE because the expected image screenshots were created
#     # on a Mac, and they will differ from screenshots taken on the CI platform,
#     # which runs on Linux.
#     appdir <- system.file(package = "covoid", "shiny-interfaces/seir")
#     shinytest::expect_pass(shinytest::testApp(appdir, compareImages = FALSE))
# })

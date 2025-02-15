test_that("Config has the right structure", {
  config <- Config(test_path("testdata"))
  expect_s3_class(config, "becophd.Config")
  expect_named(config, c("basedir", "pptprefix", "indexfile", "tests"))
  expect_named(config$tests)
  expect_type(config$tests[[1]], "character")
})

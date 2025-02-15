config <- Config(
  test_path("testdata"),
  pptprefix = "PPT_",
  tests = list(foo = c("metric"))
)
index <- ReadIndex(config)

test_that("ReadAllConfiguredSummaries", {
  dfs <- ReadAllConfiguredSummaries(config, index)
  expect_named(dfs, "foo")
  expect_equal(dfs$foo$metric, c(0.5, NA))
})

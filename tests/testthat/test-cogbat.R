test_that("ReadAllConfiguredSummaries reads metric", {
  config <- Config(
    test_path("testdata"),
    pptprefix = "PPT_",
    tests = list(foo = c("metric"))
  )
  index <- data.frame(
    ID = "1",
    dir = test_path("testdata", "PPT_1"),
    row.names = "1"
  )
  dfs <- ReadAllConfiguredSummaries(config, index)
  expect_named(dfs, "foo")
  expect_equal(dfs$foo$metric, 0.5)
  expect_equal(dfs$foo$ID, "1")
})

test_that("ReadAllConfiguredSummaries parses NAs", {
  config <- Config(
    test_path("testdata"),
    pptprefix = "PPT_",
    tests = list(foo = c("metric"))
  )
  index <- ReadIndex(config)
  dfs <- ReadAllConfiguredSummaries(config, index)
  expect_named(dfs, "foo")
  expect_equal(dfs$foo$metric, c(0.5, NA))
  expect_equal(dfs$foo$ID, c("1", "3"))
  expect_equal(as.character(dfs$foo$Group), c("A", "B"))
})

test_that("ReadAllConfiguredSummaries inserts NAs for missing files", {
  config <- Config(
    test_path("testdata"),
    pptprefix = "PPT_",
    tests = list(foo = c("metric"))
  )
  index <- data.frame(
    ID = c("1", "4"),
    dir = c(test_path("testdata", "PPT_1"), test_path("testdata", "PPT_4")),
    row.names = c("1", "4")
  )
  expect_warning(
    dfs <- ReadAllConfiguredSummaries(config, index),
    "Missing data for test: foo participant: 4"
  )
  expect_named(dfs, "foo")
  expect_equal(dfs$foo$metric, c(0.5, NA))
  expect_equal(dfs$foo$ID, c("1", "4"))
})

test_that("ReadAllConfiguredSummaries errors when all files are missing", {
  config <- Config(
    test_path("testdata"),
    pptprefix = "PPT_",
    tests = list(foo = c("metric"))
  )
  index <- data.frame(
    ID = "4",
    dir = test_path("testdata", "PPT_4"),
    row.names = "4"
  )
  expect_error(
    suppressWarnings(ReadAllConfiguredSummaries(config, index)),
    "No data for test: foo"
  )
})

test_that("ReadIndex parses index.csv", {
  config <- Config(test_path("testdata"))
  index <- ReadIndex(config)
  expect_s3_class(index, "data.frame")
  expect_equal(nrow(index), 2L)
  expect_named(index, c("ID", "Age", "Group", "dir"))
  expect_equal(index$ID, c("1", "3"))
  expect_equal(
    basename(index$dir),
    c("MRH152_1", "MRH152_3")
  )
})

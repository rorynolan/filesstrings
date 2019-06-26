context("col_stats")

test_that("mean_cols works", {
  m <- matrix(sample.int(100), ncol = 20)
  expect_equal(mean_cols(m), apply(m, 2, mean))
})

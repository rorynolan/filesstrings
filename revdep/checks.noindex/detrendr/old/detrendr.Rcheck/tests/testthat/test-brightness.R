test_that("brightness works", {
  v <- sample.int(100)
  expect_equal(brightness_vec(v), var(v) / mean(v))
  m <- matrix(v, nrow = 5)
  expect_equal(brightness_rows(m), apply(m, 1, brightness_vec))
})

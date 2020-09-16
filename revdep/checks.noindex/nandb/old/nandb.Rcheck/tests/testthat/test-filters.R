context("Filters")

test_that("median and smooth filters work", {
  m <- matrix(1:9, nrow = 3)
  m[2:3, 2:3] <- NA
  expect_equal(round(mean(median_filter(m), na.rm = TRUE), 2), NaN)
  expect_equal(
    mean(median_filter(m, na_rm = TRUE), na.rm = TRUE), 3.812,
    tolerance = 0.01
  )
  expect_equal(
    mean(median_filter(m, na_count = TRUE), na.rm = TRUE), 3.42,
    tolerance = 0.01
  )
  expect_equal(round(mean(smooth_filter(m), na.rm = TRUE), 2), NaN)
  expect_equal(
    round(mean(smooth_filter(m, na_rm = TRUE), na.rm = TRUE), 3),
    3.592
  )
  expect_equal(
    round(mean(smooth_filter(m, na_count = TRUE), na.rm = TRUE), 2),
    3.34
  )
})

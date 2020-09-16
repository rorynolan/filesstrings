test_that("cpp smoothing functions work", {
  v <- runif(10)
  expect_equal(boxcar_smooth(v, 1), weighted_smooth(v, rep(1, 3)))
  expect_equal(boxcar_smooth(v, 3), weighted_smooth(v, rep(1, 7)))
  exp5weights <- exp(-abs(-1:1) / 5)
  expect_equal(
    exp_smooth(v, 5, 1),
    weighted_smooth(v, exp5weights)
  )
  v <- seq_len(3)
  expect_equal(weighted_smooth(v, rep(1, 21)), rep(2, 3))
  if (get_os() == "mac") {
    skip_on_cran()
  } else {
    expect_error(weighted_smooth(v, rep(1, 20)), "must be odd",
      class = "C++Error"
    )
  }
})

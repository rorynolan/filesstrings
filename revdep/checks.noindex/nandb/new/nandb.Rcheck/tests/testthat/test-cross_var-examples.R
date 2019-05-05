context("Cross var")

test_that("cross_var_pillars() works", {
  x <- array(1:27, dim = rep(3, 3))
  y <- array(0:26, dim = rep(3, 3))
  z <- array(0, dim = rep(3, 2))
  for (i in 1:3) {
    for (j in 1:3) {
      z[i, j] <- cross_var(x[i, j, ], y[i, j, ])
    }
  }
  expect_equal(cross_var_pillars(x, y), z)
  expect_error(
    cross_var_pillars(x[1:2, , ], y),
    paste0(
      "`x` and `y` must have the same dimensions.+You have `x`\\s?",
      "with dimension `c\\(2, 3, 3\\)` and `y` with.+dimension\\s?",
      "`c\\(3, 3, 3\\)`"
    )
  )
})

test_that("cross_var() works", {
  expect_equal(cross_var(0:3, 2:5), 1.25)
  expect_error(
    cross_var(1:2, 1:3),
    paste0(
      "`x` and `y` must have the same length.+Your `x` is of\\s?",
      "length 2 and your `y` is of length 3"
    )
  )
})

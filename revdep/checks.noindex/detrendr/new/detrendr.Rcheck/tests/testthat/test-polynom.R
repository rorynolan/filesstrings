test_that("poly_fits works", {
  skip_on_cran()
  expect_error(poly_fit_pillars(array(3, dim = rep(3, 3)), 9), "too short")
  m <- matrix(1:4, nrow = 2)
  expect_error(poly_fit_cols(m, 9), "too short")
  m <- matrix(runif(50), ncol = 5)
  mNA <- m %T>% {
    .[, 1] <- NA
  }
  expect_equal(poly_fit_cols(mNA, 2),
    poly_fit_cols(m, 2) %T>% {
      .[, 1] <- NA
    },
    check.attributes = FALSE
  )
  x <- matrix(sample(1:100, 20), ncol = 2)
  x[1, 1] <- NA
  y <- x
  y[3, 1] <- NA
  expect_equal(poly_fit_cols(x, 3), poly_fit_cols(y, 3))
})

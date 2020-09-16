test_that("ijtiff_img works", {
  img <- array(seq_len(2^4), dim = rep(2, 4))
  eq_to <- img
  attr(eq_to, "bits_per_sample") <- 8
  class(eq_to) <- c("ijtiff_img", class(eq_to))
  expect_equal(ijtiff_img(img, bits_per_sample = 8), eq_to)
  expect_error(
    ijtiff_img(img, 8),
    paste0(
      "All arguments in ... must be named.+",
      ". Your argument 8 is not named."
    )
  )
  img <- img[, , 1, ]
  expect_equal(dim(ijtiff_img(img)), c(2, 2, 1, 2))
  expect_equal(
    ijtiff_img(matrix(1, nrow = 3, ncol = 3)),
    ijtiff_img(matrix(T, nrow = 3, ncol = 3))
  )
})

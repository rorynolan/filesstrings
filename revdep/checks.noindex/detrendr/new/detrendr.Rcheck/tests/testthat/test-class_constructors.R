context("class_constructors")

test_that("detrended_img works", {
  three_d <- array(runif(8), dim = rep(2, 3))
  expect_error(
    detrended_img(array(runif(8), dim = rep(2, 3)), "box", 5, TRUE,
      purpose = "ff"
    ),
    "Elements of a detrended_img must all be integers."
  )
  three_d[] %<>% ceiling()
  four_d <- three_d
  dim(four_d) <- c(dim(three_d)[1:2], 1, dim(three_d)[3])
  expect_equal(
    detrended_img(three_d, "exp", 9, FALSE, purpose = "ff"),
    detrended_img(four_d, "exp", 9, FALSE, purpose = "ff")
  )
  expect_equal(
    detrended_img(three_d, "rh", 9, FALSE),
    detrended_img(four_d, "robin", 9, FALSE)
  )
  expect_error(
    detrended_img(three_d, "rh", 8:9, TRUE),
    paste0(
      "The length of the `parameter` argument must be ",
      "equal to 1 or.+equal to the number of channels in `img`.+",
      "Your `img` has 1 channel and your ",
      "`parameter` argument.+is of length 2."
    )
  )
})

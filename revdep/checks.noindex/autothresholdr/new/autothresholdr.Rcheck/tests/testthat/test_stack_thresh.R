test_that("mean_stack_thresh works", {
  context("mean_stack_thresh()")
  img <- ijtiff::read_tif(system.file('extdata', '50.tif',
                          package = 'autothresholdr'))
  img_thresh_mask <- mean_stack_thresh(img, "Otsu")
  expect_equal(round(mean(img_thresh_mask, na.rm = TRUE), 2), 24.09)
  img_thresh_mask <- mean_stack_thresh(img, 13.2)
  expect_equal(round(mean(img_thresh_mask, na.rm = TRUE), 2), 24.08)
  img_thresh_mask <- mean_stack_thresh(img + 2 ^ 30, "Otsu")
  expect_equal(round(mean(img_thresh_mask, na.rm = TRUE)), 24 + 2 ^ 30)
  const_arr <- array(2, dim = rep(2, 3))
  expect_error(mean_stack_thresh(const_arr, "tri"))
  real_arr <- array(seq(2, 3, length.out = 8), dim = rep(2, 3))
  expect_error(mean_stack_thresh(real_arr, "tri"), "integer")
  expect_equal(sum(var_pillars(const_arr)), 0)
  context("med_stack_thresh()")
  img_thresh_mask <- med_stack_thresh(img, "Triangle")
  expect_equal(round(mean(img_thresh_mask, na.rm = TRUE), 3), 23.583)
  expect_error(mean_stack_thresh(img + 2 ^ 32, "Triangle"),
               "All elements .* must be in the integer range")
  expect_error(med_stack_thresh(img + 2 ^ 32, "Triangle"),
               "All elements .* must be in the integer range")
  img_thresh_mask <- mean_stack_thresh(img + 2 ^ 30, "Otsu")
  expect_equal(round(mean(img_thresh_mask, na.rm = TRUE)), 24 + 2 ^ 30)
  const_arr <- array(2, dim = rep(2, 3))
  expect_error(mean_stack_thresh(const_arr, "tri"))
  expect_error(med_stack_thresh(const_arr, "tri"))
  expect_equal(sum(var_pillars(const_arr)), 0)
  real_arr <- array(seq(2, 3, length.out = 8), dim = rep(2, 3))
  expect_error(med_stack_thresh(real_arr, "tri"), "integer")
  expect_error(mean_stack_thresh(img %T>% {.[] <- NA}, "otsu"),
               paste0(
                 "`img` cannot be all `NA`s.+Every element of your `img`\\s?",
                 "is `NA`."
               ))
  expect_error(med_stack_thresh(img %T>% {.[] <- NA}, "otsu"),
               paste0(
                 "`img` cannot be all `NA`s.+Every element of your `img`\\s?",
                 "is `NA`."
               ))
})

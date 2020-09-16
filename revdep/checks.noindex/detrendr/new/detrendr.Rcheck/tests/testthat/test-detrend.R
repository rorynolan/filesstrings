test_that("detrending works", {
  skip_if(getRversion() < "3.6.0")
  skip_on_cran()
  context("Boxcar detrending")
  img <- ijtiff::read_tif(system.file("extdata", "bleached.tif",
    package = "detrendr"
  ), msg = FALSE)
  set.seed(1)
  corrected <- img_detrend_boxcar(img[, , 1, ], "auto",
    parallel = 2,
    purpose = "ff"
  )
  expect_equal(round(mean(brightness_pillars(corrected[, , 1, ])), 2), 1.45,
    tolerance = 0.15
  )
  corrected10 <- img_detrend_boxcar(img, 10, parallel = 2, purpose = "ff")
  expect_equal(round(mean(brightness_pillars(corrected10[, , 1, ])), 2), 1.27)
  corrected50 <- img_detrend_boxcar(img, 50, parallel = 2, purpose = "ff")
  expect_equal(round(mean(brightness_pillars(corrected50[, , 1, ])), 2), 2.08)
  corrected100 <- img_detrend_boxcar(img, 100, parallel = 2, purpose = "ff")
  expect_equal(round(mean(brightness_pillars(corrected100[, , 1, ])), 2), 3.04)
  corrected300 <- img_detrend_boxcar(img, 300, parallel = 2, purpose = "ff")
  expect_equal(round(mean(brightness_pillars(corrected300[, , 1, ])), 2), 5.6)
  correctedNA <- img_detrend_boxcar(img, NA, purpose = "ff")
  expect_equal(
    list(dim(correctedNA), as.vector(correctedNA)),
    list(dim(img), as.vector(img))
  )
  expect_error(
    img_detrend_boxcar(img, 4),
    paste0(
      "You must choose \\*either\\* 'FCS' \\*or\\* 'FFS' for\\s?",
      "`purpose`."
    )
  )
  context("Exponential filtering detrending")
  corrected <- img_detrend_exp(img[, , 1, ], "auto",
    parallel = 2,
    purpose = "ff"
  )
  expect_equal(round(mean(brightness_pillars(corrected[, , 1, ])), 2), 1.64,
    tolerance = 0.1
  )
  corrected10 <- img_detrend_exp(img, 10, parallel = 2, purpose = "ff")
  expect_equal(round(mean(brightness_pillars(corrected10[, , 1, ])), 2), 1.29)
  corrected50 <- img_detrend_exp(img, 50, parallel = 2, purpose = "ff")
  expect_equal(round(mean(brightness_pillars(corrected50[, , 1, ])), 2), 2.38)
  corrected100 <- img_detrend_exp(img, 100, parallel = 2, purpose = "ff")
  expect_equal(round(mean(brightness_pillars(corrected100[, , 1, ])), 2), 3.38)
  corrected1000 <- img_detrend_exp(img, 1000, parallel = 2, purpose = "ff")
  expect_equal(round(mean(brightness_pillars(corrected1000[, , 1, ])), 2), 6.01)
  correctedNA <- img_detrend_exp(img, NA, purpose = "ff")
  expect_equal(
    list(dim(correctedNA), as.vector(correctedNA)),
    list(dim(img), as.vector(img))
  )
  expect_error(
    img_detrend_exp(img, 4),
    paste0(
      "You must choose \\*either\\* 'FCS' \\*or\\* 'FFS' for\\s?",
      "`purpose`."
    )
  )
  context("Polynomial detrending")
  expect_warning(
    img_detrend_polynom(img, "auto", parallel = 2, purpose = "ff"),
    "polynomial degree"
  )
  expect_equal(img_detrend_degree_specified(img, NA, "ff", FALSE),
    img,
    check.attributes = FALSE
  )
  corrected <- suppressWarnings(img_detrend_polynom(img, "auto",
    purpose = "ff",
    parallel = 2
  ))
  expect_equal(mean(brightness_pillars(corrected[, , 1, ]), na.rm = TRUE),
    1.6,
    tolerance = 0.15
  )
  corrected1 <- img_detrend_polynom(img[, , 1, ], 1,
    parallel = 2,
    purpose = "ff"
  )
  expect_equal(round(mean(brightness_pillars(corrected1[, , 1, ]),
    na.rm = TRUE
  ), 1), 4.5)
  corrected2 <- img_detrend_polynom(img, 2, parallel = 2, purpose = "ff")
  expect_equal(round(mean(brightness_pillars(corrected2[, , 1, ]),
    na.rm = TRUE
  ), 1), 3.5)
  corrected4 <- img_detrend_polynom(img, 4, parallel = 2, purpose = "ff")
  expect_equal(round(mean(brightness_pillars(corrected4[, , 1, ]),
    na.rm = TRUE
  ), 1), 2.6)
  corrected8 <- img_detrend_polynom(img, 8, parallel = 2, purpose = "ff")
  expect_equal(round(mean(brightness_pillars(corrected8[, , 1, ]),
    na.rm = TRUE
  ), 1), 1.9)
  correctedNA <- img_detrend_polynom(img, NA, purpose = "ff")
  expect_equal(
    list(dim(correctedNA), as.vector(correctedNA)),
    list(dim(img), as.vector(img))
  )
  expect_error(
    img_detrend_polynom(img, 4),
    paste0(
      "You must choose \\*either\\* 'FCS' \\*or\\* 'FFS' for\\s?",
      "`purpose`."
    )
  )
  context("Robin Hood detrending")
  corrected <- img_detrend_rh(img)
  expect_equal(mean(brightness_pillars(corrected)), 4.38, tolerance = 0.01)
  set.seed(8)
  x1 <- img_detrend_rh(img, 10)
  set.seed(8)
  x2 <- img_detrend_rh(img[, , 1, ], 10)
  expect_equal(x1, x2)
  skip_if_not_installed("abind")
  twoch <- system.file("extdata", "2ch_ij.tif", package = "detrendr") %>%
    ijtiff::read_tif(msg = FALSE)
  set.seed(8)
  x1 <- img_detrend_rh(twoch, c("auto", NA), quick = TRUE)
  set.seed(8)
  x2 <- img_detrend_rh(twoch[, , 1, ], "auto", quick = TRUE) %>%
    abind::abind(twoch[, , 2, , drop = FALSE], along = 3)
  expect_equal(list(dim(x1), as.vector(x1)), list(dim(x2), as.vector(x2)))
  set.seed(8)
  x2 <- img_detrend_rh(twoch, c("auto", 0), quick = TRUE)
  expect_equal(x1, x2)
  expect_error(
    img_detrend_rh(twoch, 1:3),
    paste(
      "Your `swaps` argument has length 3 and your",
      "image has 2.+channels."
    )
  )
  expect_error(
    img_detrend_rh(twoch, c(-1, 1)),
    paste(
      "`swaps` must be greater than or equal to zero.*",
      "You have .* equal to -1"
    )
  )
  expect_error(
    img_detrend_rh(twoch, "wrong"),
    paste("If `swaps` is a string.* only permissible value .*auto")
  )
  expect_error(
    img_detrend_rh(twoch, TRUE),
    paste0(
      "`swaps` must be specified as a positive number or",
      ".*auto.* You have used.*TRUE"
    )
  )
  arr3d <- array(0, dim = rep(3, 3)) %T>% {
    .[5] <- 1
  }
  expect_error(
    img_detrend_swaps_specified(arr3d, 2),
    paste0(
      "Your image is too close to zero.+Can't detrend an\\s?",
      "image with so few nonzero values.+`img` has 27\\s?",
      "elements and just 1 of them are greater.+than zero."
    )
  )
})

context("Detrending errors")
test_that("detrending errors correctly", {
  img <- ijtiff::read_tif(system.file("extdata", "bleached.tif",
    package = "detrendr"
  ), msg = FALSE)
  expect_error(img_detrend_boxcar(img, "abc", purpose = "ff"), "auto")
  expect_error(img_detrend_exp(img, "abc", purpose = "ff"), "auto")
  expect_error(img_detrend_polynom(img, "abc", purpose = "ff"), "auto")
  expect_error(
    img_detrend_boxcar(img, FALSE, purpose = "ff"),
    "positive number or"
  )
  expect_error(
    img_detrend_exp(img, FALSE, purpose = "ff"),
    "positive number or"
  )
  expect_error(
    img_detrend_polynom(img, FALSE, purpose = "ff"),
    "positive number or"
  )
  img <- img[, , 1, ]
  expect_error(
    img_detrend_polynom(img, degree = 1:7, purpose = "ff"),
    "length 1 or length equal to the.+number of channels.+7.+1"
  )
  expect_error(
    img_detrend_boxcar(img, l = 1:7, purpose = "ff"),
    "length 1 or length equal to the number.+of channels.+7.+1"
  )
  expect_error(
    img_detrend_exp(img, tau = 1:7, purpose = "ff"),
    "length 1 or length equal to the.+number.+of channels.+7.+1"
  )
  expect_error(
    img_detrend_polynom(img, degree = -1, purpose = "ff"),
    "must be greater than zero"
  )
  expect_error(
    img_detrend_polynom(img, degree = 1.5, purpose = "ff"),
    "must be an integer"
  )
  expect_error(
    img_detrend_boxcar(img, l = -1, purpose = "ff"),
    "must be greater than zero"
  )
  expect_error(
    img_detrend_boxcar(img, l = 1.5, purpose = "ff"),
    "must be an integer"
  )
  expect_error(
    img_detrend_exp(img, tau = -1, purpose = "ff"),
    "must be greater than zero"
  )
})

context("Automatic parameter finding")

test_that("best_tau works", {
  skip_on_os("solaris")
  img <- ijtiff::read_tif(system.file("extdata", "bleached.tif",
                                      package = "detrendr"), msg = FALSE)
  set.seed(1)
  expect_equal(round(best_tau(img, purpose = "ffs", parallel = 2)),
               34, tolerance = 2)
  img <- array(rpois(99 ^ 3, 99), dim = rep(99, 3))
  bt <- best_tau(img, purpose = "ffs")
  if (!is.na(bt))
    expect_equal(bt, 6618, tolerance = 6400)
  img[] <- 0
  expect_error(best_tau(img, purpose = "fcs"), "all pixel values are equal to 0")
})

test_that("best_l works", {
  skip_on_os("solaris")
  img <- ijtiff::read_tif(system.file("extdata", "bleached.tif",
                                      package = "detrendr"), msg = FALSE)
  set.seed(1)
  expect_equal(round(best_l(img, parallel = 2, purpose = "ffs")),
               17, tolerance = 2)
  set.seed(4)
  img <- array(rpois(99 ^ 3, 99), dim = rep(99, 3))
  bt <- round(best_tau(img, purpose = "ffs"))
  if (!is.na(bt))
    expect_equal(bt, 28372, tolerance = 28200)
  img[] <- 0
  expect_error(best_l(img, purpose = "ffs"), "all pixel values are equal to 0")
  img <- array(round(seq(0, .Machine$integer.max, length.out = 2 ^ 3)),
               dim = rep(2, 3))
  expect_error(best_l(img, purpose = "ffs"),
               "Even with.*the most severe detrend")
})

test_that("best_degree works", {
  img <- ijtiff::read_tif(system.file("extdata", "bleached.tif",
                                      package = "detrendr"), msg = FALSE)
  set.seed(1)
  best_degree <- suppressWarnings(round(best_degree(img, purpose = "ffs")))
  expect_equal(best_degree, 17, tolerance = 2)
  set.seed(7)
  img <- array(rpois(99 ^ 3, 99), dim = rep(99, 3))
  best_degree <- suppressWarnings(round(best_degree(img, purpose = "ffs")))
  expect_equal(best_degree, NA_real_)
  img[] <- 0
  expect_error(best_degree(img, purpose = "ffs"),
               "all pixel values are equal to 0")
})

test_that("best_swaps() works", {
  img <- ijtiff::read_tif(system.file("extdata", "bleached.tif",
                                      package = "detrendr"))
  set.seed(1)
  expect_equal(best_swaps(img), 1588366, tolerance = 4000)
  expect_error(best_swaps(array(7, dim = rep(1, 4))),
               paste("Your image is constant: all pixel values are equal to 7.",
                     "This type of image is not detrendable."))
  for (i in seq_len(99)) {  # ensure covering brightness part of `best_swaps()`
    set.seed(i)
    sim_img <- array(rpois(16 ^ 3, 16), dim = rep(16, 3))
    expect_lt(best_swaps(sim_img), sum(sim_img))
  }
})

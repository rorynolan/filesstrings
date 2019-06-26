context("Auto thresh methods")

img <- system.file("extdata", "eg.tif", package = "autothresholdr") %>%
  ijtiff::read_tif()

test_that("IJDefault works", {
  expect_equal(autothresholdr:::IJDefault(c(0, 2, 0)), 1,
    check.attributes = FALSE
  )
})

test_that("Huang2 works", {
  expect_equal(auto_thresh(img, "Huang"), auto_thresh(img, "Huang2"),
    check.attributes = FALSE
  )
  expect_equal(Huang2(3), 0, check.attributes = FALSE)
})

test_that("Intermodes works", {
  expect_equal(as.vector(auto_thresh(img, "inter")), 12)
})

test_that("IsoData works", {
  expect_equal(as.vector(auto_thresh(img, "iso")), 13)
})

test_that("Li works", {
  expect_equal(as.vector(auto_thresh(img, "Li")), 7)
})

test_that("Intermodes works", {
  expect_equal(as.vector(auto_thresh(img, "inter")), 12)
})

test_that("MaxEntropy works", {
  expect_error(as.vector(auto_thresh(img, "maxe")), "failed to find threshold")
})

test_that("Mean works", {
  expect_equal(autothresholdr:::Mean(rep(9, 7)), 3)
})

test_that("MinErrorI works", {
  expect_equal(as.vector(auto_thresh(img, "mine")), 21)
})

test_that("Intermodes works", {
  expect_equal(as.vector(auto_thresh(img, "inter")), 12)
})

test_that("Minimum works", {
  expect_equal(as.vector(auto_thresh(img, "mini")), 7)
})

test_that("Moments works", {
  expect_equal(as.vector(auto_thresh(img, "mom")), 19)
})

test_that("Percentile works", {
  expect_equal(as.vector(auto_thresh(img, "perc")), 22)
})

test_that("RenyiEntropy works", {
  expect_equal(as.vector(auto_thresh(img, "ren")), 34)
})

test_that("Shanbhag works", {
  ans <- th(23L, FALSE, FALSE, FALSE, "Shanbhag")
  expect_equal(auto_thresh(img, "shan"), ans)
})

test_that("Yen works", {
  expect_error(auto_thresh(img, "y"), "failed to find threshold")
})

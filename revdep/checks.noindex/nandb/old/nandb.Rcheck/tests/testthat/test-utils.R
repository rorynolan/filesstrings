context("Utils")

test_that("nb_get_img() errors correctly", {
  set.seed(1)
  img <- array(runif(8), dim = rep(2, 3))
  expect_error(
    nb_get_img(img),
    paste0(
      "`img` must be positive integers \\(and NAs\\)\\s?",
      "only.+Element 1 of `img` is (\\d|\\.)+ which is\\s?",
      "neither.+NA nor positive integer."
    )
  )
  img <- array(8, dim = c(2, 2, 2, 1))
  expect_error(
    nb_get_img(img),
    paste0(
      "Your image only has one frame.+Images to be used for\\s?",
      "number and brightness analysis.+must have more than\\s?",
      "one frame."
    )
  )
})

test_that("`can_be_numeric()` works", {
  expect_false(can_be_numeric(c("a", "1")))
})

test_that("extend_for_all_chs() edge case works", {
  expect_equal(extend_for_all_chs(list(NULL), 4), as.list(rep(NA, 4)))
})

test_that("fix_filt() edge cases and exceptions work correctly", {
  expect_equal(fix_filt(NULL), NA_character_)
  expect_error(fix_filt("abc"), "must be either")
})

test_that("prepare_filt() edge cases and exceptions work correctly", {
  expect_error(
    prepare_filt("wrong"),
    paste(
      "The allowable values for filt are.*smooth.*median.*NA.*",
      "You have `filt =.*wrong"
    )
  )
})

test_that("deduplicate_nb_filename() works correctly", {
  path <- paste0(
    "detrended_exponential_thresh=4,3_swaps=1,2",
    "_brightness_thresh=NA,NA_swaps=NA,NA_filt=NA,NA.tif"
  )
  expect_equal(
    deduplicate_nb_filename(path),
    paste0(
      "detrended_exponential_thresh=4,3_swaps=1,2",
      "_brightness_filt=NA,NA.tif"
    )
  )
  path <- paste0(
    "detrended_exponential_thresh=4,3_swaps=1,2",
    "_brightness_thresh=NA,NA_swaps=NA,NA_"
  )
  expect_equal(
    deduplicate_nb_filename(path),
    paste0(
      "detrended_exponential_thresh=4,3_swaps=1,2",
      "_brightness"
    )
  )
  path <- paste0(
    "detrended_exponential_thresh=4,3_swaps=1,2",
    "_brightness_thresh=NA,NA_"
  )
  expect_equal(
    deduplicate_nb_filename(path),
    paste0(
      "detrended_exponential_thresh=4,3_swaps=1,2",
      "_brightness"
    )
  )
  expect_equal(deduplicate_nb_filename("abc"), "abc")
})

test_that("prepare_thresh works", {
  expect_equal(prepare_thresh(NULL), as.list(rep(NA_real_, 2)))
  expect_equal(prepare_thresh(NA), as.list(rep(NA_real_, 2)))
  expect_equal(prepare_thresh(c("H", NA)), list("H", NA_real_))
  expect_equal(prepare_thresh("H"), as.list(rep("H", 2)))
})

test_that("deduplicate_cc_nb_filename() works correctly", {
  path <- paste0(
    "detrended_exponential_thresh=4,3_swaps=1,2",
    "_brightness_thresh=NA,NA_swaps=NA,NA_filt=NA,NA.tif"
  )
  expect_equal(
    deduplicate_cc_nb_filename(path),
    paste0(
      "detrended_exponential_thresh=4,3_swaps=1,2",
      "_brightness_filt=NA,NA.tif"
    )
  )
  path <- paste0(
    "detrended_exponential_thresh=4,3_swaps=1,2",
    "_brightness_thresh=NA,NA_swaps=NA,NA_"
  )
  expect_equal(
    deduplicate_cc_nb_filename(path),
    paste0(
      "detrended_exponential_thresh=4,3_swaps=1,2",
      "_brightness"
    )
  )
  path <- paste0(
    "detrended_exponential_thresh=4,3_swaps=1,2",
    "_brightness_thresh=NA,NA_"
  )
  expect_equal(
    deduplicate_cc_nb_filename(path),
    paste0(
      "detrended_exponential_thresh=4,3_swaps=1,2",
      "_brightness"
    )
  )
  expect_equal(deduplicate_cc_nb_filename("abc"), "abc")
})

test_that("make_cc_nb_filename_ending() edge cases work correctly", {
  img <- ijtiff::read_tif(system.file("extdata", "two_ch.tif",
    package = "nandb"
  ))
  cc_b1 <- cc_brightness(img, thresh = "Huang", detrend = FALSE)
  cc_b2 <- cc_b1
  attr(cc_b1, "thresh") <- c(1, 1)
  attr(cc_b2, "thresh") <- structure(c(1, 1),
    autothresh_method = rep(NA, 2)
  )
  expect_equal(
    make_cc_nb_filename_ending(cc_b1),
    make_cc_nb_filename_ending(cc_b2)
  )
  cc_b_ts <- cc_brightness_timeseries(img, 20)
  attr(cc_b_ts, "frames_per_set") <- NULL
  expect_error(
    make_cc_nb_filename_ending(cc_b_ts),
    paste0(
      "If `cc_nb_img` is a cross-correlated number or\\s?",
      "brightness time.+series, then it must have an\\s?",
      "attribute 'frames_per_set'.+Your `cc_nb_img` appears\\s?",
      "to be a cross-correlated number.+or brightness time\\s?",
      "series without a 'frames_per_set'.+attribute."
    )
  )
})

test_that("`make_nb_filename_ending()` works", {
  img <- ijtiff::read_tif(system.file("extdata", "two_ch.tif",
    package = "nandb"
  ))
  nts <- number_timeseries(img, "N", 20)
  attr(nts, "frames_per_set") <- NULL
  expect_error(
    make_nb_filename_ending(nts),
    paste0(
      "If `nb_img` is a number or brightness time series,\\s?",
      "then it.+must have an attribute 'frames_per_set'.+Your\\s?",
      "`nb_img` appears to be a number or brightness\\s?",
      "time.+series without a 'frames_per_set' attribute."
    )
  )
  nts <- number_timeseries(img, "N", 20)
  attr(nts, "thresh") <- NA
  expect_error(
    make_nb_filename_ending(nts),
    paste0(
      "The lengths of the 'thresh', 'swaps' and 'filt'\\s?",
      "attributes and.+the 'autothresh_method' and 'auto'\\s?",
      "attriutes of the thresh and.+swaps attributes\\s?",
      "respectively of `nb_img` must all be the same.+as the\\s?",
      "number of channels in `img`.+There are 2 channels in\\s?",
      "`img`.+The 'thresh' attribute has length 1.+The\\s?",
      "'swaps' attribute has length 2.+The 'filt' attribute\\s?",
      "has length 2.+The 'autothresh_method' attribute of the\\s?",
      "'thresh'.+attribute has length 2.+The 'auto' attribute\\s?",
      "of the 'swaps' attribute has length.+2."
    )
  )
})

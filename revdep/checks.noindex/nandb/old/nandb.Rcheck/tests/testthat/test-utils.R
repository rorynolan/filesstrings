context("Utils")

test_that("nb_get_img() errors correctly", {
  img <- array(runif(8), dim = rep(2, 3))
  expect_error(nb_get_img(img), "pos.*int.*NA.*only")
})

test_that("extend_for_all_chs() edge case works", {
  expect_equal(extend_for_all_chs(list(NULL), 4), as.list(rep(NA, 4)))
})

test_that("fix_filt() edge cases and exceptions work correctly", {
  expect_equal(fix_filt(NULL), NA_character_)
  expect_error(fix_filt("abc"), "must be either")
})

test_that("prepare_filt() edge cases and exceptions work correctly", {
  expect_error(prepare_filt("wrong"),
               paste("The allowable values for filt are.*smooth.*median.*NA.*",
                     "You have `filt =.*wrong"))
})

test_that("deduplicate_nb_filename() works correctly", {
  path <- paste0("detrended_exponential_thresh=4,3_tau=1,2",
                 "_brightness_thresh=NA,NA_tau=NA,NA_filt=NA,NA.tif")
  expect_equal(deduplicate_nb_filename(path),
               paste0("detrended_exponential_thresh=4,3_tau=1,2",
                      "_brightness_filt=NA,NA.tif"))
  path <- paste0("detrended_exponential_thresh=4,3_tau=1,2",
                 "_brightness_thresh=NA,NA_tau=NA,NA_")
  expect_equal(deduplicate_nb_filename(path),
               paste0("detrended_exponential_thresh=4,3_tau=1,2",
                      "_brightness"))
  path <- paste0("detrended_exponential_thresh=4,3_tau=1,2",
                 "_brightness_thresh=NA,NA_")
  expect_equal(deduplicate_nb_filename(path),
               paste0("detrended_exponential_thresh=4,3_tau=1,2",
                      "_brightness"))
  expect_equal(deduplicate_nb_filename("abc"), "abc")
})


test_that("prepare_tau works", {
  expect_equal(prepare_tau(NULL), as.list(rep(NA, 2)))
  expect_equal(prepare_tau(list(NULL, 4)), list(NA, 4))
  expect_equal(prepare_tau(NA), as.list(rep(NA, 2)))
  expect_equal(prepare_tau(list("a", NULL)), list("auto", NA))
  expect_error(prepare_tau("x"), "string.*must be.*auto.*Element 1.*x")
  expect_error(prepare_tau(1:3),
               paste("`tau` should have length 1 or 2.*",
                     "Yours has length 3."))
  expect_equal(prepare_tau(as.character(1:2)), prepare_tau(1:2))
})

test_that("prepare_thresh works", {
  expect_equal(prepare_thresh(NULL), as.character(rep(NA, 2)))
  expect_equal(prepare_thresh(NA), as.character(rep(NA, 2)))
  expect_equal(prepare_thresh(c("H", NA)), c("H", NA))
  expect_equal(prepare_thresh("H"), rep("H", 2))
})

test_that("deduplicate_cc_nb_filename() works correctly", {
  path <- paste0("detrended_exponential_thresh=4,3_tau=1,2",
                 "_brightness_thresh=NA,NA_tau=NA,NA_filt=NA,NA.tif")
  expect_equal(deduplicate_cc_nb_filename(path),
               paste0("detrended_exponential_thresh=4,3_tau=1,2",
                      "_brightness_filt=NA,NA.tif"))
  path <- paste0("detrended_exponential_thresh=4,3_tau=1,2",
                 "_brightness_thresh=NA,NA_tau=NA,NA_")
  expect_equal(deduplicate_cc_nb_filename(path),
               paste0("detrended_exponential_thresh=4,3_tau=1,2",
                      "_brightness"))
  path <- paste0("detrended_exponential_thresh=4,3_tau=1,2",
                 "_brightness_thresh=NA,NA_")
  expect_equal(deduplicate_cc_nb_filename(path),
               paste0("detrended_exponential_thresh=4,3_tau=1,2",
                      "_brightness"))
  expect_equal(deduplicate_cc_nb_filename("abc"), "abc")
})

test_that("make_cc_nb_filename_ending() edge cases work correctly", {
  img <- ijtiff::read_tif(system.file("extdata", "two_ch.tif",
                                      package = "nandb"))
  cc_b1 <- cc_brightness(img, tau = 9, thresh = "Huang")
  cc_b2 <- cc_b1
  attr(cc_b1, "thresh") <- c(1, 1)
  attr(cc_b2, "thresh") <- rlang::set_attrs(c(1, 1),
                                            autothresh_method = rep(NA, 2))
  expect_equal(make_cc_nb_filename_ending(cc_b1),
               make_cc_nb_filename_ending(cc_b2))
})

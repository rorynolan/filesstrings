context("cc_brightness()")
test_that("cc_brightness() works", {
  img <- ijtiff::read_tif(system.file("extdata", "two_ch.tif",
                                      package = "nandb"))
  set.seed(1)
  cc_b <- cc_brightness(img, tau = "auto", thresh = "Huang", filt = "median")
  expect_equal(median(cc_b, na.rm = TRUE), 0.014, tolerance = 0.01)
  cc_b <- cc_brightness(img, tau = "auto", thresh = "Huang", filt = "smooth")
  expect_equal(median(cc_b, na.rm = TRUE), 0.014, tolerance = 0.008)
  expect_error(cc_brightness(img, tau = "wrong", thresh = "Huang"),
               paste("If an element of `tau` is a string, is must be.*auto.*",
                     "Element 1 of your `tau` is.*wrong"))
  expect_error(cc_brightness(img, tau = FALSE, thresh = "Huang"),
               paste("If `tau` is not numeric, then it must be NA or.*auto.*",
                     "You have `tau = FALSE`."))
})

context("cc_brightness_timeseries()")
test_that("cc_brightness_timeseries() works", {
  img <- ijtiff::read_tif(system.file("extdata", "two_ch.tif",
                                      package = "nandb"))
  set.seed(1)
  cc_b_ts <- cc_brightness_timeseries(img, 10, thresh = "Huang", tau = "auto",
                                       filt = "median", parallel = 2)
  expect_equal(median(cc_b_ts, na.rm = TRUE), 0, tolerance = 0.01)
  cc_b_ts <- cc_brightness_timeseries(img, 10, thresh = "Huang", tau = "auto",
                                       filt = "smooth", parallel = 2)
  expect_equal(median(cc_b_ts, na.rm = TRUE), 0.0013, tolerance = 0.003)
  expect_error(cc_brightness_timeseries(img, 9999),
               paste("You have selected 9999 frames per set,",
                     "but there are only 100 frames in total."))
  expect_error(cc_brightness_timeseries(img, 9, tau = "wrong"),
               paste("If an element of `tau` is a string, is must be.*auto.*",
                     "Element 1 of your `tau` is.*wrong"))
  expect_error(cc_brightness_timeseries(img, 9, tau = FALSE),
               paste("If `tau` is not numeric, then it must be NA or.*auto.*",
                     "You have `tau = FALSE`."))
})


context("cc_brightness_folder()")
test_that("cc_brightness_folder() works", {
  img <- ijtiff::read_tif(system.file("extdata", "two_ch.tif",
                                      package = "nandb"))
  cwd <- getwd()
  on.exit(setwd(cwd))
  setwd(tempdir())
  ijtiff::write_tif(img, 'a.tif')
  set.seed(1)
  cc_brightness_folder(thresh = "Huang")
  cc_b <- dir(pattern = "cc_brightness", recursive = TRUE) %>%
    ijtiff::read_tif()
  expect_equal(median(cc_b, na.rm = TRUE), 0.02, tolerance = 0.002)
  filesstrings::dir.remove("cc_brightness")
  dir.create("dir")
  ijtiff::write_tif(img, 'dir/a.tif')
  set.seed(1)
  cc_brightness_file('dir/a.tif', thresh = "Huang")
  cc_b <- dir(pattern = "cc_brightness", recursive = TRUE) %>%
    ijtiff::read_tif()
  expect_equal(median(cc_b, na.rm = TRUE), 0.02, tolerance = 0.002)
  filesstrings::dir.remove("dir")
  file.remove("a.tif")
  setwd(cwd)
})

context("cc_brightness_timeseries_folder()")
test_that("cc_brightness_timeseries_folder() works", {
  img <- ijtiff::read_tif(system.file("extdata", "two_ch.tif",
                                      package = "nandb"))
  cwd <- getwd()
  on.exit(setwd(cwd))
  setwd(tempdir())
  ijtiff::write_tif(img, 'a.tif')
  set.seed(1)
  cc_brightness_timeseries_folder(thresh = "Huang", frames_per_set = 10,
                               filt = "median", parallel = 2)
  cc_b_ts <- dir(pattern = "cc_brightness_timeseries", recursive = TRUE) %>%
    ijtiff::read_tif()
  expect_equal(median(cc_b_ts, na.rm = TRUE), 0, tolerance = 0.001)
  filesstrings::dir.remove("cc_brightness_timeseries")
  dir.create("dir")
  ijtiff::write_tif(img, 'dir/a.tif')
  set.seed(1)
  cc_brightness_timeseries_file('dir/a.tif', thresh = "Huang",
                                 frames_per_set = 10,
                                 filt = "median", parallel = 2)
  cc_b_ts <- dir(pattern = "cc_brightness_timeseries", recursive = TRUE) %>%
    ijtiff::read_tif()
  expect_equal(median(cc_b_ts, na.rm = TRUE), 0, tolerance = 0.001)
  filesstrings::dir.remove("dir")
  file.remove("a.tif")
  setwd(cwd)
})



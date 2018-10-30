context("cc_number()")
test_that("cc_number() works", {
  img <- ijtiff::read_tif(system.file("extdata", "two_ch.tif",
                                      package = "nandb"))
  set.seed(1)
  cc_n <- cc_number(img, tau = "auto", thresh = "Huang", filt = "median")
  expect_equal(median(cc_n, na.rm = TRUE), 24, tolerance = 25)
  cc_n <- cc_number(img, tau = "auto", thresh = "Huang", filt = "smooth")
  expect_equal(median(cc_n, na.rm = TRUE), 4, tolerance = 10)
  expect_error(cc_number(img, tau = "wrong", thresh = "Huang"),
               paste("If an element of `tau` is a string, is must be.*auto.*",
                     "Element 1 of your `tau` is.*wrong"))
  expect_error(cc_number(img, tau = FALSE, thresh = "Huang"),
               paste("If `tau` is not numeric, then it must be NA or.*auto.*",
                     "You have `tau = FALSE`."))
})

context("cc_number_timeseries()")
test_that("cc_number_timeseries() works", {
  img <- ijtiff::read_tif(system.file("extdata", "two_ch.tif",
                                      package = "nandb"))
  set.seed(1)
  cc_n_ts <- cc_number_timeseries(img, 10, thresh = "Huang",
                                   filt = "median", parallel = 2)
  expect_equal(median(cc_n_ts, na.rm = TRUE), -1.8, tolerance = 2)
  cc_n_ts <- cc_number_timeseries(img, 10, thresh = "Huang", tau = "auto",
                                      filt = "smooth", parallel = 2)
  expect_equal(median(cc_n_ts, na.rm = TRUE), 0, tolerance = 2)
  expect_error(cc_number_timeseries(img, 9999),
               paste("You have selected 9999 frames per set,",
                     "but there are only 100 frames in total."))
  expect_error(cc_number_timeseries(img, 9, tau = "wrong"),
               paste("If an element of `tau` is a string, is must be.*auto.*",
                     "Element 1 of your `tau` is.*wrong"))
  expect_error(cc_number_timeseries(img, 9, tau = FALSE),
               paste("If `tau` is not numeric, then it must be NA or.*auto.*",
                     "You have `tau = FALSE`."))
})


context("cc_number_folder()")
test_that("cc_number_folder() works", {
  img <- ijtiff::read_tif(system.file("extdata", "two_ch.tif",
                                      package = "nandb"))
  cwd <- getwd()
  on.exit(setwd(cwd))
  setwd(tempdir())
  ijtiff::write_tif(img, 'a.tif')
  set.seed(1)
  cc_number_folder(thresh = "Huang")
  cc_n <- dir(pattern = "cc_number", recursive = TRUE) %>%
    ijtiff::read_tif()
  expect_equal(median(cc_n, na.rm = TRUE), 24, tolerance = 25)
  filesstrings::dir.remove("cc_number")
  dir.create("dir")
  ijtiff::write_tif(img, 'dir/a.tif')
  set.seed(1)
  cc_number_file('dir/a.tif', thresh = "Huang")
  cc_n <- dir(pattern = "cc_number", recursive = TRUE) %>%
    ijtiff::read_tif()
  expect_equal(median(cc_n, na.rm = TRUE), 22.2, tolerance = 3)
  filesstrings::dir.remove("dir")
  file.remove("a.tif")
  setwd(cwd)
})

context("cc_number_timeseries_folder()")
test_that("cc_number_timeseries_folder() works", {
  img <- ijtiff::read_tif(system.file("extdata", "two_ch.tif",
                                      package = "nandb"))
  cwd <- getwd()
  on.exit(setwd(cwd))
  setwd(tempdir())
  ijtiff::write_tif(img, 'a.tif')
  set.seed(1)
  cc_number_timeseries_folder(thresh = "Huang", frames_per_set = 10,
                               filt = "median", parallel = 2)
  cc_n_ts <- dir(pattern = "cc_number_timeseries", recursive = TRUE) %>%
    ijtiff::read_tif()
  expect_equal(median(cc_n_ts, na.rm = TRUE), -1.8, tolerance = 2)
  filesstrings::dir.remove("cc_number_timeseries")
  dir.create("dir")
  ijtiff::write_tif(img, 'dir/a.tif')
  set.seed(1)
  cc_number_timeseries_file('dir/a.tif', thresh = "Huang",
                                frames_per_set = 10,
                                filt = "median", parallel = 2)
  cc_n_ts <- dir(pattern = "cc_number_timeseries", recursive = TRUE) %>%
    ijtiff::read_tif()
  expect_equal(median(cc_n_ts, na.rm = TRUE), -1.8, tolerance = 2)
  filesstrings::dir.remove("dir")
  file.remove("a.tif")
  setwd(cwd)
})



context("cc_brightness()")
test_that("cc_brightness() works", {
  img <- ijtiff::read_tif(system.file("extdata", "two_ch.tif",
    package = "nandb"
  ))
  set.seed(1)
  cc_b <- cc_brightness(img, thresh = "Huang", detrend = TRUE, filt = "median")
  expect_equal(median(cc_b, na.rm = TRUE), 0.014, tolerance = 0.01)
  cc_b <- cc_brightness(img, thresh = "Huang", filt = "smooth")
  expect_equal(median(cc_b, na.rm = TRUE), 0.014, tolerance = 0.008)
  expect_error(cc_brightness(img, ch2 = 3),
               paste0(
                 "You have requested to use channel 3, but your image\\s?",
                 "has only 2.+channels in total.+This is not possible.+\\s?",
                 "Please retry with valid channel.+numbers."
               ))
  expect_error(cc_brightness(
    img %T>% {
      .[, , 1, ] <- NA
    }
  ),
  paste0(
    "The first channel is all NAs.+Can't compute on an\\s?",
    "array of all NAs."
  ))
  expect_error(cc_brightness(
    img %T>% {
      .[, , 2, ] <- NA
    }
  ),
  paste0(
    "The second channel is all NAs.+Can't compute on an\\s?",
    "array of all NAs."
  ))
  expect_error(cc_brightness(img, thresh = max(img) + 1),
               paste0(
                 "After thresholding, the first channel is all\\s?",
                 "NAs.+Can't compute on an array of all NAs.+You need to\\s?",
                 "choose a less severe threshold for this.+channel."
               ))
  expect_error(cc_brightness(img, thresh = c(0, max(img) + 1)),
               paste0(
                 "After thresholding, the second channel is all\\s?",
                 "NAs.+Can't compute on an array of all NAs.+You need to\\s?",
                 "choose a less severe threshold for this.+channel."
               ))
})

context("cc_brightness_timeseries()")
test_that("cc_brightness_timeseries() works", {
  img <- ijtiff::read_tif(system.file("extdata", "two_ch.tif",
    package = "nandb"
  ))
  set.seed(1)
  cc_b_ts <- cc_brightness_timeseries(img, 10,
    thresh = "Huang", detrend = TRUE, filt = "median")
  expect_equal(median(cc_b_ts, na.rm = TRUE), 0, tolerance = 0.01)
  cc_b_ts <- cc_brightness_timeseries(img, 10,
    thresh = "Huang", filt = "smooth")
  expect_equal(median(cc_b_ts, na.rm = TRUE), 0.0013, tolerance = 0.003)
  cc_b_ts_overlapped <- cc_brightness_timeseries(img, 10, overlap = TRUE,
                                                 thresh = "H", filt = "s")
  common_frames <- which(seq_len(dim(img)[4]) %% 10 == 1)
  expect_equal(
    cc_b_ts_overlapped[, , , common_frames, drop = FALSE] %>% {
      list(dim(.), as.vector(.))
    },
    cc_b_ts %>% {
      list(dim(.), as.vector(.))
    }
  )
  cc_b <- cc_brightness(img)
  cc_b_ts_overlapped <- cc_brightness_timeseries(img, dim(img)[4],
                                                 overlap = TRUE)
  expect_equal(
    cc_b_ts_overlapped %>% {
      list(dim(.), as.vector(.))
    },
    cc_b %>% {
      list(dim(.), as.vector(.))
    }
  )
  expect_error(
    cc_brightness_timeseries(img, 9999),
    paste0(
      "You have selected 9999 frames per set, but there are only\\s?",
      "100,.+frames in total.+Please select less than 100 frames\\s?",
      "per set"
    )
  )
  expect_error(cc_brightness_timeseries(
    img %T>% {
      .[, , 1, ] <- NA
    }, 20
  ),
  paste0(
    "The first channel is all NAs.+Can't compute on an\\s?",
    "array of all NAs."
  ))
  expect_error(cc_brightness_timeseries(
    img %T>% {
      .[, , 2, ] <- NA
    }, 20
  ),
  paste0(
    "The second channel is all NAs.+Can't compute on an\\s?",
    "array of all NAs."
  ))
  expect_error(cc_brightness_timeseries(img, 20, thresh = max(img) + 1),
               paste0(
                 "After thresholding, the first channel is all\\s?",
                 "NAs.+Can't compute on an array of all NAs.+You need to\\s?",
                 "choose a less severe threshold for this.+channel."
               ))
  expect_error(cc_brightness_timeseries(img, 20, thresh = c(0, max(img) + 1)),
               paste0(
                 "After thresholding, the second channel is all\\s?",
                 "NAs.+Can't compute on an array of all NAs.+You need to\\s?",
                 "choose a less severe threshold for this.+channel."
               ))
})


context("cc_brightness_folder()")
test_that("cc_brightness_folder() works", {
  img <- ijtiff::read_tif(system.file("extdata", "two_ch.tif",
    package = "nandb"
  ))
  cwd <- setwd(tempdir())
  on.exit(setwd(cwd))
  ijtiff::write_tif(img, "a.tif")
  set.seed(1)
  cc_brightness_folder(thresh = "Huang", detrend = FALSE)
  cc_b <- dir(pattern = "cc_brightness", recursive = TRUE) %>%
    ijtiff::read_tif()
  expect_equal(median(cc_b, na.rm = TRUE), 0.02, tolerance = 0.002)
  filesstrings::dir.remove("cc_brightness")
  dir.create("dir")
  ijtiff::write_tif(img, "dir/a.tif")
  set.seed(1)
  cc_brightness_file("dir/a.tif", thresh = "Huang", detrend = FALSE)
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
    package = "nandb"
  ))
  cwd <- setwd(tempdir())
  on.exit(setwd(cwd))
  ijtiff::write_tif(img, "a.tif")
  set.seed(1)
  cc_brightness_timeseries_folder(
    thresh = "Huang", frames_per_set = 10, detrend = FALSE,
    filt = "median", parallel = 2
  )
  cc_b_ts <- c("cc_brightness_contiguous_timeseries",
               "cc_brightness_overlapped_timeseries") %>%
    purrr::map(~dir(pattern = paste0(., ".*tif$"), recursive = TRUE)) %>%
    unlist() %>%
    ijtiff::read_tif()
  expect_equal(median(cc_b_ts, na.rm = TRUE), 0, tolerance = 0.001)
  filesstrings::dir.remove("cc_brightness_timeseries")
  dir.create("dir")
  ijtiff::write_tif(img, "dir/a.tif")
  set.seed(1)
  cc_brightness_timeseries_file("dir/a.tif",
    thresh = "Huang", detrend = FALSE,
    frames_per_set = 10,
    filt = "median", parallel = 2
  )
  cc_b_ts <- c("cc_brightness_contiguous_timeseries",
               "cc_brightness_overlapped_timeseries") %>%
    purrr::map(~dir(pattern = paste0(., ".*tif$"), recursive = TRUE)) %>%
    unlist() %>%
    ijtiff::read_tif()
  expect_equal(median(cc_b_ts, na.rm = TRUE), 0, tolerance = 0.001)
  filesstrings::dir.remove("dir", "cc_brightness_timeseries")
  file.remove(dir(pattern = "tif$"))
  setwd(cwd)
})

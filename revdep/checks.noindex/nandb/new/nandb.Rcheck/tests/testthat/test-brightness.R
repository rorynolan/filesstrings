context("brightness()")
test_that("brightness works", {
  set.seed(1)
  img <- system.file("extdata", "50.tif", package = "nandb")
  brightness <- brightness(img, "e",
    thresh = "Huang", detrend = TRUE,
    filt = "median", parallel = 2
  )
  expect_equal(mean(brightness, na.rm = TRUE), 0.028, tolerance = 0.011)
  brightness <- brightness(img, "b", thresh = "Huang", filt = "mean")
  expect_equal(mean(brightness, na.rm = TRUE), 1.05, tolerance = 0.01)
  img %<>% ijtiff::read_tif()
  brightness <- brightness(img, "B", detrend = FALSE, filt = "median")
  skip_if_not_installed("abind")
  two_channel_img <- abind::abind(img, img, along = 3)
  brightness_2ch <- brightness(two_channel_img, "B",
    detrend = FALSE,
    filt = "median"
  )
  expect_equal(brightness_2ch %>% {
    list(dim(.), as.vector(.))
  },
  abind::abind(brightness, brightness, along = 3) %>% {
    list(dim(.), as.vector(.))
  },
  check.attributes = FALSE
  )
  expect_error(
    brightness(img, def = "rory"),
    paste0(
      "`def` must be one of 'B' or 'epsilon'.+You have used\\s?",
      "`def = 'rory'`."
    )
  )
})

context("brightness_folder()")
test_that("brightness_folder works", {
  set.seed(1)
  img <- ijtiff::read_tif(system.file("extdata", "50.tif", package = "nandb"))
  cwd <- setwd(tempdir())
  on.exit(setwd(cwd))
  ijtiff::write_tif(img, "50.tif")
  ijtiff::write_tif(img, "50again.tif")
  ijtiff::write_tif(array(4, dim = rep(4, 4)), "const.tif")
  brightness_folder(def = "b", detrend = FALSE)
  expect_true(
    all(c(
      "50_brightness_B_swaps=NA_thresh=NA_filt=NA.tif",
      "50again_brightness_B_swaps=NA_thresh=NA_filt=NA.tif",
      paste0(
        "const_brightness_B_swaps=NA,NA,NA,NA_",
        "thresh=NA,NA,NA,NA_filt=NA,NA,NA,NA.tif"
      )
    ) %in%
      list.files("brightness"))
  )
  brightness_folder(def = "E", detrend = FALSE)
  expect_true(
    all(c(
      "50_brightness_epsilon_swaps=NA_thresh=NA_filt=NA.tif",
      "50again_brightness_epsilon_swaps=NA_thresh=NA_filt=NA.tif",
      paste0(
        "const_brightness_epsilon_swaps=NA,NA,NA,NA_",
        "thresh=NA,NA,NA,NA_filt=NA,NA,NA,NA.tif"
      )
    ) %in%
      list.files("brightness"))
  )
  filesstrings::create_dir("tempwithintemp")
  ijtiff::write_tif(img, "tempwithintemp/50.tif")
  brightness_file("tempwithintemp/50.tif", def = "b")
  expect_true(any(stringr::str_detect(
    dir("tempwithintemp/brightness"),
    "^50_brightness_B.*tif$"
  )))
  brightness_file("tempwithintemp/50.tif", def = "E")
  expect_true(any(stringr::str_detect(
    dir("tempwithintemp/brightness"),
    "^50_brightness_epsilon.*tif$"
  )))
  filesstrings::dir.remove("tempwithintemp")
  filesstrings::dir.remove("brightness")
  suppressWarnings(file.remove(list.files()))
  setwd(cwd)
})

context("brightness_timeseries()")
test_that("brightness_timeseries works", {
  set.seed(1)
  img <- system.file("extdata", "50.tif", package = "nandb")
  b <- brightness(img, "e")
  bts <- brightness_timeseries(img, "e", 20,
    thresh = "Huang", detrend = FALSE, filt = "median"
  )
  expect_equal(mean(bts, na.rm = TRUE), -0.013, tolerance = 0.005)
  bts_overlapped <- brightness_timeseries(img, "e", 20,
    overlap = TRUE,
    thresh = "Huang", detrend = FALSE, filt = "median"
  )
  img %<>% ijtiff::read_tif()
  common_frames <- which(seq_len(dim(img)[4]) %% 20 == 0) - 20 + 1
  expect_equal(
    bts_overlapped[, , , common_frames, drop = FALSE] %>% {
      list(dim(.), as.vector(.))
    },
    bts %>% {
      list(dim(.), as.vector(.))
    }
  )
  bts_overlapped <- brightness_timeseries(img, "e", dim(img)[4],
    overlap = TRUE
  )
  expect_equal(
    bts_overlapped %>% {
      list(dim(.), as.vector(.))
    },
    b %>% {
      list(dim(.), as.vector(.))
    }
  )
  expect_equal(median(bts, na.rm = TRUE), median(bts_overlapped, na.rm = TRUE),
    tolerance = min(abs(c(
      median(bts, na.rm = TRUE),
      median(bts_overlapped, na.rm = TRUE)
    ))) / 10
  )
  bts <- brightness_timeseries(img, "B", 30,
    detrend = TRUE,
    thresh = "tri", filt = "median"
  )
  expect_equal(mean(bts, na.rm = TRUE), 1.01, tolerance = 0.006)
  expect_error(
    brightness_timeseries(img, "b", 51),
    paste0(
      "You have selected 51 frames per set, but there are only\\s?",
      "50,.+frames in total.+Please select less than 50 frames per\\s?",
      "set"
    )
  )
  bts <- brightness_timeseries(img, "b", 10, detrend = FALSE)
  skip_if_not_installed("abind")
  two_channel_img <- abind::abind(img, img, along = 3)
  bts_2ch <- brightness_timeseries(two_channel_img, "b", 10, detrend = FALSE)
  expect_equal(
    bts_2ch %>% {
      list(dim(.), as.vector(.))
    },
    abind::abind(bts, bts, along = 3) %>% {
      list(dim(.), as.vector(.))
    }
  )
  b <- brightness(two_channel_img, "b")
  bts_2ch <- brightness_timeseries(two_channel_img, "b",
    dim(two_channel_img)[4],
    detrend = FALSE
  )
  expect_equal(
    bts_2ch %>% {
      list(dim(.), as.vector(.))
    },
    b %>% {
      list(dim(.), as.vector(.))
    }
  )
  expect_error(
    brightness_timeseries(matrix(1:4, nrow = 2)),
    "argument.*def.*is missing, with no default"
  )
  cwd <- setwd(tempdir())
  on.exit(setwd(cwd))
  ijtiff::write_tif(img, "50.tif")
  ijtiff::write_tif(img, "50again.tif")
  context("brightness_timeseries_folder()")
  filesstrings::create_dir("tempwithintemp")
  ijtiff::write_tif(img, "tempwithintemp/50.tif")
  brightness_timeseries_file("tempwithintemp/50.tif",
    def = "b",
    frames_per_set = 10
  )
  expect_true(any(stringr::str_detect(
    dir("tempwithintemp/brightness_timeseries"),
    "^50_brightness_B_contiguous_timeseries.*tif$"
  )))
  brightness_timeseries_file("tempwithintemp/50.tif",
    def = "E",
    frames_per_set = 10
  )
  expect_true(any(stringr::str_detect(
    dir("tempwithintemp/brightness_timeseries"),
    "^50_brightness_epsilon_contiguous_timeseries.*tif$"
  )))
  filesstrings::dir.remove("tempwithintemp")
  set.seed(1)
  brightness_timeseries_folder(def = "b", thresh = "tri", frames_per_set = 20)
  expect_true(dir.exists("brightness_timeseries"))
  expect_gt(length(dir("brightness_timeseries")), 0)
  expect_equal(
    sum(
      purrr::map_lgl(
        list.files("brightness_timeseries"),
        ~ any(stringr::str_detect(
          .,
          paste0(
            "50",
            c(
              "_brightness_B_contiguous_timeseries_",
              "again_brightness_B_contiguous_timeseries_"
            ),
            c(
              "frames_per_set=20_swaps=NA_thresh=Triangle=0.68_filt=NA.tif",
              "frames_per_set=20_swaps=NA_thresh=Triangle=0.68_filt=NA.tif"
            )
          )
        ))
      )
    ),
    2
  )
  brightness_timeseries_folder(def = "E", thresh = "tri", frames_per_set = 20)
  expect_true(dir.exists("brightness_timeseries"))
  expect_gt(length(dir("brightness_timeseries")), 0)
  expect_equal(
    sum(
      purrr::map_lgl(
        list.files("brightness_timeseries"),
        ~ any(stringr::str_detect(
          .,
          paste0(
            "50",
            c(
              "_brightness_epsilon_contiguous_timeseries_",
              "again_brightness_epsilon_contiguous_timeseries_"
            ),
            c(
              "frames_per_set=20_swaps=NA_thresh=Triangle=0.68_filt=NA.tif",
              "frames_per_set=20_swaps=NA_thresh=Triangle=0.68_filt=NA.tif"
            )
          )
        ))
      )
    ),
    2
  )
  filesstrings::dir.remove("brightness_timeseries")
  suppressWarnings(file.remove(list.files())) # cleanup
  setwd(cwd)
})

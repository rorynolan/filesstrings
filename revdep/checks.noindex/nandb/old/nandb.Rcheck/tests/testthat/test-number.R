test_that("number works", {
  set.seed(1)
  context("number()")
  img <- system.file("extdata", "50.tif", package = "nandb")
  number <- number(img, "n", thresh = "Huang", filt = "median", detrend = TRUE)
  expect_equal(median(number, na.rm = TRUE), 0, tolerance = 20)
  number <- number(img, "N", thresh = "Huang", filt = "mean")
  expect_equal(mean(number, na.rm = TRUE), 17.4, tolerance = 0.01)
  img %<>% ijtiff::read_tif()
  number <- number(img, "n", filt = "median", detrend = FALSE)
  skip_if_not_installed("abind")
  two_channel_img <- abind::abind(img, img, along = 3)
  number_2ch <- number(two_channel_img, "n", filt = "median", detrend = FALSE)
  expect_equal(number_2ch %>% {
    list(dim(.), as.vector(.))
  },
  abind::abind(number, number, along = 3) %>% {
    list(dim(.), as.vector(.))
  },
  check.attributes = FALSE
  )
  expect_error(
    number(img, def = "rory"),
    paste0(
      "`def` must be one of 'N' or 'n.+You have used `def =\\s?",
      "'rory'`."
    )
  )
})

test_that("number_folder works", {
  set.seed(1)
  context("number_folder()")
  img <- ijtiff::read_tif(system.file("extdata", "50.tif", package = "nandb"))
  cwd <- setwd(tempdir())
  on.exit(setwd(cwd))
  ijtiff::write_tif(img, "50.tif")
  ijtiff::write_tif(img, "50again.tif")
  ijtiff::write_tif(array(4, dim = rep(3, 4)), "const.tif")
  expect_error(
    number_folder(def = "rory", detrend = FALSE),
    paste0(
      "`def` must be one of 'N' or 'n.+You have used `def =\\s?",
      "'rory'`."
    )
  )
  number_folder(def = "N", detrend = FALSE)
  expect_true(
    all(c(
      "50_number_N_swaps=NA_thresh=NA_filt=NA.tif",
      "50again_number_N_swaps=NA_thresh=NA_filt=NA.tif",
      paste0(
        "const_number_N_swaps=NA,NA,NA_",
        "thresh=NA,NA,NA_filt=NA,NA,NA.tif"
      )
    ) %in%
      list.files("number"))
  )
  filesstrings::create_dir("tempwithintemp")
  ijtiff::write_tif(img, "tempwithintemp/50.tif")
  expect_error(
    number_file("tempwithintemp/50.tif", def = "rory"),
    paste0(
      "`def` must be one of 'N' or 'n.+You have used `def =\\s?",
      "'rory'`."
    )
  )
  number_file("tempwithintemp/50.tif", def = "n")
  expect_true(stringr::str_detect(
    dir("tempwithintemp/number"),
    "^50_number_n.*tif$"
  ))
  filesstrings::dir.remove("tempwithintemp", "number")
  suppressWarnings(file.remove(list.files()))
  setwd(cwd)
})

test_that("number_timeseries works", {
  set.seed(1)
  context("number_timeseries()")
  img <- system.file("extdata", "50.tif", package = "nandb")
  n <- number(img, "N")
  expect_error(
    number_timeseries(img, def = "rory", 10),
    paste0(
      "`def` must be one of 'N' or 'n.+You have used `def =\\s?",
      "'rory'`."
    )
  )
  nts <- number_timeseries(img, "N", 20, thresh = "Huang")
  expect_equal(mean(nts, na.rm = TRUE), 18, tolerance = 2)
  nts_overlapped <- number_timeseries(img, "N", 20,
    overlap = TRUE,
    thresh = "Huang", detrend = FALSE
  )
  img %<>% ijtiff::read_tif()
  common_frames <- which(seq_len(dim(img)[4]) %% 20 == 0) - 20 + 1
  expect_equal(
    nts_overlapped[, , , common_frames, drop = FALSE] %>% {
      list(dim(.), as.vector(.))
    },
    nts %>% {
      list(dim(.), as.vector(.))
    }
  )
  nts_overlapped <- number_timeseries(img, "N", dim(img)[4],
    overlap = TRUE
  )
  expect_equal(
    nts_overlapped %>% {
      list(dim(.), as.vector(.))
    },
    n %>% {
      list(dim(.), as.vector(.))
    }
  )
  expect_equal(median(nts, na.rm = TRUE), median(nts_overlapped, na.rm = TRUE),
    tolerance = min(abs(c(
      median(nts, na.rm = TRUE),
      median(nts_overlapped, na.rm = TRUE)
    ))) / 10
  )
  nts <- number_timeseries(img, "n", 30,
    detrend = FALSE,
    thresh = "tri", filt = "median"
  )
  expect_equal(median(nts, na.rm = TRUE), -2, tolerance = 2)
  expect_error(
    number_timeseries(img, "n", 51),
    paste0(
      "You have selected 51 frames per set, but there are only\\s?",
      "50,.+frames in total.+Please select less than 50 frames per\\s?",
      "set"
    )
  )
  nts <- number_timeseries(img, "N", 10, detrend = FALSE)
  skip_if_not_installed("abind")
  two_channel_img <- abind::abind(img, img, along = 3)
  nts_2ch <- number_timeseries(two_channel_img, "N", 10, detrend = FALSE)
  expect_equal(
    nts_2ch %>% {
      list(dim(.), as.vector(.))
    },
    abind::abind(nts, nts, along = 3) %>% {
      list(dim(.), as.vector(.))
    }
  )
  n <- number(two_channel_img, "n")
  nts_2ch <- number_timeseries(two_channel_img, "n",
    dim(two_channel_img)[4],
    detrend = FALSE
  )
  expect_equal(
    nts_2ch %>% {
      list(dim(.), as.vector(.))
    },
    n %>% {
      list(dim(.), as.vector(.))
    }
  )
  expect_error(
    number_timeseries(matrix(1:4, nrow = 2)),
    "argument.*def.*is missing, with no default"
  )
  cwd <- setwd(tempdir())
  on.exit(setwd(cwd))
  ijtiff::write_tif(img, "50.tif")
  ijtiff::write_tif(img, "50again.tif")
  filesstrings::create_dir("tempwithintemp")
  ijtiff::write_tif(img, "tempwithintemp/50.tif")
  expect_error(
    number_timeseries_file("tempwithintemp/50.tif",
      def = "rory",
      frames_per_set = 10
    ),
    paste0(
      "`def` must be one of 'N' or 'n.+You have used `def =\\s?",
      "'rory'`."
    )
  )
  number_timeseries_file("tempwithintemp/50.tif",
    def = "n",
    frames_per_set = 10
  )
  expect_true(stringr::str_detect(
    dir("tempwithintemp/number_timeseries"),
    "^50_number_n_contiguous_timeseries.*tif$"
  ))
  filesstrings::dir.remove("tempwithintemp")
  expect_error(
    number_timeseries_folder(
      def = "rory", thresh = "tri", frames_per_set = 20,
      detrend = TRUE
    ),
    paste0(
      "`def` must be one of 'N' or 'n.+You have used `def =\\s?",
      "'rory'`."
    )
  )
  number_timeseries_folder(
    def = "n", thresh = "tri", frames_per_set = 20,
    detrend = TRUE
  )
  skip_on_cran()
  ans0 <- paste0(
    "50",
    c(
      "_number_n_contiguous_timeseries_",
      "again_number_n_contiguous_timeseries_"
    ),
    c(
      "frames_per_set=20_swaps=auto=0_thresh=Triangle=0.68_filt=NA",
      "frames_per_set=20_swaps=auto=5029_thresh=Triangle=0.68_filt=NA"
    ),
    ".tif"
  )
  ans1 <- stringr::str_replace(ans0, "5029", "5363") # travis ubuntu
  ans2 <- stringr::str_replace(ans0, "5029", "862") # windows
  ans3 <- stringr::str_replace(
    ans0,
    c("auto=0", "auto=5029"),
    c("auto=1600", "auto=3840")
  ) # rhub fedora
  ans4 <- stringr::str_replace(
    ans0,
    c("auto=0", "auto=5029"),
    c("auto=5363", "auto=0")
  ) # rhub ubuntu
  ans5 <- stringr::str_replace(ans0, "5029", "0") # Appveyor R 4.0.0
  lfnts <- list.files("number_timeseries")
  if (all(lfnts %in% ans0)) {
    expect_true(all(lfnts %in% ans0))
  } else if (all(lfnts %in% ans1)) {
    expect_equal(lfnts, ans1)
  } else if (all(lfnts %in% ans2)) {
    expect_true(all(lfnts %in% ans2))
  } else if (all(lfnts %in% ans3)) {
    expect_true(all(lfnts %in% ans3))
  } else if (all(lfnts %in% ans4)) {
    expect_true(all(lfnts %in% ans4))
  } else if (all(lfnts %in% ans5)) {
    expect_true(all(lfnts %in% ans5))
  } else if (getRversion() >= "3.6") {
    expect_equal(sort(lfnts), sort(ans0))
  }
  suppressWarnings(file.remove(list.files(pattern = "\\.tiff*"))) # cleanup
  filesstrings::dir.remove("number_timeseries", "tempwithintemp")
  setwd(cwd)
})

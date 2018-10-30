context("brightness()")
test_that("brightness works", {
  set.seed(1)
  img <- system.file('extdata', '50.tif', package = 'nandb')
  brightness <- brightness(img, "e", tau = 'auto', thresh = 'Huang',
                           filt = 'median', parallel = 2)
  expect_equal(mean(brightness, na.rm = TRUE), 0.028, tolerance = 0.011)
  brightness <- brightness(img, "B", tau = 1000, thresh = 'Huang',
                           filt = 'mean')
  expect_equal(mean(brightness, na.rm = TRUE), 1.05, tolerance = 0.01)
  expect_error(brightness(img, "e", tau = "abc"), "If `tau` is a string")
  expect_error(brightness(img, "B", tau = FALSE),
               "`tau` must be specified as a positive number or")
  img %<>% ijtiff::read_tif()
  brightness <- brightness(img, "B", filt = "median")
  skip_if_not_installed("abind")
  two_channel_img <- abind::abind(img, img, along = 3)
  brightness_2ch <- brightness(two_channel_img, "B", filt = "median")
  expect_equal(brightness_2ch %>% {list(dim(.), as.vector(.))},
               abind::abind(brightness, brightness, along = 3) %>%
                 {list(dim(.), as.vector(.))},
               check.attributes = FALSE)
})

context("brightness_folder()")
test_that("brightness_folder works", {
  set.seed(1)
  img <- ijtiff::read_tif(system.file('extdata', '50.tif', package = 'nandb'))
  cwd <- getwd()
  on.exit(setwd(cwd))
  setwd(tempdir())
  ijtiff::write_tif(img, '50.tif')
  ijtiff::write_tif(img, '50again.tif')
  ijtiff::write_tif(array(4, dim = rep(4, 4)), "const.tif")
  brightness_folder(def = "B", tau = NA)
  expect_true(
    all(c("50_brightness_B_tau=NA_thresh=NA_filt=NA.tif",
          "50again_brightness_B_tau=NA_thresh=NA_filt=NA.tif",
          paste0("const_brightness_B_tau=NA,NA,NA,NA_",
                 "thresh=NA,NA,NA,NA_filt=NA,NA,NA,NA.tif")) %in%
          list.files("brightness"))
  )
  filesstrings::create_dir("tempwithintemp")
  ijtiff::write_tif(img, 'tempwithintemp/50.tif')
  brightness_file("tempwithintemp/50.tif", def = "B")
  expect_true(stringr::str_detect(dir("tempwithintemp/brightness"),
                                  "^50_brightness_B.*tif$"))
  filesstrings::dir.remove("tempwithintemp")
  suppressWarnings(file.remove(list.files()))
})

context("brightness_timeseries()")
test_that("brightness_timeseries works", {
  set.seed(1)
  img <- system.file('extdata', '50.tif', package = 'nandb')
  bts <- brightness_timeseries(img, "e", 20, tau = 100, thresh = 'Huang',
                                filt = 'median', parallel = 2)
  expect_equal(mean(bts, na.rm = TRUE), -0.013, tolerance = 0.005)
  bts <- brightness_timeseries(img, "B", 30, tau = NA, thresh = 'tri',
                                filt = 'median', parallel = 2)
  expect_equal(mean(bts, na.rm = TRUE), 1.012, tolerance = 0.005)
  expect_error(brightness_timeseries(img, "b", 51),
               paste("You have selected 51 frames per set,",
                     "but there are only 50 frames in total"))
  img %<>% ijtiff::read_tif()
  bts <- brightness_timeseries(img, "b", 10)
  skip_if_not_installed("abind")
  two_channel_img <- abind::abind(img, img, along = 3)
  bts_2ch <- brightness_timeseries(two_channel_img, "b", 10)
  expect_equal(bts_2ch %>% {list(dim(.), as.vector(.))},
               abind::abind(bts, bts, along = 3) %>%
                 {list(dim(.), as.vector(.))})
  expect_error(brightness_timeseries(matrix(1:4, nrow = 2)),
               "argument.*def.*is missing, with no default")
  cwd <- getwd()
  on.exit(setwd(cwd))
  setwd(tempdir())
  ijtiff::write_tif(img, '50.tif')
  ijtiff::write_tif(img, '50again.tif')
  context("brightness_timeseries_folder()")
  filesstrings::create_dir("tempwithintemp")
  ijtiff::write_tif(img, 'tempwithintemp/50.tif')
  brightness_timeseries_file("tempwithintemp/50.tif", def = "B",
                              frames_per_set = 10)
  expect_true(stringr::str_detect(
                dir("tempwithintemp/brightness_timeseries"),
                "^50_brightness_B_timeseries.*tif$"))
  filesstrings::dir.remove("tempwithintemp")
  set.seed(1)
  brightness_timeseries_folder(def = "B", tau = 333, thresh = 'tri',
                                frames_per_set = 20)
  expect_true(all(
    paste0("50", c("_brightness_B_timeseries_",
                   "again_brightness_B_timeseries_"),
           c("frames=20_tau=333_thresh=Triangle=0.68_filt=NA.tif",
             "frames=20_tau=333_thresh=Triangle=0.68_filt=NA.tif")) %in%
      list.files("brightness_timeseries")))
  suppressWarnings(file.remove(list.files()))  # cleanup
})



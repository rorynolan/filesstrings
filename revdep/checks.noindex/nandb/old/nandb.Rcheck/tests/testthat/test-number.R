test_that("number works", {
  set.seed(1)
  context("number()")
  img <- system.file('extdata', '50.tif', package = 'nandb')
  number <- number(img, "n", tau = 'auto', thresh = 'Huang',
                           filt = 'median', parallel = 2)
  expect_equal(median(number, na.rm = TRUE), 0, tolerance = 20)
  number <- number(img, "N", tau = 1000, thresh = 'Huang',
                   filt = 'mean')
  expect_equal(mean(number, na.rm = TRUE), 17.4, tolerance = 0.01)
  expect_error(number(img, "n", tau = "abc"), "If `tau` is a string")
  expect_error(number(img, "n", tau = FALSE),
               "`tau` must be specified as a positive number or")
  img %<>% ijtiff::read_tif()
  number <- number(img, "n", filt = "median")
  skip_if_not_installed("abind")
  two_channel_img <- abind::abind(img, img, along = 3)
  number_2ch <- number(two_channel_img, "n", filt = "median")
  expect_equal(number_2ch %>% {list(dim(.), as.vector(.))},
               abind::abind(number, number, along = 3) %>%
               {list(dim(.), as.vector(.))},
               check.attributes = FALSE)
})

test_that("number_folder works", {
  set.seed(1)
  context("number_folder()")
  img <- ijtiff::read_tif(system.file('extdata', '50.tif', package = 'nandb'))
  cwd <- getwd()
  on.exit(setwd(cwd))
  setwd(tempdir())
  ijtiff::write_tif(img, '50.tif')
  ijtiff::write_tif(img, '50again.tif')
  ijtiff::write_tif(array(4, dim = rep(3, 4)), "const.tif")
  number_folder(def = "N", tau = NA)
  expect_true(
    all(c("50_number_N_tau=NA_thresh=NA_filt=NA.tif",
          "50again_number_N_tau=NA_thresh=NA_filt=NA.tif",
          paste0("const_number_N_tau=NA,NA,NA_",
                 "thresh=NA,NA,NA_filt=NA,NA,NA.tif")) %in%
          list.files("number"))
  )
  filesstrings::create_dir("tempwithintemp")
  ijtiff::write_tif(img, 'tempwithintemp/50.tif')
  number_file("tempwithintemp/50.tif", def = "n")
  expect_true(stringr::str_detect(dir("tempwithintemp/number"),
                                  "^50_number_n.*tif$"))
  filesstrings::dir.remove("tempwithintemp")
  suppressWarnings(file.remove(list.files()))
})

test_that("number_timeseries works", {
  set.seed(1)
  context("number_timeseries()")
  library(magrittr)
  img <- system.file('extdata', '50.tif', package = 'nandb')
  nts <- number_timeseries(img, "N", 20, tau = 100, thresh = 'Huang',
                            filt = 'median', parallel = 2)
  expect_equal(round(mean(nts, na.rm = TRUE)), 18)
  nts <- number_timeseries(img, "n", 30, tau = NA, thresh = 'tri',
                            filt = 'median', parallel = 2)
  expect_equal(median(nts, na.rm = TRUE), -2, tolerance = 2)
  expect_error(number_timeseries(img, "n", 51),
               paste("You have selected 51 frames per set,",
                     "but there are only 50 frames in total"))
  img <- ijtiff::read_tif(img)
  nts <- number_timeseries(img, "N", 10)
  skip_if_not_installed("abind")
  two_channel_img <- abind::abind(img, img, along = 3)
  nts_2ch <- number_timeseries(two_channel_img, "N", 10)
  expect_equal(nts_2ch %>% {list(dim(.), as.vector(.))},
               abind::abind(nts, nts, along = 3) %>%
               {list(dim(.), as.vector(.))})
  expect_error(number_timeseries(matrix(1:4, nrow = 2)),
               "argument.*def.*is missing, with no default")
  setwd(tempdir())
  ijtiff::write_tif(img, '50.tif')
  ijtiff::write_tif(img, '50again.tif')
  filesstrings::create_dir("tempwithintemp")
  ijtiff::write_tif(img, 'tempwithintemp/50.tif')
  number_timeseries_file("tempwithintemp/50.tif", def = "n",
                          frames_per_set = 10)
  expect_true(stringr::str_detect(dir("tempwithintemp/number_timeseries"),
                                  "^50_number_n_timeseries.*tif$"))
  filesstrings::dir.remove("tempwithintemp")
  number_timeseries_folder(def = "n", tau = 333, thresh = 'tri',
                                frames_per_set = 20)
  expect_true(all(
    paste0("50", c("_number_n_timeseries_",
                   "again_number_n_timeseries_"),
           c("frames=20_tau=333_thresh=Triangle=0.68_filt=NA.tif",
             "frames=20_tau=333_thresh=Triangle=0.68_filt=NA.tif")) %in%
      list.files("number_timeseries")))
  suppressWarnings(file.remove(list.files()))  # cleanup
})

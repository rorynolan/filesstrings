context("Detrend directories")

test_that("detrending entire derectories works", {
  cwd <- setwd(tempdir())
  on.exit(setwd(cwd))
  orig_files <- c(
    system.file("img", "2ch_ij.tif", package = "ijtiff"),
    system.file("extdata", "bleached.tif", package = "detrendr")
  )
  file.copy(orig_files, ".")
  orig_imgs <- purrr::map(orig_files, ijtiff::read_tif, msg = FALSE)
  detrendeds <- try(stop("eee"), silent = TRUE)
  set.seed(1)
  while (class(detrendeds) == "try-error") {
    set.seed(get_seed())
    detrendeds <- tryCatch(purrr::map(orig_imgs,
      autothresholdr::mean_stack_thresh,
      method = "tri"
    ) %>%
      purrr::map(img_detrend_boxcar,
        l = "auto",
        purpose = "ff"
      ),
    error = function(e) {
      err_msg <- conditionMessage(e)
      if (!stringr::str_detect(
        err_msg,
        "Even.*most severe d"
      )) {
        stop(err_msg)
      } else {
        structure(err_msg, class = "try-error")
      }
    }
    )
  }
  eee <- try(stop("eee"), silent = TRUE)
  set.seed(1)
  while (class(eee) == "try-error") {
    set.seed(get_seed())
    eee <- tryCatch(dir_detrend_boxcar(
      l = "auto", thresh = "tri",
      purpose = "ff", msg = FALSE
    ),
    error = function(e) {
      err_msg <- conditionMessage(e)
      if (!stringr::str_detect(
        err_msg,
        "Even .* most severe detrend"
      )) {
        stop("unexpected error")
      } else {
        structure(err_msg, class = "try-error")
      }
    }
    )
  }
  detrendeds_dir <- dir(pattern = "detrended.*tif", recursive = TRUE) %>%
    purrr::map(ijtiff::read_tif, msg = FALSE)
  expect_equal(purrr::map(detrendeds, dim), purrr::map(detrendeds_dir, dim))
  expect_equal(unlist(detrendeds), unlist(detrendeds_dir))
  if (get_os() == "mac") {
    expect_equal(
      dir("detrended"),
      paste0(
        c(
          "2ch_ij_detrended_thresh=Triangle=0.6,Triangle=0.6_",
          "bleached_detrended_thresh=Triangle=41.622_"
        ),
        c(
          "boxcar_for_FFS_l=auto=NA,auto=2.tif",
          "boxcar_for_FFS_l=auto=19.tif"
        )
      )
    )
  }
  filesstrings::dir.remove("detrended")
  set.seed(1)
  detrendeds <- purrr::map(orig_imgs, autothresholdr::mean_stack_thresh,
    method = "tri"
  ) %>%
    purrr::map(img_detrend_exp, tau = "auto", purpose = "ff")
  set.seed(1)
  dir_detrend_exp(
    tau = "auto", thresh = "tri", purpose = "ff",
    msg = FALSE
  )
  detrendeds_dir <- dir(pattern = "detrended.*tif", recursive = TRUE) %>%
    purrr::map(ijtiff::read_tif, msg = FALSE)
  expect_equal(purrr::map(detrendeds, dim), purrr::map(detrendeds_dir, dim))
  expect_equal(unlist(detrendeds), unlist(detrendeds_dir))
  if (get_os() == "mac") {
    expect_equal(
      dir("detrended"),
      paste0(
        c(
          "2ch_ij_detrended_thresh=Triangle=0.6,Triangle=0.6_",
          "bleached_detrended_thresh=Triangle=41.622_"
        ),
        "exponential_",
        c(
          "for_FFS_tau=auto=NA,auto=6.1767578125.tif",
          "for_FFS_tau=auto=20.703125.tif"
        )
      )
    )
  }
  filesstrings::dir.remove("detrended")
  set.seed(1)
  detrendeds <- purrr::map(orig_imgs, autothresholdr::mean_stack_thresh,
    method = "tri"
  ) %>%
    purrr::map(~suppressWarnings(img_detrend_polynom(.,
      degree = 2,
      purpose = "ff"
    )))
  set.seed(1)
  suppressWarnings(dir_detrend_polynom(
    degree = 2, thresh = "tri",
    purpose = "ff", msg = FALSE
  ))
  detrendeds_dir <- dir(pattern = "detrended.*tif", recursive = TRUE) %>%
    purrr::map(ijtiff::read_tif, msg = FALSE)
  expect_equal(purrr::map(detrendeds, dim), purrr::map(detrendeds_dir, dim))
  expect_equal(unlist(detrendeds), unlist(detrendeds_dir))
  expect_equal(
    dir("detrended"),
    paste0(
      c(
        "2ch_ij_detrended_thresh=Triangle=0.6,Triangle=0.6_",
        "bleached_detrended_thresh=Triangle=41.622_"
      ),
      c(
        "polynomial_for_FFS_degree=2,2.tif",
        "polynomial_for_FFS_degree=2.tif"
      )
    )
  )
  filesstrings::dir.remove("detrended")
  set.seed(1)
  detrendeds <- purrr::map(orig_imgs, autothresholdr::mean_stack_thresh,
    method = "tri"
  ) %>%
    purrr::map(img_detrend_rh, swaps = 222)
  set.seed(1)
  dir_detrend_rh(swaps = 222, thresh = "tri", msg = FALSE)
  detrendeds_dir <- dir(pattern = "detrended.*tif$", recursive = TRUE) %>%
    purrr::map(ijtiff::read_tif, msg = FALSE)
  expect_equal(purrr::map(detrendeds, dim), purrr::map(detrendeds_dir, dim))
  expect_equal(unlist(detrendeds), unlist(detrendeds_dir))
  expect_equal(
    dir("detrended"),
    paste0(
      c(
        "2ch_ij_detrended_thresh=Triangle=0.6,Triangle=0.6_",
        "bleached_detrended_thresh=Triangle=41.622_"
      ),
      c(
        "robinhood_swaps=222,222.tif",
        "robinhood_swaps=222.tif"
      )
    )
  )
  filesstrings::dir.remove("detrended")
  file.remove(dir(pattern = "tif$"))
  setwd(cwd)
})

test_that("file_detrend() deals with other directories correctly", {
  setwd(tempdir())
  filesstrings::create_dir("tempwithintemp")
  file.copy(
    system.file("extdata", "bleached.tif", package = "detrendr"),
    "tempwithintemp"
  )
  file_detrend("tempwithintemp/bleached.tif",
    method = "exp", parameter = 5,
    purpose = "ff"
  )
})

test_that("`make_thresh_filename_part()` works", {
  expect_equal(
    make_thresh_filename_part(structure(0, thresh = 1)),
    "thresh=1_"
  )
})

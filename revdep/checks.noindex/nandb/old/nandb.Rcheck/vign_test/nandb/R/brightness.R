#' Calculate brightness from image series.
#'
#' Given a time stack of images, `brightness()` performs a calculation of the
#' brightness for each pixel.
#'
#' @param def A character. Which definition of brightness do you want to use,
#'   `"B"` or `"epsilon"`?
#' @param thresh The threshold or thresholding method (see
#'   [autothresholdr::mean_stack_thresh()]) to use on the image prior to
#'   detrending and brightness calculations.
#' @inheritParams detrendr::img_detrend_exp
#' @inheritParams number
#'
#' @return A matrix, the brightness image.
#'
#' @references Digman MA, Dalal R, Horwitz AF, Gratton E. Mapping the Number of
#'   Molecules and Brightness in the Laser Scanning Microscope. Biophysical
#'   Journal. 2008;94(6):2320-2332. \doi{10.1529/biophysj.107.114645}.
#'
#'   Dalal, RB, Digman, MA, Horwitz, AF, Vetri, V, Gratton, E (2008).
#'   Determination of particle number and brightness using a laser scanning
#'   confocal microscope operating in the analog mode. Microsc. Res. Tech., 71,
#'   1:69-81. \doi{10.1002/jemt.20526}.
#'
#' @examples
#' img <- ijtiff::read_tif(system.file('extdata', '50.tif', package = 'nandb'))
#' ijtiff::display(img[, , 1, 1])
#' b <- brightness(img, "e", thresh = "Huang")
#' b <- brightness(img, "B", thresh = "tri")
#' @export
brightness <- function(img, def, thresh = NULL, detrend = FALSE, quick = FALSE,
                       filt = NULL,
                       s = 1, offset = 0, readout_noise = 0,
                       parallel = FALSE) {
  checkmate::assert_string(def)
  if (startsWith("epsilon", tolower(def))) def <- "epsilon"
  if (def == "b") def <- "B"
  if (!def %in% c("epsilon", "B")) {
    custom_stop("`def` must be one of 'B' or 'epsilon'.",
                "You have used `def = '{def}'`.")
  }
  img %<>% nb_get_img()
  d <- dim(img)
  n_ch <- dplyr::if_else(length(d) == 3, 1L, d[3])
  if (n_ch == 1 && length(d) == 4) {
    dim(img) %<>% {
      .[-3]
    }
  }
  thresh %<>% extend_for_all_chs(n_ch)
  detrend %<>% extend_for_all_chs(n_ch)
  if (!is.null(filt)) filt %<>% fix_filt()
  filt %<>% extend_for_all_chs(n_ch) %>% unlist() %>% as.character()
  swaps_atts <- extend_for_all_chs(
    rlang::set_attrs(NA, auto = FALSE),
    n_ch
  )
  thresh_atts <- extend_for_all_chs(NA, n_ch)
  if (n_ch == 1) {
    if (!is.na(thresh[[1]])) {
      img %<>% autothresholdr::mean_stack_thresh(
        method = thresh[[1]],
        ignore_na = TRUE
      )
      thresh_atts <- attr(img, "thresh")
    }
    if (detrend[[1]]) {
      img %<>% detrendr::img_detrend_rh(quick = quick)
      swaps_atts <- attr(img, "parameter")
      attr(swaps_atts, "auto") <- attr(img, "auto")
    }
    out <- (detrendr::var_pillars(img, parallel = parallel) - readout_noise) /
      (detrendr::mean_pillars(img, parallel = parallel) - offset)
    if (def == "epsilon") {
      out %<>% {
        . / s - 1
      }
    }
    if (length(dim(out)) > 2) {
      dim(out) %<>% {
        .[1:2]
      }
    }
    if (!is.na(filt[[1]])) {
      checkmate::assert_string(filt)
      if (filt[[1]] == "median") {
        out %<>% median_filter(na_count = TRUE)
      } else {
        out %<>% smooth_filter(na_count = TRUE)
      }
    }
  } else {
    out <- img[, , , 1]
    thresh_atts <- list()
    swaps_atts <- list()
    for (i in seq_len(n_ch)) {
      out_i <- brightness(img[, , i, , drop = FALSE],
        def = def, thresh = thresh[[i]],
        detrend = detrend[[i]], quick = quick,
        filt = filt[[i]],
        s = s, offset = offset, readout_noise = readout_noise,
        parallel = parallel
      )
      out[, , i] <- out_i
      thresh_atts[[i]] <- attr(out_i, "thresh")
      swaps_atts[[i]] <- attr(out_i, "swaps")
    }
  }
  brightness_img(out,
                 def = def,
                 thresh = thresh_atts, swaps = swaps_atts, filt = filt)
}

#' Create a brightness time-series.
#'
#' Given a stack of images `img`, use the first `frames_per_set` of them to
#' create one brightness image, the next `frames_per_set` of them to create the
#' next brightness image and so on to get a time-series of brightness images.
#'
#' This may discard some images, for example if 175 frames are in the input and
#' `frames_per_set = 50`, then the last 25 are discarded. If detrending is
#' selected, it is performed on the whole image stack before the sectioning is
#' done for calculation of numbers.
#'
#' @param frames_per_set The number of frames with which to calculate the
#'   successive brightnesses.
#' @param overlap A boolean. If `TRUE`, the windows used to calculate number are
#'   overlapped, if `FALSE`, they are not. For example, for a 20-frame image
#'   series with 5 frames per set, if the windows are not overlapped, then the
#'   frame sets used are 1-5, 6-10, 11-15 and 16-20; whereas if they are
#'   overlapped, the frame sets are 1-5, 2-6, 3-7, 4-8 and so on up to 16-20.
#'
#' @inheritParams brightness
#' @inheritParams number
#'
#' @return An object of class [brightness_ts_img].
#'
#'   \itemize{\item If `img` is 3-dimensional (i.e. 1-channel), a 3-dimensional
#'   array `arr` is returned with `arr[y, x, t]` being pixel \eqn{(x, y)} of the
#'   \eqn{t}th brightness image in the brightness time series. \item If  `img`
#'   is 4-dimensional (i.e. 2-channel), a 4-dimensional array `arr` is returned
#'   with `arr[y, x, c, t]` being pixel \eqn{(x, y)} of the \eqn{c}th channel of
#'   the \eqn{t}th brightness image in the brightness time series.}
#'
#' @seealso [brightness()].
#'
#' @examples
#' img <- ijtiff::read_tif(system.file('extdata', '50.tif', package = 'nandb'))
#' bts <- brightness_timeseries(img, "e", frames_per_set = 20, thresh = "Huang")
#' @export
brightness_timeseries <- function(img, def, frames_per_set, overlap = FALSE,
                                  thresh = NULL, detrend = FALSE, quick = FALSE,
                                  filt = NULL,
                                  s = 1, offset = 0,
                                  readout_noise = 0, parallel = FALSE) {
  if (startsWith("epsilon", tolower(def))) def <- "epsilon"
  if (def == "b") def <- "B"
  img %<>% nb_get_img()
  d <- dim(img)
  n_ch <- dplyr::if_else(length(d) == 3, 1L, d[3])
  if (n_ch == 1 && length(d) == 4) {
    dim(img) %<>% {
      .[-3]
    }
  }
  thresh %<>% extend_for_all_chs(n_ch)
  detrend %<>% extend_for_all_chs(n_ch)
  if (!is.null(filt)) filt %<>% fix_filt()
  filt %<>% extend_for_all_chs(n_ch) %>% unlist() %>% as.character()
  swaps_atts <- extend_for_all_chs(
    rlang::set_attrs(NA, auto = FALSE),
    n_ch
  )
  thresh_atts <- extend_for_all_chs(NA, n_ch)
  if (n_ch == 1) {
    frames <- dim(img)[3]
    if (frames < frames_per_set) {
      custom_stop("
        You have selected {frames_per_set} frames per set,
        but there are only {frames}, frames in total.
        ", "
        Please select less than {frames} frames per set.
        "
      )
    }
    if (!is.na(thresh[[1]])) {
      img %<>% autothresholdr::mean_stack_thresh(
        method = thresh[[1]],
        ignore_na = TRUE
      )
      thresh_atts <- attr(img, "thresh")
      img <- img[, , 1, ]
    }
    if (detrend[[1]]) {
      img %<>% detrendr::img_detrend_rh(quick = quick)
      swaps_atts <- attr(img, "parameter")
      attr(swaps_atts, "auto") <- attr(img, "auto")
      img <- img[, , 1, ]
    }
    if (overlap) {
      sets <- frames - frames_per_set + 1
      out <- img[, , seq_len(sets)]
      if (sets == 1) dim(out) %<>% c(1)
      for (i in seq_len(sets)) {
        indices_i <- seq(i, i + frames_per_set - 1)
        out[, , i] <- brightness(img[, , indices_i],
                                 def = def, detrend = FALSE, filt = filt,
                                 s = s, offset = offset,
                                 readout_noise = readout_noise,
                                 parallel = parallel
        )
      }
    } else {
      sets <- frames %/% frames_per_set
      out <- img[, , seq_len(sets)]
      if (sets == 1) dim(out) %<>% c(1)
      for (i in seq_len(sets)) {
        indices_i <- seq((i - 1) * frames_per_set + 1, i * frames_per_set)
        out[, , i] <- brightness(img[, , indices_i],
                                 def = def, detrend = FALSE, filt = filt,
                                 s = s, offset = offset,
                                 readout_noise = readout_noise,
                                 parallel = parallel
        )
      }
    }
  } else {
    frames <- d[4]
    sets <- frames %/% frames_per_set
    out <- img[, , , seq_len(sets)]
    if (sets == 1) dim(out) %<>% c(1)
    thresh_atts <- list()
    swaps_atts <- list()
    for (i in seq_len(n_ch)) {
      out_i <- brightness_timeseries(img[, , i, , drop = FALSE],
        def = def,
        frames_per_set = frames_per_set,
        thresh = thresh[[i]],
        detrend = detrend[[i]],
        filt = filt[[i]],
        s = s, offset = offset, readout_noise = readout_noise,
        parallel = parallel
      )
      out[, , i, ] <- out_i
      thresh_atts[[i]] <- attr(out_i, "thresh")
      swaps_atts[[i]] <- attr(out_i, "swaps")
    }
  }
  brightness_ts_img(out,
    def = def, frames_per_set = frames_per_set, overlapped = overlap,
    thresh = thresh_atts, swaps = swaps_atts, filt = filt
  )
}

brightness_file <- function(path, def,
                            thresh = NULL,
                            detrend = FALSE, quick = FALSE,
                            filt = NULL,
                            s = 1, offset = 0, readout_noise = 0,
                            parallel = FALSE) {
  checkmate::assert_file_exists(path)
  need_to_change_dir <- stringr::str_detect(path, "/")
  if (need_to_change_dir) {
    dir <- filesstrings::str_before_last(path, "/")
    cwd <- getwd()
    on.exit(setwd(cwd))
    setwd(dir)
    path %<>% filesstrings::str_after_last("/")
  }
  b <- brightness(path, def,
    thresh = thresh,
    detrend = detrend, quick = quick,
    filt = filt,
    s = s, offset = offset, readout_noise = readout_noise,
    parallel = parallel
  )
  suppressMessages(filesstrings::create_dir("brightness"))
  path %<>% filesstrings::before_last_dot() %>%
    paste0("brightness", "/", ., make_nb_filename_ending(b)) %>%
    deduplicate_nb_filename()
  ijtiff::write_tif(b, path)
}

brightness_timeseries_file <- function(path, def, frames_per_set,
                                       overlap = FALSE,
                                       thresh = NULL,
                                       detrend = FALSE, quick = FALSE,
                                       filt = NULL,
                                       s = 1, offset = 0,
                                       readout_noise = 0,
                                       parallel = FALSE) {
  if (startsWith("epsilon", tolower(def))) def <- "epsilon"
  if (def == "b") def <- "B"
  checkmate::assert_file_exists(path)
  need_to_change_dir <- stringr::str_detect(path, "/")
  if (need_to_change_dir) {
    dir <- filesstrings::str_before_last(path, "/")
    cwd <- getwd()
    on.exit(setwd(cwd))
    setwd(dir)
    path %<>% filesstrings::str_after_last("/")
  }
  bts <- brightness_timeseries(path, def,
    frames_per_set = frames_per_set, overlap = overlap,
    thresh = thresh,
    detrend = detrend, quick = quick,
    filt = filt,
    s = s, offset = offset,
    readout_noise = readout_noise,
    parallel = parallel
  )
  suppressMessages(filesstrings::create_dir("brightness_timeseries"))
  path %<>% filesstrings::before_last_dot() %>%
    paste0("brightness_timeseries", "/", ., make_nb_filename_ending(bts)) %>%
    deduplicate_nb_filename()
  ijtiff::write_tif(bts, path)
}


#' Brightness calculations for every image in a folder.
#'
#' Perform [brightness()] calculations on all tif images in a folder and save the
#' resulting brightness images to disk.
#'
#' @inheritParams brightness
#' @inheritParams number
#' @inheritParams number_folder
#'
#' @seealso [number()]
#'
#' @examples
#' \dontrun{
#' setwd(tempdir())
#' img <- ijtiff::read_tif(system.file('extdata', '50.tif', package = 'nandb'))
#' ijtiff::write_tif(img, 'img1.tif')
#' ijtiff::write_tif(img, 'img2.tif')
#' brightness_folder(def = "B", thresh = "Huang")
#' }
#' @export
brightness_folder <- function(folder_path = ".", def,
                              thresh = NULL,
                              detrend = FALSE, quick = FALSE,
                              filt = NULL,
                              s = 1, offset = 0, readout_noise = 0,
                              parallel = FALSE) {
  if (startsWith("epsilon", tolower(def))) def <- "epsilon"
  if (def == "b") def <- "B"
  init_dir <- getwd()
  on.exit(setwd(init_dir))
  setwd(folder_path)
  file_names <- list.files(pattern = "\\.tiff*$")
  purrr::map(file_names, brightness_file,
    def = def,
    thresh = thresh,
    detrend = detrend, quick = quick,
    filt = filt, s = s, offset = offset,
    readout_noise = readout_noise,
    parallel = parallel
  ) %>%
    magrittr::set_names(filesstrings::before_last_dot(file_names)) %>%
    invisible()
}

#' Brightness time-series calculations for every image in a folder.
#'
#' Perform [brightness_timeseries()] calculations on all tif images in a folder
#' and save the resulting number images to disk.
#'
#' @inheritParams brightness
#' @inheritParams brightness_timeseries
#' @inheritParams brightness_folder
#' @inheritParams number
#' @inheritParams number_timeseries
#' @inheritParams number_folder
#'
#' @seealso [brightness_timeseries()]
#'
#' @examples
#' \dontrun{
#' setwd(tempdir())
#' img <- ijtiff::read_tif(system.file('extdata', '50.tif', package = 'nandb'))
#' ijtiff::write_tif(img, 'img1.tif')
#' ijtiff::write_tif(img, 'img2.tif')
#' brightness_timeseries_folder(def = "e", thresh = "tri", frames_per_set = 20)
#' }
#' @export
brightness_timeseries_folder <- function(folder_path = ".", def,
                                         frames_per_set, overlap = FALSE,
                                         thresh = NULL,
                                         detrend = FALSE, quick = FALSE,
                                         filt = NULL,
                                         s = 1, offset = 0, readout_noise = 0,
                                         parallel = FALSE) {
  if (startsWith("epsilon", tolower(def))) def <- "epsilon"
  if (def == "b") def <- "B"
  init_dir <- getwd()
  on.exit(setwd(init_dir))
  setwd(folder_path)
  file_names <- list.files(pattern = "\\.tif")
  purrr::map(file_names, brightness_timeseries_file,
    def = def,
    frames_per_set = frames_per_set, overlap = overlap,
    thresh = thresh,
    detrend = detrend, quick = quick,
    filt = filt,
    s = s, offset = offset, readout_noise = readout_noise,
    parallel = parallel
  ) %>%
    magrittr::set_names(filesstrings::before_last_dot(file_names)) %>%
    invisible()
}

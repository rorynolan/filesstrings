#' Calculate number from image series.
#'
#' Given a time stack of images, `number()` performs a calculation of the number
#' for each pixel.
#'
#' @param img A 4-dimensional array of images indexed by `img[y, x, channel,
#'   frame]` (an object of class [ijtiff::ijtiff_img]). The image to perform the
#'   calculation on. To perform this on a file that has not yet been read in,
#'   set this argument to the path to that file (a string).
#' @param def A character. Which definition of number do you want to use, `"n"`
#'   or `"N"`?
#' @param thresh The threshold or thresholding method (see
#'   [autothresholdr::mean_stack_thresh()]) to use on the image prior to
#'   detrending and number calculations. If there are many channels, this may be
#'   specified as a vector or list, one element for each channel.
#' @param detrend Detrend your data with [detrendr::img_detrend_rh()]. This is
#'   the best known detrending method for brightness analysis. For more
#'   fine-grained control over your detrending, use the `detrendr` package. If
#'   there are many channels, this may be specified as a vector, one element for
#'   each channel.
#' @param quick `FALSE` repeats the detrending procedure (which has some inherent
#'   randomness) a few times to hone in on the best detrend. `TRUE` is quicker,
#'   performing the routine only once. `FALSE` is better.
#' @param filt Do you want to smooth (`filt = 'mean'`) or median (`filt =
#'   'median'`) filter the number image using [smooth_filter()] or
#'   [median_filter()] respectively? If selected, these are invoked here with a
#'   filter radius of 1 (with corners included, so each median is the median of
#'   9 elements) and with the option `na_count = TRUE`. If you want to
#'   smooth/median filter the number image in a different way, first calculate
#'   the numbers without filtering (`filt = NULL`) using this function and then
#'   perform your desired filtering routine on the result. If there are many
#'   channels, this may be specified as a vector, one element for each channel.
#' @param offset,readout_noise Microscope acquisition parameters. See reference
#'   Dalal et al.
#' @param s A positive number. The \eqn{S}-factor of microscope acquisition.
#' @param gamma Factor for correction of number \eqn{n} due to the illumination
#'   profile. The default (`gamma = 1`) has no effect. Changing gamma will have
#'   the effect of dividing the result by `gamma`, so the result with `gamma =
#'   0.5` is two times the result with `gamma = 1`. For a Gaussian illumination
#'   profile, use `gamma = 0.3536`; for a Gaussian-Lorentzian illumination
#'   profile, use `gamma = 0.0760`.
#' @inheritParams detrendr::img_detrend_exp
#'
#' @return A matrix, the number image.
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
#'   Hur K-H, Macdonald PJ, Berk S, Angert CI, Chen Y, Mueller JD (2014)
#'   Quantitative Measurement of Brightness from Living Cells in the Presence of
#'   Photodepletion. PLoS ONE 9(5): e97440. \doi{10.1371/journal.pone.0097440}.
#'
#' @examples
#' img <- ijtiff::read_tif(system.file('extdata', '50.tif', package = 'nandb'))
#' ijtiff::display(img[, , 1, 1])
#' num <- number(img, "N", thresh = "Huang")
#' num <- number(img, "n", thresh = "tri")
#' @export
number <- function(img, def, thresh = NULL, detrend = FALSE, quick = FALSE,
                   filt = NULL, s = 1, offset = 0, readout_noise = 0, gamma = 1,
                   parallel = FALSE) {
  checkmate::assert_string(def)
  checkmate::assert_logical(detrend)
  if (!def %in% c("n", "N")) {
    custom_stop("`def` must be one of 'N' or 'n.",
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
  swaps_atts <- extend_for_all_chs(rlang::set_attrs(NA, auto = FALSE), n_ch)
  thresh_atts <- extend_for_all_chs(NA, n_ch)
  if (n_ch == 1) {
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
    pillar_means <- detrendr::mean_pillars(img, parallel = parallel)
    pillar_vars <- detrendr::var_pillars(img, parallel = parallel)
    if (def == "N") {
      out <- ((pillar_means - offset) ^ 2) / (pillar_vars - readout_noise)
    } else {
      out <- ((pillar_means - offset) ^ 2) /
        (pillar_vars - readout_noise - s * (pillar_means - offset)) /
        gamma
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
      out_i <- number(img[, , i, , drop = FALSE],
        def = def, detrend = detrend[[i]],
        thresh = thresh[[i]], filt = filt[[i]],
        s = s, offset = offset, readout_noise = readout_noise,
        gamma = gamma, parallel = parallel
      )
      out[, , i] <- out_i
      thresh_atts[[i]] <- attr(out_i, "thresh")
      swaps_atts[[i]] <- attr(out_i, "swaps")
    }
  }
  number_img(out,
    def = def, thresh = thresh_atts, swaps = swaps_atts, filt = filt
  )
}


#' Create a number time-series.
#'
#' Given a stack of images `img`, use the first `frames_per_set` of them to
#' create one number image, the next `frames_per_set` of them to create the next
#' number image and so on to get a time-series of number images.
#'
#' This may discard some images, for example if 175 frames are in the input and
#' `frames_per_set = 50`, then the last 25 are discarded. If detrending is
#' selected, it is performed on the whole image stack before the sectioning is
#' done for calculation of numbers.
#'
#' @inheritParams number
#' @param frames_per_set The number of frames with which to calculate the
#'   successive numbers.
#' @param overlap A boolean. If `TRUE`, the windows used to calculate brightness
#'   are overlapped, if `FALSE`, they are not. For example, for a 20-frame image
#'   series with 5 frames per set, if the windows are not overlapped, then the
#'   frame sets used are 1-5, 6-10, 11-15 and 16-20; whereas if they are
#'   overlapped, the frame sets are 1-5, 2-6, 3-7, 4-8 and so on up to 16-20.
#'
#' @return An object of class [number_ts_img].
#'
#' @seealso [number()].
#'
#' @examples
#' img <- ijtiff::read_tif(system.file('extdata', '50.tif', package = "nandb"))
#' nts <- number_timeseries(img, "n", frames_per_set = 20, thresh = "Huang")
#' @export
number_timeseries <- function(img, def, frames_per_set, overlap = FALSE,
                              thresh = NULL,
                              detrend = FALSE, quick = FALSE, filt = NULL,
                              s = 1, offset = 0, readout_noise = 0, gamma = 1,
                              parallel = FALSE) {
  if (!def %in% c("n", "N")) if (!def %in% c("n", "N")) {
    custom_stop("`def` must be one of 'N' or 'n.",
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
        out[, , i] <- number(img[, , indices_i],
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
        out[, , i] <- number(img[, , indices_i],
          def = def, detrend = FALSE, filt = filt[[1]],
          s = s, offset = offset,
          readout_noise = readout_noise, gamma = gamma,
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
      out_i <- number_timeseries(img[, , i, , drop = FALSE],
        def = def, frames_per_set = frames_per_set,
        thresh = thresh[[i]],
        detrend = detrend[[i]], quick = quick,
        filt = filt[[i]], offset = offset,
        readout_noise = readout_noise,
        parallel = parallel
      )
      out[, , i, ] <- out_i
      thresh_atts[[i]] <- attr(out_i, "thresh")
      swaps_atts[[i]] <- attr(out_i, "swaps")
    }
  }
  number_ts_img(out,
    def = def, frames_per_set = frames_per_set, overlapped = overlap,
    thresh = thresh_atts, swaps = swaps_atts, filt = filt
  )
}

number_file <- function(path, def, detrend = FALSE, quick = FALSE,
                        thresh = NULL, filt = NULL,
                        s = 1, offset = 0, readout_noise = 0, gamma = 1,
                        parallel = FALSE) {
  if (!def %in% c("n", "N")) if (!def %in% c("n", "N")) {
    custom_stop("`def` must be one of 'N' or 'n.",
                "You have used `def = '{def}'`.")
  }
  checkmate::assert_file_exists(path)
  need_to_change_dir <- stringr::str_detect(path, "/")
  if (need_to_change_dir) {
    dir <- filesstrings::str_before_last(path, "/")
    cwd <- getwd()
    on.exit(setwd(cwd))
    setwd(dir)
    path %<>% filesstrings::str_after_last("/")
  }
  num <- number(path, def,
    detrend = detrend, quick = quick,
    thresh = thresh, filt = filt,
    s = s, offset = offset, readout_noise = readout_noise,
    gamma = gamma, parallel = parallel
  )
  num[abs(num) >= float_max()] <- NA
  suppressMessages(filesstrings::create_dir("number"))
  path %<>%
    filesstrings::before_last_dot() %>%
    paste0("number", "/", ., make_nb_filename_ending(num)) %>%
    deduplicate_nb_filename()
  ijtiff::write_tif(num, path)
}

number_timeseries_file <- function(path, def, frames_per_set, overlap = FALSE,
                                   thresh = NULL,
                                   detrend = FALSE, quick = FALSE,
                                   filt = NULL, s = 1, offset = 0,
                                   readout_noise = 0, gamma = 1,
                                   parallel = FALSE) {
  if (!def %in% c("n", "N")) if (!def %in% c("n", "N")) {
    custom_stop("`def` must be one of 'N' or 'n.",
                "You have used `def = '{def}'`.")
  }
  checkmate::assert_file_exists(path)
  need_to_change_dir <- stringr::str_detect(path, "/")
  if (need_to_change_dir) {
    dir <- filesstrings::str_before_last(path, "/")
    cwd <- getwd()
    on.exit(setwd(cwd))
    setwd(dir)
    path %<>% filesstrings::str_after_last("/")
  }
  nts <- number_timeseries(path, def,
    frames_per_set = frames_per_set, overlap = overlap,
    thresh = thresh, detrend = detrend, quick = quick,
    filt = filt,
    s = s, offset = offset, readout_noise = readout_noise, gamma = gamma,
    parallel = parallel
  )
  nts[abs(nts) >= float_max()] <- NA
  suppressMessages(filesstrings::create_dir("number_timeseries"))
  path %<>% filesstrings::before_last_dot() %>%
    paste0("number_timeseries", "/", ., make_nb_filename_ending(nts)) %>%
    deduplicate_nb_filename()
  ijtiff::write_tif(nts, path)
}


#' Number calculations for every image in a folder.
#'
#' Perform [number()] calculations on all tif images in a folder and save the
#' resulting number images to disk.
#'
#' @note Extreme number values (of magnitude greater than 3.40282e+38) will be
#'   written to the TIFF file as `NA`, since TIFF files cannot handle such huge
#'   numbers.
#'
#' @param folder_path The path (relative or absolute) to the folder you wish to
#'   process.
#'
#' @inheritParams number
#'
#' @seealso [number()]
#'
#' @examples
#' \dontrun{
#' setwd(tempdir())
#' img <- ijtiff::read_tif(system.file('extdata', '50.tif', package = 'nandb'))
#' ijtiff::write_tif(img, 'img2.tif')
#' number_folder(def = "n", thresh = "Huang", parallel = 2)
#' }
#' @export
number_folder <- function(folder_path = ".", def,
                          thresh = NULL, detrend = FALSE, quick = FALSE,
                          filt = NULL,
                          s = 1, offset = 0, readout_noise = 0, gamma = 1,
                          parallel = FALSE) {
  if (!def %in% c("n", "N")) {
    custom_stop("`def` must be one of 'N' or 'n.",
                "You have used `def = '{def}'`.")
  }
  init_dir <- getwd()
  on.exit(setwd(init_dir))
  setwd(folder_path)
  file_names <- list.files(pattern = "\\.tif")
  purrr::map(file_names, number_file,
    def = def, thresh = thresh, detrend = detrend, quick = quick,
    filt = filt, s = s, offset = offset,
    readout_noise = readout_noise, gamma = gamma,
    parallel = parallel
  ) %>%
    magrittr::set_names(filesstrings::before_last_dot(file_names)) %>%
    invisible()
}

#' Number time-series calculations for every image in a folder.
#'
#' Perform [number_timeseries()] calculations on all tif images in a folder and
#' save the resulting number images to disk.
#'
#' @note Extreme number values (of magnitude greater than 3.40282e+38) will be
#'   written to the TIFF file as `NA`, since TIFF files cannot handle such huge
#'   numbers.
#'
#' @inheritParams number
#' @inheritParams number_timeseries
#' @inheritParams number_folder
#'
#' @seealso [number_timeseries()]
#'
#' @examples
#' \dontrun{
#' setwd(tempdir())
#' img <- ijtiff::read_tif(system.file('extdata', '50.tif', package = 'nandb'))
#' ijtiff::write_tif(img, 'img1.tif')
#' ijtiff::write_tif(img, 'img2.tif')
#' number_timeseries_folder(def = "n", thresh = "Huang", frames_per_set = 20)
#' }
#' @export
number_timeseries_folder <- function(folder_path = ".", def, frames_per_set,
                                     overlap = FALSE, thresh = NULL,
                                     detrend = FALSE, quick = FALSE,
                                     filt = NULL, s = 1, offset = 0,
                                     readout_noise = 0, gamma = 1,
                                     parallel = FALSE) {
  if (!def %in% c("n", "N")) {
    custom_stop("`def` must be one of 'N' or 'n.",
                "You have used `def = '{def}'`.")
  }
  init_dir <- getwd()
  on.exit(setwd(init_dir))
  setwd(folder_path)
  file_names <- list.files(pattern = "\\.tif")
  purrr::map(file_names, number_timeseries_file,
    def = def, frames_per_set = frames_per_set, overlap = overlap,
    thresh = thresh,
    detrend = detrend, quick = quick,
    filt = filt,
    s = s, offset = offset, readout_noise = readout_noise,
    gamma = gamma, parallel = parallel
  ) %>%
    magrittr::set_names(filesstrings::before_last_dot(file_names)) %>%
    invisible()
}

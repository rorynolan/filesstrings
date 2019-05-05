#' Detrend all TIFF images in an entire folder.
#'
#' Batch processing. Apply any of the available detrending routines to detrend
#' all of the TIFF images in a folder, saving the detrended images as TIFF files
#' in the same folder.
#'
#' These functions include a thresholding option, unlike their non-batch
#' processing counterparts which they wrap (i.e. [img_detrend_boxcar],
#' [img_detrend_exp] and [img_detrend_polynom]). This is because, when working
#' interactively, it's easy to threshold and then detrend, but for batch
#' processing, it's not so easy to efficiently do one after the other, so it's
#' nice to have that taken care of should you want it.
#'
#' @param folder_path The path (relative or absolute) to the folder you wish to
#'   process.
#' @inheritParams detrending
#' @inheritParams best_swaps
#' @param thresh The threshold or thresholding method (see
#'   [autothresholdr::mean_stack_thresh()]) to use on the image prior to
#'   detrending.
#' @param msg Receive messages to tell you how the processing of the directory
#'   is going? Default is yes.
#'
#' @return Silently, a character vector of the paths to the detrended images.
#'
#' @name detrend-directory
#'
#' @examples
#' \dontrun{
#' setwd(tempdir())
#' file.copy(c(system.file("extdata", "bleached.tif", package = "detrendr"),
#'             system.file("img", "2ch_ij.tif", package = "ijtiff")),
#'           ".")
#' dir_detrend_robinhood(thresh = "huang")
#' dir_detrend_boxcar(l = "auto", thresh = "tri", purpose = "FFS")
#' dir_detrend_exp(tau = "auto", thresh = "tri", purpose = "FCS")
#' dir_detrend_polynom(degree = "auto", thresh = "huang", purpose = "FFS")}
NULL

#' @rdname detrend-directory
#' @export
dir_detrend_robinhood <- function(folder_path = ".", swaps = "auto",
                                  thresh = NULL, quick = FALSE,
                                  msg = TRUE) {
  checkmate::assert_directory_exists(folder_path)
  cwd <- getwd()
  on.exit(setwd(cwd))
  setwd(folder_path)
  tiffs <- list.files(pattern = "\\.tiff*")
  purrr::map_chr(tiffs, file_detrend,
    method = "R",
    parameter = swaps, quick = quick,
    thresh = thresh, msg = msg
  ) %>%
    invisible()
}

#' @rdname detrend-directory
#' @export
dir_detrend_rh <- dir_detrend_robinhood

#' @rdname detrend-directory
#' @export
dir_detrend_boxcar <- function(folder_path = ".", l, purpose = c("FCS", "FFS"),
                               thresh = NULL, parallel = FALSE, msg = TRUE) {
  checkmate::assert_directory_exists(folder_path)
  cwd <- getwd()
  on.exit(setwd(cwd))
  setwd(folder_path)
  tiffs <- list.files(pattern = "\\.tiff*")
  purrr::map_chr(tiffs, file_detrend, "box",
    parameter = l,
    purpose = purpose, thresh = thresh,
    parallel = parallel, msg = msg
  ) %>%
    invisible()
}

#' @rdname detrend-directory
#' @export
dir_detrend_exp <- function(folder_path = ".", tau, purpose = c("FCS", "FFS"),
                            thresh = NULL, parallel = FALSE, msg = TRUE) {
  checkmate::assert_directory_exists(folder_path)
  cwd <- getwd()
  on.exit(setwd(cwd))
  setwd(folder_path)
  tiffs <- list.files(pattern = "\\.tiff*")
  purrr::map_chr(tiffs, file_detrend, "exp",
    parameter = tau,
    purpose = purpose, thresh = thresh,
    parallel = parallel, msg = msg
  ) %>%
    invisible()
}

#' @rdname detrend-directory
#' @export
dir_detrend_polynom <- function(folder_path = ".", degree,
                                purpose = c("FCS", "FFS"), thresh = NULL,
                                parallel = FALSE, msg = TRUE) {
  checkmate::assert_directory_exists(folder_path)
  cwd <- getwd()
  on.exit(setwd(cwd))
  setwd(folder_path)
  tiffs <- list.files(pattern = "\\.tiff*")
  purrr::map_chr(tiffs, file_detrend, "poly",
    parameter = degree,
    purpose = purpose,
    thresh = thresh, parallel = parallel, msg = msg
  ) %>%
    invisible()
}

file_detrend <- function(path, method, parameter, purpose = NULL, thresh = NULL,
                         quick = FALSE, parallel = FALSE, msg = TRUE) {
  checkmate::assert_file_exists(path)
  checkmate::assert_string(method)
  if (stringi::stri_startswith_coll("robinhood", tolower(method))) {
    method <- "robinhood"
  }
  method %<>% filesstrings::match_arg(c(
    "boxcar", "exponential", "polynomial",
    "rh", "robinhood"
  ),
  ignore_case = TRUE
  )
  if (method == "rh") method <- "robinhood"
  if (method != "robinhood") {
    checkmate::assert_string(purpose)
    purpose %<>%
      filesstrings::match_arg(c("FCS", "FFS"), ignore_case = TRUE)
  }
  need_to_change_dir <- stringr::str_detect(path, "/")
  if (need_to_change_dir) {
    dir <- filesstrings::str_before_last(path, "/")
    cwd <- getwd()
    on.exit(setwd(cwd))
    setwd(dir)
    path %<>% filesstrings::str_after_last("/")
  }
  img <- ijtiff::read_tif(path, msg = msg)
  if (msg) message("Detrending ", path, " . . .")
  if (!is.null(thresh)) {
    img %<>% autothresholdr::mean_stack_thresh(thresh)
    thresh <- attr(img, "thresh")
  }
  img <- switch(method,
    boxcar = img_detrend_boxcar(img,
      l = parameter,
      purpose = purpose,
      parallel = parallel
    ),
    exponential = img_detrend_exp(img,
      tau = parameter,
      purpose = purpose,
      parallel = parallel
    ),
    polynomial = img_detrend_polynom(img,
      degree = parameter,
      purpose = purpose,
      parallel = parallel
    ),
    robinhood = img_detrend_rh(img, swaps = parameter, quick = quick)
  )
  if (msg) message("\b Done.")
  if (!is.null(thresh)) attr(img, "thresh") <- thresh
  filename_start <- filesstrings::before_last_dot(path)
  filename_end <- make_detrended_filename_ending(img)
  suppressMessages(filesstrings::create_dir("detrended"))
  path <- paste0("detrended", "/", filename_start, filename_end, ".tif")
  ijtiff::write_tif(img, path, msg = msg)
  path
}



make_thresh_filename_part <- function(img) {
  assertthat::assert_that("thresh" %in% names(attributes(img)))
  thresh <- attr(img, "thresh")
  if (is.list(thresh)) {
    threshs <- unlist(thresh)
    methods <- purrr::map(thresh, attr, "autothresh_method") %T>% {
      for (i in seq_along(.)) if (is.null(.[[i]])) .[[i]] <- NA
    } %>%
      unlist()
    paste0(
      "thresh=",
      paste0(ifelse(is.na(methods), "", paste0(methods, "=")), threshs) %>%
        paste(collapse = ","), "_"
    )
  } else {
    make_thresh_filename_part(structure(0, thresh = list(thresh)))
  }
}

make_detrended_filename_ending <- function(img) {
  checkmate::assert_class(img, "detrended_img")
  method <- attr(img, "method")
  checkmate::assert_string(method)
  parameter <- attr(img, "parameter")
  assertthat::assert_that(
    method %in% c("boxcar", "exponential", "polynomial", "robinhood")
  )
  symbol <- switch(method, boxcar = "l", exponential = "tau",
    polynomial = "degree", robinhood = "swaps"
  )
  checkmate::assert_string(symbol)
  auto <- attr(img, "auto") %>%
    dplyr::if_else("auto=", "")
  purpose <- ""
  if (method != "robinhood") {
    purpose <- attr(img, "purpose")
    assertthat::assert_that(purpose %in% c("FCS", "FFS"))
  }
  thresh_part <- ""
  if ("thresh" %in% names(attributes(img))) {
    thresh_part <- make_thresh_filename_part(img)
  }
  purpose <- dplyr::if_else(method == "robinhood",
                            "", paste0("_for_", purpose)
  )
  parameter <- paste(paste0(auto, parameter), collapse = ",")
  paste0(
    "_", "detrended_", thresh_part, method, purpose, "_",
    symbol, "=", parameter
  )
}

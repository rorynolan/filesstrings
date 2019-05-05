#' Number and brightness image classes.
#'
#' The `number_img` and `brightness_img` classes are designed to hold objects
#' which are images calculated from the *number and brightness* technique.
#'
#' An object of class `number_img` or `brightness_img` is a 4-dimensional array
#' of real numbers in the mould of an [ijtiff_img][ijtiff::ijtiff_img] (indexed
#' as `img[y, x, channel, frame]`) with 4 attributes: \describe{\item{`def`}{Are
#' we using the `"N"` or `"n"` definition of number, or the `"B"` or `"epsilon"`
#' definition of brightness?} \item{`thresh`}{A positive integer, possibly an
#' object of class [autothresholdr::th] detailing which threshold and
#' thresholding method was used in preprocessing (in the multi-channel case, one
#' threshold per channel is given).} \item{`swaps`}{A non-negative integer
#' indicating the number of swaps Robin Hood detrending, with an attribute `auto`
#' which is a logical indicating whether or not the parameter was chosen
#' automatically (in the multi-channel case, one threshold per channel is
#' given).}\item{`filt`}{Was mean or median filtering used in postprocessing?}}
#'
#' @param img The calculated number or brightness image.
#' @param def The number or brightness definition used.
#' @param thresh A positive integer, possibly an object of class
#'   [autothresholdr::th]. If the different channels of the image had different
#'   thresholds, this argument may be specified as a vector or list (of positive
#'   integers, possibly objects of class [autothresholdr::th]), one element for
#'   each channel.
#' @param swaps A non-negative integer with an attribute `auto`. If the
#'   different channels of the image had different `swaps`, this argument may be
#'   specified as a list (of non-negative integers with attributes `auto`), one
#'   element for each channel. For undetrended images, set `swaps = NA`.
#' @param filt A string, the filtering method used. Must be either `"mean"` or
#'   `"median"`, or `NA` for no filtering. If the different channels of the
#'   image had different filters, this may be specified as a character vector,
#'   one element for each channel.
#'
#' @return An object of class `number_img` or `brightness_img`.
#'
#' @name nb-img-classes
NULL

#' @rdname nb-img-classes
#' @export
number_img <- function(img, def, thresh, swaps, filt) {
  checkmate::assert_array(img, min.d = 2, max.d = 3)
  checkmate::assert_numeric(img)
  for (i in seq_along(swaps)) checkmate::assert_count(swaps[[i]], na.ok = TRUE)
  n_ch <- dim(img) %>% {
    dplyr::if_else(length(.) == 3, .[3], 1L)
  }
  img %<>% number_img_common(
    n_ch = n_ch, def = def,
    thresh = thresh, swaps = swaps, filt = filt
  )
  class(img) %<>% c("number_img", .)
  img
}

number_img_common <- function(img, n_ch, def, thresh, swaps, filt) {
  checkmate::assert_string(def)
  for (i in seq_along(swaps)) checkmate::assert_count(swaps[[i]], na.ok = TRUE)
  if (!isTRUE(def %in% c("n", "N"))) {
    custom_stop("Argument `def` must be one of 'n' or 'N'.",
                "You have used `def = '{def}'`.")
  }
  if (length(thresh) == 1 && n_ch > 1) {
    thresh %<>% {
      rep(list(.), n_ch)
    }
  }
  if (length(swaps) == 1 && n_ch > 1) {
    swaps %<>% {
      rep(list(.), n_ch)
    }
  }
  if (all(is.na(filt))) {
    filt <- rep(NA, n_ch)
  } else {
    filt %<>% fix_filt()
  }
  if (length(filt) == 1 && n_ch > 1) filt %<>% rep(n_ch)
  thresh %<>% c_list_attr_na()
  swaps %<>% c_list_attr_na()
  checkmate::assert_numeric(thresh)
  for (i in seq_along(swaps)) {
    if (is.na(swaps[[i]]) && (!"auto" %in% names(attributes(swaps[[i]])))) {
      attr(swaps[[i]], "auto") <- FALSE
    }
  }
  checkmate::assert_numeric(swaps, lower = 0)
  if (!"auto" %in% names(attributes(swaps))) {
    custom_stop("If swaps is specified, it must have an attribute 'auto'.",
                "Your `swaps` has no attribute 'auto'.")
  } else if (length(swaps) != length(attr(swaps, "auto"))) {
    if (length(attr(swaps, "auto")) == 1) {
      attr(swaps, "auto") %<>% rep(length(swaps))
    } else {
      custom_stop("
        The 'auto' attribute of `swaps`
        must be the same length as 'swaps' itself.
        ", "
        Your `swaps` has length {length(swaps)} and its 'auto' attribute
        has length {length(attr(swaps, 'auto'))}.
        "
      )
    }
  }
  checkmate::assert_logical(attr(swaps, "auto"))
  if ((!all(is.na(swaps))) && anyNA(attr(swaps, "auto"))) {
    autos <- purrr::map(swaps, attr, "auto") %>%
      unlist()
    i <- match(T, is.na(autos))
    custom_stop("
      Each element of `swaps` must have an associated attribute 'auto'
      which must be `TRUE` or `FALSE` and not NA.
      ", "
      Element {i} of `swaps` has an 'auto' attribute which is NA.
      "
    )
  }
  if (!filesstrings::all_equal(c(
    length(thresh), length(swaps),
    length(filt), n_ch
  ))) {
    custom_stop(
      "
      The lengths of `thresh`, `swaps` and `filt` must all be the same
      as the number of channels in `img`.
      ",
      "There are {n_ch} channels in `img`.",
      "`thresh` has length {length(thresh)}.",
      "`swaps` has length {length(swaps)}.",
      "`filt` has length {length(filt)}."
    )
  }
  img %<>% structure(def = def, thresh = thresh, swaps = swaps, filt = filt)
  if (length(dim(img)) < 4) {
    if (n_ch == 1 && length(dim(img)) == 3) {
      dim(img) <- c(dim(img)[1:2], 1, dim(img)[3])
    } else {
      dim(img) %<>% c(rep(1, max(0, 4 - length(.))))
    }
  }
  img
}

#' @rdname nb-img-classes
#' @export
brightness_img <- function(img, def, thresh, swaps, filt) {
  checkmate::assert_string(def)
  for (i in seq_along(swaps)) checkmate::assert_count(swaps[[i]], na.ok = TRUE)
  def %<>% tolower()
  if (def == "b") {
    def <- "B"
  } else if (startsWith("epsilon", def)) {
    def <- "epsilon"
  } else {
    if (!def %in% c("epsilon", "B")) {
      custom_stop("`def` must be one of 'B' or 'epsilon'.",
                  "You have used `def = '{def}'`.")
    }
  }
  out <- number_img(img, "n", thresh, swaps, filt)
  attr(out, "def") <- def
  class(out)[class(out) == "number_img"] <- "brightness_img"
  out
}

#' Number and brightness time series image classes.
#'
#' The `number_ts_img` and `brightness_ts_img` classes are designed to hold
#' objects which are images calculated from the *number and brightness*
#' technique.
#'
#' An object of class `number_ts_img` or `brightness_ts_img` is a 3- or
#' 4-dimensional array of real numbers with 4 attributes:
#' \describe{\item{`def`}{Are we using the `"N"` or `"n"` definition of number,
#' or the `"B"` or `"epsilon"` definition of brightness?} \item{`thresh`}{A
#' positive integer, possibly an object of class [autothresholdr::th] detailing
#' which threshold and thresholding method was used in preprocessing (in the
#' multi-channel case, one threshold per channel is given).} \item{`swaps`}{A
#' non-negative integer indicating the number of swaps used for Robin Hood
#' detrending, with an attribute `auto` which is a logical indicating whether or
#' not the parameter was chosen automatically (in the multi-channel case, one
#' `swaps` per channel is given).} \item{`frames_per_set`}{A positive integer
#' detailing how many frames were used in the calculation of each point in the
#' number or brightness time series.} \item{`overlapped`}{A boolean. `TRUE`
#' indicates that the windows used to calculate consecutive brightnesses over
#' time were overlapped, `FALSE` indicates that they were not.}}
#'
#' @param img The calculated number or brightness time series image series.
#' @inheritParams number_img
#' @param frames_per_set The number of frames used in the calculation of each
#'   point in the number or brightness time series.
#' @param overlapped A boolean. `TRUE` indicates that the windows used to
#'   calculate consecutive brightnesses over time were overlapped, `FALSE`
#'   indicates that they were not.
#'
#' @return An object of class `number_ts_img` or `brightness_ts_img`.
#'
#' @seealso [number_timeseries()], [brightness_timeseries()].
#'
#' @name nb-ts-img-classes
NULL

#' @rdname nb-ts-img-classes
#' @export
number_ts_img <- function(img, def, frames_per_set, overlapped,
                          thresh, swaps, filt) {
  checkmate::assert_array(img, min.d = 3, max.d = 4)
  checkmate::assert_numeric(img)
  checkmate::assert_flag(overlapped)
  for (i in seq_along(swaps)) checkmate::assert_count(swaps[[i]], na.ok = TRUE)
  n_ch <- dim(img) %>% {
    dplyr::if_else(length(.) == 4, .[3], 1L)
  }
  img %<>% number_img_common(
    n_ch = n_ch, def = def,
    thresh = thresh, swaps = swaps, filt = filt
  )
  class(img) %<>% c("number_ts_img", .)
  structure(img, frames_per_set = frames_per_set, overlapped = overlapped)
}

#' @rdname nb-ts-img-classes
#' @export
brightness_ts_img <- function(img, def, frames_per_set, overlapped,
                              thresh, swaps, filt) {
  checkmate::assert_string(def)
  for (i in seq_along(swaps)) checkmate::assert_count(swaps[[i]], na.ok = TRUE)
  def %<>% tolower()
  if (def == "b") {
    def <- "B"
  } else if (startsWith("epsilon", def)) {
    def <- "epsilon"
  } else {
    if (!def %in% c("epsilon", "B")) {
      custom_stop("`def` must be one of 'B' or 'epsilon'.",
                  "You have used `def = '{def}'`.")
    }
  }
  out <- number_ts_img(img, "n",
    frames_per_set = frames_per_set, overlapped = overlapped,
    thresh = thresh, swaps = swaps, filt = filt
  )
  attr(out, "def") <- def
  class(out)[class(out) == "number_ts_img"] <- "brightness_ts_img"
  out
}


#' Cross-correlated number and brightness image classes.
#'
#' The `cc_number_img` and `cc_brightness_img` classes are designed to hold
#' objects which are images calculated from the *cross-correlated number and
#' brightness* technique.
#'
#' An object of class `cc_number_img` or `cc_brightness_img` is a 4-dimensional
#' array of real numbers in the mould of an [ijtiff_img][ijtiff::ijtiff_img]
#' (indexed as `img[y, x, channel, frame]`) with 3 attributes:
#' \describe{\item{`thresh`}{A positive integer, possibly an object of class
#' [autothresholdr::th] detailing which threshold and thresholding method was
#' used in preprocessing (in the multi-channel case, one threshold per channel
#' is given).} \item{`swaps`}{A non-negative integer indicating the number of
#' swaps used for Robin Hood detrending, with an attribute `auto` which is a
#' logical indicating whether or not the parameter was chosen automatically (in
#' the multi-channel case, one `swaps` per channel is given).}\item{`filt`}{Was
#' mean or median filtering used in postprocessing?}}
#'
#' @param img The calculated cross-correlated number or brightness image.
#' @inheritParams nb-img-classes
#'
#' @return An object of class `cc_number_img` or `cc_brightness_img`.
#'
#' @name cc-nb-img-classes
NULL

#' @rdname cc-nb-img-classes
#' @export
cc_number_img <- function(img, thresh, swaps, filt) {
  checkmate::assert_array(img, d = 2)
  checkmate::assert_numeric(img)
  checkmate::assert_string(filt, na.ok = TRUE)
  for (i in seq_along(swaps)) checkmate::assert_count(swaps[[i]], na.ok = TRUE)
  img %<>% cc_number_img_common(thresh = thresh, swaps = swaps, filt = filt)
  class(img) %<>% c("cc_number_img", .)
  img
}

#' @rdname cc-nb-img-classes
#' @export
cc_brightness_img <- function(img, thresh, swaps, filt) {
  for (i in seq_along(swaps)) checkmate::assert_count(swaps[[i]], na.ok = TRUE)
  img %<>% cc_number_img(thresh = thresh, swaps = swaps, filt = filt)
  class(img)[class(img) == "cc_number_img"] %<>%
    stringr::str_replace("number", "brightness")
  img
}

#' Cross-correlated number and brightness time series image classes.
#'
#' The `cc_number_ts_img` and `cc_brightness_ts_img` classes are designed to
#' hold objects which are images calculated from the *cross-correlated number
#' and brightness* technique.
#'
#' An object of class `cc_number_ts_img` or `cc_brightness_ts_img` is a
#' 4-dimensional array of real numbers in the mould of an
#' [ijtiff_img][ijtiff::ijtiff_img] with 3 attributes:
#' \describe{\item{`thresh`}{A positive integer, possibly an object of class
#' [autothresholdr::th] detailing which threshold and thresholding method was
#' used in preprocessing (in the multi-channel case, one threshold per channel
#' is given).} \item{`swaps`}{A non-negative integer indicating the parameter
#' used for Robin Hood detrending with an attribute `auto` which is a logical
#' indicating whether or not the parameter was chosen automatically (in the
#' multi-channel case, one `swaps` per channel is given).}
#' \item{`frames_per_set`}{A positive integer detailing how many frames were
#' used in the calculation of each point in the number or brightness time
#' series.} \item{`overlapped`}{A boolean. `TRUE` indicates that the windows
#' used to calculate consecutive brightnesses over time were overlapped, `FALSE`
#' indicates that they were not.}}
#'
#' @param img The calculated cross-correlated number or brightness time series
#'   image series.
#' @inheritParams cc_number_img
#' @param frames_per_set The number of frames used in the calculation of each
#'   point in the cross-correlated number or brightness time series.
#' @param overlapped A boolean. `TRUE` indicates that the windows used to
#'   calculate consecutive brightnesses over time were overlapped, `FALSE`
#'   indicates that they were not.
#'
#' @return An object of class `cc_number_ts_img` or `cc_brightness_ts_img`.
#'
#' @seealso [cc_number_timeseries()], [cc_brightness_timeseries()].
#'
#' @name cc-nb-ts-img-classes
NULL

#' @rdname cc-nb-ts-img-classes
#' @export
cc_number_ts_img <- function(img, frames_per_set, overlapped,
                             thresh, swaps, filt) {
  checkmate::assert_array(img, d = 3)
  checkmate::assert_numeric(img)
  checkmate::assert_flag(overlapped)
  for (i in seq_along(swaps)) checkmate::assert_count(swaps[[i]], na.ok = TRUE)
  img %<>% cc_number_img_common(thresh = thresh, swaps = swaps, filt = filt)
  class(img) %<>% c("cc_number_ts_img", .)
  structure(img, frames_per_set = frames_per_set, overlapped = overlapped)
}

#' @rdname cc-nb-ts-img-classes
#' @export
cc_brightness_ts_img <- function(img, frames_per_set, overlapped,
                                 thresh, swaps, filt) {
  for (i in seq_along(swaps)) checkmate::assert_count(swaps[[i]], na.ok = TRUE)
  checkmate::assert_flag(overlapped)
  img %<>% cc_number_ts_img(
    frames_per_set = frames_per_set, overlapped = overlapped, thresh = thresh,
    swaps = swaps, filt = filt
  )
  class(img)[class(img) == "cc_number_ts_img"] %<>%
    stringr::str_replace("number", "brightness")
  img
}

cc_number_img_common <- function(img, thresh, swaps, filt) {
  checkmate::assert_numeric(img)
  checkmate::assert_array(img, min.d = 2, max.d = 3)
  for (i in seq_along(swaps)) checkmate::assert_count(swaps[[i]], na.ok = TRUE)
  filt %<>% prepare_filt()
  thresh %<>% c_list_attr_na()
  swaps %<>% c_list_attr_na()
  checkmate::assert_numeric(thresh)
  for (i in seq_along(swaps)) {
    if (is.na(swaps[[i]]) && (!"auto" %in% names(attributes(swaps[[i]])))) {
      attr(swaps[[i]], "auto") <- FALSE
    }
  }
  checkmate::assert_numeric(swaps, lower = 0)
  if (!"auto" %in% names(attributes(swaps))) {
    custom_stop("If swaps is specified, it must have an attribute 'auto'.",
                "Your `swaps` has no attribute 'auto'.")
  } else if (length(swaps) != length(attr(swaps, "auto"))) {
    if (length(attr(swaps, "auto")) == 1) {
      attr(swaps, "auto") %<>% rep(length(swaps))
    } else {
      custom_stop("
        The 'auto' attribute of `swaps`
        must be the same length as 'swaps' itself.
        ", "
        Your `swaps` has length {length(swaps)} and its 'auto' attribute
        has length {length(attr(swaps, 'auto'))}.
        "
      )
    }
  }
  checkmate::assert_logical(attr(swaps, "auto"))
  if ((!all(is.na(swaps))) && anyNA(attr(swaps, "auto"))) {
    custom_stop("
      Each element of `swaps` must have an associated attribute 'auto'
      which must be `TRUE` or `FALSE` and not NA.
      ",
      "Element {i} of `swaps` has an 'auto' attribute which is NA."
    )
  }
  checkmate::assert_numeric(thresh, len = 2)
  checkmate::assert_numeric(swaps, len = 2)
  checkmate::assert_string(filt, na.ok = TRUE)
  if (length(dim(img)) == 3) {
    dim(img) %<>% {
      c(.[1:2], 1, .[3])
    }
  }
  if (length(dim(img)) == 2) dim(img) %<>% c(1, 1)
  structure(img, thresh = thresh, swaps = swaps, filt = filt)
}

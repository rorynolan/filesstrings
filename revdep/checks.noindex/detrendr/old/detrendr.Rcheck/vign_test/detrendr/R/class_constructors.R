#' Detrended image class.
#'
#' A [detrended_img] is a 4-dimensional array of positive integers in the style
#' of an [ijtiff_img][ijtiff::ijtiff_img] (indexed by `img[y, x, channel,
#' frame]`) which is the result of a detrending routine. It has 4 attributes:
#' \describe{\item{`method`}{The detrending method used. This must be one of
#' `"boxcar"`, `"exponential"` or `"polynomial"`. } \item{`parameter`}{The value
#' of the parameter used. This will be the `l`, `tau` or `degree` parameter for
#' the respective methods.} \item{`auto`}{A boolean that is `TRUE` if the
#' parameter was found automatically or `FALSE` if it was manually selected. }
#' \item{`purpose`}{Either `"FCS"` or `"FFS"` to denote whether the detrending
#' was done for the purpose of fluorescence correlation spectroscopy or
#' fluorescence fluctuation spectroscopy calculations respectively. `purpose` is
#' not required for _Robin Hood_ detrending.}}
#'
#' Sometimes when detrending, you can get slight negative values in the
#' detrended image. These values should really just be zero, so this constructor
#' function sets negative values of `img` to zero.
#'
#' @param img The detrended image series. A 4-dimensional array of non-negative
#'   integers in the style of an [ijtiff_img][ijtiff::ijtiff_img], or a
#'   3-dimensional array of non-negative integers which represents a single
#'   channel of an [ijtiff_img][ijtiff::ijtiff_img]-style array (indexed by
#'   `img[y, x, frame]`).
#' @param method The method used. One of `"robinhood"`,` "boxcar"`,
#'   `"exponential"` or `"polynomial"`.
#' @param parameter A number. The detrend parameter used. One per channel.
#' @param auto Logical. Was automatic detrending used? One per channel.
#' @param purpose Either `"FCS"` or `"FFS"`. Was the image detrended for the
#'   purpose of doing FCS or FFS calculations? See [detrending]. `purpose` is
#'   not required for _Robin Hood_ detrending.
#'
#' @return An object of class `detrended_img`.
#'
#' @export
detrended_img <- function(img, method, parameter, auto, purpose = NULL) {
  checkmate::assert_array(img, min.d = 3, max.d = 4)
  checkmate::assert_numeric(img)
  checkmate::assert_string(method)
  if (stringi::stri_startswith_coll("robinhood", method)) method <- "robinhood"
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
  if (!filesstrings::all_equal(floor(img), img)) {
    stop("Elements of a detrended_img must all be integers.")
  }
  if (length(dim(img)) == 3) {
    dim(img) %<>% {
      c(.[1:2], 1, .[3])
    }
  }
  n_ch <- dim(img)[3]
  if (length(parameter) == 1) parameter %<>% rep(n_ch)
  if (length(parameter) != n_ch) {
    custom_stop("
      The length of the `parameter` argument must be equal to 1 or
      equal to the number of channels in `img`.
      ", "
      Your `img` has {n_ch} channel{dplyr::if_else(n_ch == 1, '', 's')}
      and your `parameter` argument is of length {length(parameter)}.
      ")
  }
  if (method == "robinhood") {
    parameter[is.na(parameter)] <- 0
  }
  img[img < 0] <- 0
  img %<>% structure(
    method = method, parameter = parameter, auto = auto,
    purpose = purpose
  )
  class(img) %<>% c("detrended_img", .)
  img
}

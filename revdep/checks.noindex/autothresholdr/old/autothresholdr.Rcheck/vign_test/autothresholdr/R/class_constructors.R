#' Automatically found threshold class.
#'
#' A threshold found automatically via [auto_thresh()]. It is a number (the
#' value of the threshold) with 4 attributes: \itemize{\item `ignore_black` is
#' `TRUE` if black values were ignored during the thresholding and `FALSE`
#' otherwise. \item `ignore_white` is `TRUE` if white values were ignored during
#' the thresholding and `FALSE` otherwise. \item `ignore_na` is `TRUE` if `NA`s
#' were ignored during the thresholding and `FALSE` otherwise. \item
#' `autothresh_method` details which automatic thresholding method was used.}
#'
#' @param thresh A scalar. The threshold.
#' @param ignore_black `TRUE` if black values were ignored during the
#'   thresholding and `FALSE` otherwise.
#' @param ignore_white `TRUE` if white values were ignored during the
#'   thresholding and `FALSE` otherwise.
#' @param ignore_na `TRUE` if `NA` values were ignored during the thresholding
#'   and `FALSE` otherwise.
#' @param autothresh_method The name of the automatic thresholding method used.
#'
#' @return An object of class `th`.
#'
#' @export
th <- function(thresh, ignore_black, ignore_white, ignore_na,
               autothresh_method) {
  atts <- c("ignore_black", "ignore_white", "ignore_na", "autothresh_method")
  for (a in atts) attr(thresh, a) <- get(a)
  class(thresh) %<>% c("th", .)
  thresh
}

#' Thresholded array class.
#'
#' A thresholded array is an array which has had a threshold applied to it. It
#' has an attribute `thresh` which is the threshold that was applied which can
#' be a number or an object of class [th].
#'
#' The term 'array' is used loosely here in that vectors and matrices qualify as
#' arrays.
#'
#' @param arr The thresholded array (*not* the original array).
#' @param thresh The threshold that was used. Either a number or an object of
#'   class [th].
#'
#' @return An object of class [threshed_arr].
#'
#' @seealso [stack_threshed_img], [apply_mask()].
#'
#' @export
threshed_arr <- function(arr, thresh) {
  checkmate::assert_numeric(unlist(thresh))
  attr(arr, "thresh") <- thresh
  class(arr) %<>% c("threshed_arr", .)
  if (is.matrix(arr)) {
    if (! "matrix" %in% class(arr)) class(arr) %<>% c("matrix")
  }
  if (is.array(arr)) {
    if (! "array" %in% class(arr)) class(arr) %<>% c("array")
  }
  arr
}


#' Stack-thresholded image class.
#'
#' A stack-thresholded array is an array which has had stack-thresholding
#' applied to it. See [mean_stack_thresh()]. It has 3 necessary attributes:
#' \itemize{ \item `thresh` is the threshold that was applied. This is either a
#' number or an object of class [th]. Values in the original array which were
#' less than this value are deemed to have failed the thresholding. \item
#' `fail_value` is the value to which elements of the array which failed the
#' thresholding were set. This could be something like `0` or `NA`.  \item
#' `stack_thresh_method` details which stacked-thresholding method was employed;
#' this is either `"mean"` or `"median"`. }
#'
#' @inheritParams mean_stack_thresh
#' @inheritParams threshed_arr
#' @param fail_value The value to which elements of the array which failed the
#'   thresholding were set.
#' @param stack_thresh_method This must be set to either `"mean"` or `"median"`
#'   to tell which stacked-thresholding method was employed.
#'
#' @return An object of class `stack_threshed_img`.
#'
#' @seealso [threshed_arr], [mean_stack_thresh()], [med_stack_thresh()].
#'
#' @export
stack_threshed_img <- function(img, thresh, fail_value, stack_thresh_method) {
  checkmate::assert_numeric(unlist(thresh))
  checkmate::assert_numeric(img)
  checkmate::assert_array(img, d = 4)
  to_add <- c("thresh", "fail_value", "stack_thresh_method")
  for (att in to_add) attr(img, att) <- get(att)
  class(img) %<>% c("stack_threshed_img", .)
  if (is.array(img)) {
    if (! "array" %in% class(img)) class(img) %<>% c("array")
  }
  img
}

#' Array mask class.
#'
#' A *mask* of an array with respect to a given threshold is found by taking the
#' original array and setting all elements falling below the threshold to
#' `FALSE` and the others to `TRUE`. An object of class [arr_mask] has the
#' attribute `thresh` detailing the threshold value that was applied.
#'
#' @param arr An array of logicals (the mask).
#' @param thresh The threshold. Either a scalar or an object of class [th].
#'
#' @return An object of class `masked_arr`.
#'
#' @export
arr_mask <- function(arr, thresh) {
  checkmate::assert_numeric(unlist(thresh))
  checkmate::check_logical(arr)
  attr(arr, "thresh") <- thresh
  class(arr) %<>% c("arr_mask", .)
  if (is.matrix(arr)) {
    if (! "matrix" %in% class(arr)) class(arr) %<>% c("matrix")
  }
  if (is.array(arr)) {
    if (! "array" %in% class(arr)) class(arr) %<>% c("array")
  }
  arr
}

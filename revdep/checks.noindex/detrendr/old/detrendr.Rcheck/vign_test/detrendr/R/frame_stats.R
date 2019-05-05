#' Get the sums/means of frames in a 3-dimensional array.
#'
#' Frame `i` of a 3-dimensional array `arr3d` is defined as `arr3d[, , i]`.
#'
#' @param arr3d A 3-dimensional numeric array.
#' @param na_rm Do you want `NA` values to be excluded from calculations?
#'
#' @return A numeric vector.
#'
#' @examples
#' a <- array(seq_len(2 ^ 3), dim = rep(2, 3))
#' sum_frames(a)
#' mean_frames(a)
#'
#' @export
mean_frames <- function(arr3d, na_rm = FALSE) {
  checkmate::assert_numeric(arr3d)
  checkmate::assert_array(arr3d, d = 3)
  checkmate::assert_flag(na_rm)
  if (na_rm) {
    mean_frames_na_omit_(arr3d)
  } else {
    mean_frames_(arr3d)
  }
}

#' @rdname mean_frames
#' @export
sum_frames <- function(arr3d, na_rm = FALSE) {
  checkmate::assert_numeric(arr3d)
  checkmate::assert_array(arr3d, d = 3)
  checkmate::assert_flag(na_rm)
  if (na_rm) {
    sum_frames_na_omit_(arr3d)
  } else {
    sum_frames_(arr3d)
  }
}

sum_frames_na_omit_ <- function(arr3d) {
  checkmate::assert_array(arr3d, d = 3)
  checkmate::assert_numeric(arr3d)
  if (isTRUE(checkmate::check_integer(arr3d))) {
    int_sum_frames_na_omit(arr3d)
  } else {
    dbl_sum_frames_na_omit(arr3d)
  }
}

mean_frames_na_omit_ <- function(arr3d) {
  checkmate::assert_array(arr3d, d = 3)
  checkmate::assert_numeric(arr3d)
  if (isTRUE(checkmate::check_integer(arr3d))) {
    int_mean_frames_na_omit(arr3d)
  } else {
    dbl_mean_frames_na_omit(arr3d)
  }
}

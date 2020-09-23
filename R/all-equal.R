#' An alternative version of [base::all.equal()].
#'
#' @description
#' This function will return `TRUE` whenever [base::all.equal()]
#' would return `TRUE`, however it will also return `TRUE` in some other cases:
#' * If `a` is given and `b` is not, `TRUE` will be returned if all of the
#' elements of `a` are the same.
#' * If `a` is a scalar and `b` is a vector or array, `TRUE` will be returned
#' if every element in `b` is equal to `a`.
#' * If `a` is a vector or array and `b` is a scalar, `TRUE` will be returned
#' if every element in `a` is equal to `b`.
#'
#' This function ignores names and attributes (except for `dim`).
#'
#' When this function does not return `TRUE`, it returns `FALSE` (unless it
#' errors). This is unlike [base::all.equal()].
#'
#' @note \itemize{\item This behaviour is totally different from
#'   [base::all.equal()]. \item There's also [dplyr::all_equal()], which is
#'   different again. To avoid confusion, always use the full
#'   `filesstrings::all_equal()` and never `library(filesstrings)` followed by
#'   just `all_equal()`.}
#'
#' @param a A vector, array or list.
#' @param b Either `NULL` or a vector, array or list of length either 1 or
#'   `length(a)`.
#' @return `TRUE` if "equality of all" is satisfied (as detailed in
#'   'Description' above) and `FALSE` otherwise.
#' @examples
#' all_equal(1, rep(1, 3))
#' all_equal(2, 1:3)
#' all_equal(1:4, 1:4)
#' all_equal(1:4, c(1, 2, 3, 3))
#' all_equal(rep(1, 10))
#' all_equal(c(1, 88))
#' all_equal(1:2)
#' all_equal(list(1:2))
#' all_equal(1:4, matrix(1:4, nrow = 2)) # note that this gives TRUE
#' @export
all_equal <- function(a, b = NULL) {
  checkmate::assert(
    checkmate::check_null(a),
    checkmate::check_vector(a),
    checkmate::check_list(a),
    checkmate::check_array(a)
  )
  checkmate::assert(
    checkmate::check_null(b),
    checkmate::check_vector(b),
    checkmate::check_list(b),
    checkmate::check_array(b)
  )
  if (is.null(b)) {
    return(alleq1(a))
  }
  if (is.array(a) || is.array(b)) {
    return(alleq_arr(a, b))
  }
  if (is.null(a) && (!is.null(b))) {
    return(FALSE)
  }
  if (xor(length(a) == 0, length(b) == 0)) {
    return(FALSE)
  }
  if (length(a) == 1) {
    a <- rep(a, length(b))
  }
  if (length(b) == 1) {
    b <- rep(b, length(a))
  }
  return(isTRUE(all.equal(a, b, check.names = FALSE, check.attributes = FALSE)))
}

alleq1 <- function(x) {
  if (rlang::is_atomic(x)) {
    return(isTRUE(all(x == x[[1]])) || all(is.na(x)))
  }
  if (is.list(x)) {
    x %<>% lapply(function(y) {
      y %T>% {
        mostattributes(.) <- NULL
      }
    })
  }
  return(length(unique(x)) == 1)
}

alleq_arr <- function(a, b) {
  checkmate::assert(checkmate::check_array(a), checkmate::check_array(b))
  if (is.array(a) && isTRUE(checkmate::check_scalar(b))) {
    b %<>% array(dim = dim(a))
  }
  if (is.array(b) && isTRUE(checkmate::check_scalar(a))) {
    a %<>% array(dim = dim(b))
  }
  if (is.array(a)) {
    if (!is.array(b)) {
      return(FALSE)
    }
    if (!all_equal(dim(a), dim(b))) {
      return(FALSE)
    }
    a %<>% as.vector()
    b %<>% as.vector()
  }
  if (is.array(b)) {
    if (!is.array(a)) {
      return(FALSE)
    }
  }
  isTRUE(all.equal(a, b, check.names = FALSE, check.attributes = FALSE))
}

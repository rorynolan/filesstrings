#' @rdname filesstrings-deprecated
#' @param a A vector.
#' @param b Either `NULL` or a vector of length either 1 or `length(a)`
#' @export
AllEqual <- function(a, b = NULL) {
  .Deprecated("all_equal")
  all_equal(a, b)
}

#' @rdname filesstrings-deprecated
#' @param vec.ascending A strictly increasing numeric vector.
#' @param max.gap The biggest allowable gap between adjacent elements for them
#'   to be considered part of the same \emph{group}.
#' @export
GroupClose <- function(vec.ascending, max.gap = 1) {
  .Deprecated("group_close")
  group_close(vec.ascending, max.gap)
}


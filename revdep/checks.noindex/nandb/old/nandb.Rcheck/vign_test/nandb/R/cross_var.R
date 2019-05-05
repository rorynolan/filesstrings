#' Calculate the _cross-variance_ of two vectors.
#'
#' The cross-variance function is defined in the reference.
#'
#' @param x A numeric vector.
#' @param y A numeric vector with the same length as `x`.
#'
#' @return A number
#'
#' @examples
#' cross_var(0:3, 2:5)
#'
#' @references Digman, MA, Wiseman, PW, Choi, C, Horwitz, AR, Gratton, E (2009).
#' Stoichiometry of molecular complexes at adhesions in living cells. Proc.
#' Natl. Acad. Sci. U.S.A., 106, 7:2170-5.
#'
#' @export
cross_var <- function(x, y) {
  checkmate::assert_atomic_vector(x)
  checkmate::assert_atomic_vector(y)
  checkmate::assert_numeric(x)
  checkmate::assert_numeric(y)
  if (length(x) != length(y)) {
    custom_stop(
      "`x` and `y` must have the same length. ",
      "Your `x` is of length {length(x)} and your `y` is of length {length(y)}."
    )
  }
  cross_var_Cpp(x, y)
}

#' Calculate the _cross-variance_ of corresponding pillars of 3d arrays.
#'
#' The cross-variance function is defined in the reference.
#'
#' Pillar `i, j` of the 3-dimensional array `arr` is `arr[i, j, ]`.
#'
#' @param x A 3-dimensional array.
#' @param y A 3-dimensional array with the same dimensions as `x`.
#'
#' @return A matrix.
#'
#' @examples
#' x <- array(1:27, dim = rep(3, 3))
#' y <- array(0:26, dim = rep(3, 3))
#' cross_var_pillars(x, y)
#' @export
cross_var_pillars <- function(x, y) {
  checkmate::assert_numeric(x)
  checkmate::assert_numeric(y)
  checkmate::assert_array(x, d = 3)
  checkmate::assert_array(y, d = 3)
  if (!filesstrings::all_equal(dim(x), dim(y))) {
    custom_stop(
      "`x` and `y` must have the same dimensions. ",
      "
      You have `x` with dimension `c({toString(dim(x))})` and
      `y` with dimension `c({toString(dim(y))})`.
      "
    )
  }
  cross_var_pillars_Cpp(x, y)
}

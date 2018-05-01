#' Interleave two vectors.
#'
#' Given two vectors that differ in length by at most 1, interleave them into a
#' vector where every odd element is from the first vector and every even
#' element is from the second. Hence, every second element is from the same
#' original vector and every next element is from a different vector. This is
#' done in the natural order.
#' @param vec1 A vector. If the vectors are of different lengths, this must be
#'   the longer one.
#' @param vec2 A vector of either the same length as vec1 or 1 shorter than it.
#' @return A vector which is \code{vec1} and \code{vec2} interleaved.
#' @examples
#' Interleave(c("a", "b", "c"), c("x", "y", "z"))
#' Interleave(c("a", "b", "c", "d"), c("x", "y", "z"))
#' @export
Interleave <- function(vec1, vec2) {
  l1 <- length(vec1)
  l2 <- length(vec2)
  if (l1 < 1 || l2 < 1) stop("Both vectors must have positive lengths.")
  if (! (l1 - l2) %in% 0:1)
    stop("vec1 must be either the same length as vec2 or one longer than it")
  ans <- 1:(l1 + l2)
  ans[seq(1, 2 * l1 - 1, 2)] <- vec1
  ans[seq(2, 2 * l2, 2)] <- vec2
  return(ans)
}

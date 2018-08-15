#' Put specified strings in specified positions in an otherwise empty character
#' vector.
#'
#' Create a character vector with a set of strings at specified positions in
#' that character vector, with the rest of it taken up by empty strings.
#' @param strings A character vector of the strings to put in positions
#'   (coerced by [as.character] if not character already).
#' @param positions The indices of the character vector to be occupied by the
#'   elements of strings. Must be the same length as strings or of length 1.
#' @return A character vector.
#' @examples
#' put_in_pos(1:3, c(1, 8, 9))
#' put_in_pos(c("Apple", "Orange", "County"), c(5, 7, 8))
#' put_in_pos(1:2, 5)
#' @export
put_in_pos <- function(strings, positions) {
  ls <- length(strings)
  if (ls > 1 && length(positions) == 1) {
    positions <- positions:(positions + ls - 1)
  }
  strings <- as.character(strings)
  stopifnot(length(strings) == length(positions))
  out <- rep("", max(positions))
  out[positions] <- strings
  out
}

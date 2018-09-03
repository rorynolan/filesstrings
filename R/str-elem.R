#' Extract a single character from a string, using its index.
#'
#' See [strex::str_elem()].
#'
#' @inheritParams strex::str_elem
#'
#' @export
str_elem <- strex::str_elem

#' Extract bits of a string and paste them together
#'
#' See [strex::str_paste_elems()].
#'
#' @inheritParams strex::str_paste_elems
#'
#' @export
str_paste_elems <- function(string, indices) {
  checkmate::assert_character(string)
  stopifnot(length(string) == 1)
  elems <- str_elem(string, indices)
  pasted <- paste(elems, collapse = "")
  return(pasted)
}

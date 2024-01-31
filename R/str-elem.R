#' Extract a single character from a string, using its index.
#'
#' Copy of [strex::str_elem()].
#'
#' @inheritParams match_arg
#'
#' @export
str_elem <- function(...) {
  strex::str_elem(...)
}

#' @rdname str_elem
#' @export
elem <- str_elem

#' Extract several single elements from a string.
#'
#' Copy of [strex::str_elems()].
#'
#' @inheritParams match_arg
#'
#' @export
str_elems <- function(...) {
  strex::str_elems(...)
}

#' @rdname str_elems
#' @export
elems <- str_elems

#' Extract bits of a string and paste them together.
#'
#' Copy of [strex::str_paste_elems()].
#'
#' @inheritParams match_arg
#'
#' @export
str_paste_elems <- function(...) {
  strex::str_paste_elems(...)
}

#' @rdname str_paste_elems
#' @export
paste_elems <- str_paste_elems

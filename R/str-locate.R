#' Locate the braces in a string.
#'
#' Copy of [strex::str_locate_braces()].
#'
#' @inheritParams match_arg
#'
#' @export
locate_braces <- function(...) {
  strex::str_locate_braces(...)
}

#' @rdname locate_braces
#' @export
str_locate_braces <- locate_braces

#' Get the indices of the \eqn{n}th instance of a pattern.
#'
#' Copy of [strex::str_locate_nth()].
#'
#' @inheritParams match_arg
#'
#' @export
str_locate_nth <- function(...) {
  strex::str_locate_nth(...)
}

#' @rdname str_locate_nth
#' @export
locate_nth <- str_locate_nth

#' @rdname str_locate_nth
#' @export
str_locate_first <- function(...) {
  strex::str_locate_first(...)
}

#' @rdname str_locate_nth
#' @export
locate_first <- str_locate_first

#' @rdname str_locate_nth
#' @export
str_locate_last <- function(...) {
  strex::str_locate_last(...)
}

#' @rdname str_locate_nth
#' @export
locate_last <- str_locate_last

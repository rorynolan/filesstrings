#' Locate the braces in a string.
#'
#' See [strex::str_locate_braces()].
#'
#' @inheritParams strex::str_locate_braces
#'
#' @export
locate_braces <- strex::str_locate_braces

#' @rdname locate_braces
#' @export
str_locate_braces <- locate_braces

#' Get the indices of the \eqn{n}th instance of a pattern.
#'
#' See [strex::str_locate_nth()].
#'
#' @inheritParams strex::str_locate_nth
#'
#' @export
str_locate_nth <- strex::str_locate_nth

#' @rdname str_locate_nth
#' @export
locate_nth <- str_locate_nth

#' @rdname str_locate_nth
#' @export
str_locate_first <- strex::str_locate_first

#' @rdname str_locate_nth
#' @export
locate_first <- str_locate_first

#' @rdname str_locate_nth
#' @export
str_locate_last <- strex::str_locate_last

#' @rdname str_locate_nth
#' @export
locate_last <- str_locate_last

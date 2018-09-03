#' Locate the braces in a string.
#'
#' See [strex::str_locate_braces()].
#'
#' @inheritParams strex::str_locate_braces
#'
#' @export
locate_braces <- strex::str_locate_braces

#' Get the indices of the \eqn{n}th instance of a pattern.
#'
#' See [strex::str_locate_nth()].
#'
#' @inheritParams strex::str_locate_nth
#'
#' @export
str_nth_instance_indices <- strex::str_locate_nth

#' @rdname str_nth_instance_indices
#' @export
str_first_instance_indices <- strex::str_locate_first

#' @rdname str_nth_instance_indices
#' @export
str_last_instance_indices <- strex::str_locate_last

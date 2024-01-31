#' Remove the quoted parts of a string.
#'
#' Copy of [strex::str_remove_quoted()].
#'
#' @inheritParams match_arg
#'
#' @export
str_remove_quoted <- function(...) {
  strex::str_remove_quoted(...)
}

#' @rdname str_remove_quoted
#' @export
remove_quoted <- str_remove_quoted

#' Convert a string to a vector of characters
#'
#' Copy of [strex::str_to_vec()].
#'
#' @inheritParams match_arg
#'
#' @export
str_to_vec <- function(...) {
  strex::str_to_vec(...)
}

#' @rdname str_to_vec
#' @export
to_vec <- str_to_vec

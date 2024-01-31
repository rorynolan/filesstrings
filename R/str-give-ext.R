#' Ensure a file name has the intended extension.
#'
#' Copy of [strex::str_give_ext()].
#'
#' @inheritParams match_arg
#' @export
str_give_ext <- function(...) {
  strex::str_give_ext(...)
}

#' @rdname str_give_ext
#' @export
give_ext <- str_give_ext

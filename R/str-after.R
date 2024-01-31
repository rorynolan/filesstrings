#' Text after the `n`th occurrence of pattern.
#'
#' Copies of [strex::str_after_nth()] and friends.
#'
#' @inheritParams match_arg
#' @export
str_after_nth <- function(...){
  strex::str_after_nth(...)
}

#' @rdname str_after_nth
#' @export
after_nth <- function(...){
  strex::str_after_nth(...)
}

#' @rdname str_after_nth
#' @export
str_after_first <- function(...){
  strex::str_after_first(...)
}

#' @rdname str_after_nth
#' @export
after_first <- function(...){
  strex::str_after_first(...)
}

#' @rdname str_after_nth
#' @export
str_after_last <- function(...){
  strex::str_after_last(...)
}

#' @rdname str_after_nth
#' @export
after_last <- function(...){
  strex::str_after_last(...)
}

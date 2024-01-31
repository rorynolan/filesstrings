#' Text before the `n`th occurrence of pattern.
#'
#' Copies of [strex::str_before_nth()] and friends.
#'
#' @inheritParams match_arg
#' @export
str_before_nth <- function(...){
  strex::str_before_nth(...)
}

#' @rdname str_before_nth
#' @export
before_nth <- function(...){
  strex::str_before_nth(...)
}

#' @rdname str_before_nth
#' @export
str_before_first <- function(...){
  strex::str_before_first(...)
}

#' @rdname str_before_nth
#' @export
before_first <- function(...){
  strex::str_before_first(...)
}

#' @rdname str_before_nth
#' @export
str_before_last <- function(...){
  strex::str_before_last(...)
}

#' @rdname str_before_nth
#' @export
before_last <- function(...){
  strex::str_before_last(...)
}


#' Get the part of a string before the last period.
#'
#' Copy of [strex::str_before_last_dot()].
#'
#' @inheritParams match_arg
#' @export
before_last_dot <- function(...){
  strex::str_before_last_dot(...)
}

#' @rdname before_last_dot
#' @export
str_before_last_dot <- function(...){
  strex::str_before_last_dot(...)
}

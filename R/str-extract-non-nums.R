#' Extract non-numbers from a string.
#'
#' Copies of [strex::str_extract_non_numerics()] and friends.
#'
#' @inheritParams match_arg
#'
#' @export
extract_non_numerics <- function(...){
  strex::str_extract_non_numerics(...)
}

#' @rdname extract_non_numerics
#' @export
str_extract_non_numerics <- extract_non_numerics

#' @rdname extract_non_numerics
#' @inheritParams strex::str_nth_non_numeric
#' @export
nth_non_numeric <- function(...){
  strex::str_nth_non_numeric(...)
}

#' @rdname extract_non_numerics
#' @export
str_nth_non_numeric <- nth_non_numeric

#' @rdname extract_non_numerics
#' @export
first_non_numeric <- function(...){
  strex::str_first_non_numeric(...)
}

#' @rdname extract_non_numerics
#' @export
str_first_non_numeric <- first_non_numeric

#' @rdname extract_non_numerics
#' @export
last_non_numeric <- function(...){
  strex::str_last_non_numeric(...)
}

#' @rdname extract_non_numerics
#' @export
str_last_non_numeric <- last_non_numeric

#' Extract numbers from a string.
#'
#' Copies of [strex::str_extract_numbers()] and friends.
#'
#' @inheritParams match_arg
#'
#' @export
extract_numbers <- function(...){
  strex::str_extract_numbers(...)
}

#' @rdname extract_numbers
#' @export
str_extract_numbers <- extract_numbers

#' @rdname extract_numbers
#' @export
nth_number <- function(...){
  strex::str_nth_number(...)
}

#' @rdname extract_numbers
#' @export
str_nth_number <- nth_number

#' @rdname extract_numbers
#' @export
first_number <- function(...){
  strex::str_first_number(...)
}

#' @rdname extract_numbers
#' @export
str_first_number <- first_number

#' @rdname extract_numbers
#' @export
last_number <- function(...){
  strex::str_last_number(...)
}

#' @rdname extract_numbers
#' @export
str_last_number <- last_number

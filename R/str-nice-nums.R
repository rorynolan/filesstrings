#' Make string numbers comply with alphabetical order.
#'
#' Copy of [strex::str_alphord_nums()].
#'
#' @inheritParams match_arg
#' @export
str_nice_nums <- function(...) {
  strex::str_alphord_nums(...)
}

#' @rdname str_nice_nums
#' @export
nice_nums <- str_nice_nums

#' @rdname str_nice_nums
#' @export
str_alphord_nums <- nice_nums

#' @rdname str_nice_nums
#' @export
alphord_nums <- nice_nums

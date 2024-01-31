#' Check if a string could be considered as numeric.
#'
#' Copy of [strex::str_can_be_numeric()].
#'
#' @inheritParams match_arg
#'
#' @export
can_be_numeric <- function(...) {
  strex::str_can_be_numeric(...)
}

#' @rdname can_be_numeric
#' @export
str_can_be_numeric <- can_be_numeric

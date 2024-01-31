#' Argument Matching
#'
#' Copy of [strex::match_arg()].
#'
#' @param ... Pass-through to `strex` function.
#'
#' @export
match_arg <- function(...) {
  strex::match_arg(...)
}

#' @rdname match_arg
#' @export
str_match_arg <- match_arg

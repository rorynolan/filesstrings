#' Remove back-to-back duplicates of a pattern in a string.
#'
#' Copy of [strex::str_singleize()].
#'
#' @inheritParams match_arg
#'
#' @export
str_singleize <- function(...){
  strex::str_singleize(...)
}

#' @rdname str_singleize
#' @export
singleize <- str_singleize

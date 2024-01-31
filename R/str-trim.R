#' Trim something other than whitespace.
#'
#' Copy of [strex::str_trim_anything()].
#'
#' @inheritParams match_arg
#'
#' @export
str_trim_anything <- function(...){
  strex::str_trim_anything(...)
}

#' @rdname str_trim_anything
#' @export
trim_anything <- str_trim_anything

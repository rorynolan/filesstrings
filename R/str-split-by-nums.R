#' Split a string by its numeric characters.
#'
#' Copy of [strex::str_split_by_numbers()].
#'
#' @inheritParams match_arg
#'
#' @export
str_split_by_nums <- function(...) {
  strex::str_split_by_numbers(...)
}

#' @rdname str_split_by_nums
#' @export
split_by_nums <- str_split_by_nums

#' @rdname str_split_by_nums
#' @export
split_by_numbers <- str_split_by_nums

#' @rdname str_split_by_nums
#' @export
str_split_by_numbers <- str_split_by_nums

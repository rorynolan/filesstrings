#' Text before or after \eqn{n}th occurrence of pattern.
#'
#' See [strex::str_after_nth()].
#'
#' @inheritParams strex::str_after_nth
#' @export
str_after_nth <- function(strings, pattern, n) {
  nth_instance_indices <- str_nth_instance_indices(strings, pattern, n)
  str_sub(strings, nth_instance_indices[, "end"] + 1)
}

#' @rdname str_after_nth
#' @export
str_after_first <- function(strings, pattern) {
  str_after_nth(strings = strings, pattern = pattern, n = 1)
}

#' @rdname str_after_nth
#' @export
str_after_last <- function(strings, pattern) {
  str_after_nth(strings = strings, pattern = pattern, n = -1)
}

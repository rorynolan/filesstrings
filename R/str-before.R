#' @rdname str_after_nth
#' @export
str_before_nth <- function(strings, pattern, n) {
  nth_instance_indices <- str_nth_instance_indices(strings, pattern, n)
  str_sub(strings, 1, nth_instance_indices[, "start"] - 1)
}

#' @rdname str_after_nth
#' @export
str_before_first <- function(strings, pattern) {
  str_before_nth(strings = strings, pattern = pattern, n = 1)
}

#' @rdname str_after_nth
#' @export
str_before_last <- function(strings, pattern) {
  str_before_nth(strings = strings, pattern = pattern, n = -1)
}

#' Get the part of a string before the last period.
#'
#' See [strex::str_before_last_dot()].
#'
#' @inheritParams strex::str_before_last_dot
#'
#' @export
before_last_dot <- function(string) {
  string %>%
    tools::file_path_sans_ext() %T>%
      {.[(string == .) & (str_elem(., 1) == ".")] <- ""}
}

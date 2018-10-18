#' Defunct functions in `filesstrings`
#'
#' These functions are gone, no longer available.
#'
#' @param ... Arguments to defunct functions.
#'
#' @name filesstrings-defunct
NULL

#' @rdname filesstrings-defunct
#' @export
str_with_patterns <- function(...) {
  .Defunct("stringr::str_subset()", "filesstrings")
}

#' @rdname filesstrings-defunct
#' @export
count_matches <- function(...) {
  .Defunct("stringr::str_count()", "filesstrings")
}

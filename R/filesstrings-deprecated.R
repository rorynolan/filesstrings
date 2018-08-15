#' Which strings match the patterns?
#'
#' Given a character vector of strings and one of patterns (in regular
#' expression), which of the strings match all (or any) of the patterns.
#'
#' This function is deprecated. Please use [stringr::str_subset()] instead.
#'
#' @param strings A character vector.
#' @param patterns Regular expressions.
#' @param ignore_case Do we want to ignore case when matching patterns?
#' @param any Set this to `TRUE` if you want to see which strings match
#'   *any* of the patterns and not *all* (all is the default).
#'
#' @return A character vector of strings matching the patterns.
#'
#' @export
str_with_patterns <- function(strings, patterns, ignore_case = FALSE,
                              any = FALSE) {
  .Deprecated("stringr::str_subset()")
  checkmate::assert_character(strings)
  strings_orig <- strings
  if (ignore_case) {
    strings <- tolower(strings)
    patterns <- tolower(patterns)
  }
  matches <- as.matrix(vapply(strings, str_detect, logical(length(patterns)),
                              patterns))  # The docs admit that this is slow
  if (any) {
    keeps <- matrixStats::colAnys(matches)
  } else {
    keeps <- matrixStats::colAlls(matches)
  }
  strings_orig[keeps]
}

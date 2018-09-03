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

#' Count the number of the matches of a pattern in a string.
#'
#' Vectorized over `string` and `pattern`.
#'
#' @param string A character vector.
#' @param pattern A character vector. Pattern(s) specified like the pattern(s)
#'   in the stringr package (e.g. look at [stringr::str_locate()]). If
#'   this has length >1 its length must be the same as that of `string`.
#'
#' @return An integer vector giving the number of matches in each string.
#'
#' @export
count_matches <- function(string, pattern) {
  .Deprecated("stringr::str_count()")
  lp <- length(pattern)
  ls <- length(string)
  if (lp > 1) {
    if (lp != ls) {
      stop("If pattern has length greater than 1, ",
           "it must have the same length as string.")
    }
  }
  locations <- str_locate_all(string, pattern)
  n_matches <- intmat_list_nrows(locations)
  n_matches
}


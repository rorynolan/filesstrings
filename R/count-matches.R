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
#' @examples
#' count_matches(c("abacad", "xyz"), "a")
#' count_matches("2.1.0.13", ".")
#' count_matches("2.1.0.13", stringr::coll("."))
#' @export
count_matches <- function(string, pattern) {
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


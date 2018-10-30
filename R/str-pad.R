#' Pad a character vector with empty strings.
#'
#' Extend a character vector by appending empty strings at the end.
#'
#' @param char_vec A character vector. The thing you wish to expand.
#' @param extend_by A non-negative integer. By how much do you wish to extend
#'   the vector?
#' @param length_out A positive integer. How long do you want the output vector
#'   to be?
#'
#' @return A character vector.
#'
#' @examples
#' extend_char_vec(1:5, extend_by = 2)
#' extend_char_vec(c("a", "b"), length_out = 10)
#' @export
extend_char_vec <- function(char_vec, extend_by = NA, length_out = NA) {
  checkmate::assert_int(extend_by, na.ok = TRUE)
  if (is.na(extend_by) && is.na(length_out)) {
    stop("One of extend.by or length.out must be specified.")
  }
  checkmate::assert_atomic(char_vec)
  if (is.numeric(char_vec)) char_vec %<>% as.character()
  checkmate::assert_character(char_vec)
  if (!is.na(extend_by)) return(c(char_vec, rep("", extend_by)))
  if (!is.na(length_out)) {
    if (!(is.numeric(length_out) && length_out >= length(char_vec))) {
      stop(
        "If specified, length.out must be numeric and at least equal to ",
        "the length of char.vec."
      )
    }
    c(char_vec, rep("", length_out - length(char_vec)))
  }
}

#' Split a string by its numeric characters.
#'
#' Break a string wherever you go from a numeric character to a non-numeric or
#' vice-versa.
#' @inheritParams extract_numbers
#' @examples
#' str_split_by_nums(c("abc123def456.789gh", "a1b2c344"))
#' str_split_by_nums("abc123def456.789gh", decimals = TRUE)
#' str_split_by_nums("22")
#' @export
str_split_by_nums <- function(string, decimals = FALSE,
                              leading_decimals = FALSE, negs = FALSE) {
  nums <- extract_numbers(string, leave_as_string = TRUE, decimals = decimals,
                          leading_decimals = leading_decimals, negs = negs)
  if (all(can_be_numeric(string))) {
    non_nums <- list(character(0))[rep(1, length(string))]
  } else {
    non_nums <- extract_non_numerics(string, decimals = decimals, negs = negs,
                                     leading_decimals = leading_decimals)
  }
  interleave_correctly(string, non_nums, nums)
}

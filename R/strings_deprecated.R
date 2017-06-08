#' @rdname filesstrings-deprecated
#' @return A character vector. `TRUE` if the argument can be considered to be numeric or `FALSE`
#'   otherwise.
#' @export
CanBeNumeric <- function(string) {
  .Deprecated("can_be_numeric")
  can_be_numeric(string)
}

#' @param string A string.
#' @rdname filesstrings-deprecated
#' @export
GetCurrencies <- function(string) {
  .Deprecated("get_currencies")
  get_currencies(string)
}

#' @param strings A character vector.
#' @rdname filesstrings-deprecated
#' @export
GetCurrency <- function(strings) {
  .Deprecated("get_currency")
  get_currency(strings)
}

#' @rdname filesstrings-deprecated
#' @param pattern A character vector. Tegular expression(s) of the pattern(s)
#'   that we wish not to be duplicated.
#' @export
DuplicatesToSingles <- function(string, pattern) {
  .Deprecated("singleize")
  singleize(string, pattern)
}

#' @rdname filesstrings-deprecated
#' @export
NiceNums <- function(strings) {
  .Deprecated("nice_nums")
  nice_nums(strings)
}

#' @rdname filesstrings-deprecated
#' @param leave.as.string Do you want to return the number as a string (`TRUE`)
#'   or as numeric (`FALSE`, the default)?
#' @param decimals Do you want to include the possibility of decimal numbers
#'   (`TRUE`) or not (`FALSE`, the default).
#' @param leading.decimals Do you want to allow a leading decimal point to be
#'   the start of a number?
#' @param negs Do you want to allow negative numbers? Note that double negatives
#'   are not handled here (see the examples).
#' @export
ExtractNumbers <- function(string, leave.as.string = FALSE, decimals = FALSE,
                           leading.decimals = FALSE, negs = FALSE) {
  .Deprecated("extract_numbers")
  extract_numbers(string, leave.as.string, decimals,
                  leading.decimals, negs)
}

#' @rdname filesstrings-deprecated
#' @export
ExtractNonNumerics <- function(string, decimals = FALSE,
                               leading.decimals = FALSE, negs = FALSE) {
  .Deprecated("extract_non_numerics")
  extract_non_numerics(string, decimals, leading.decimals, negs)
}

#' @rdname filesstrings-deprecated
#' @export
NthNumber <- function(string, n = 1, leave.as.string = FALSE, decimals = FALSE,
                      leading.decimals = FALSE, negs = FALSE) {
  .Deprecated("nth_number")
  nth_number(string, n, leave.as.string, decimals,
             leading.decimals, negs)
}

#' @rdname filesstrings-deprecated
#' @export
NthNonNumeric <- function(string, n = 1, decimals = FALSE,
                          leading.decimals = FALSE, negs = FALSE) {
  .Deprecated("nth_non_numeric")
  nth_non_numeric(string, n, decimals,
                  leading.decimals, negs)
}

#' @rdname filesstrings-deprecated
#' @export
StrSplitByNums <- function(string, decimals = FALSE, leading.decimals = FALSE,
                           negs = FALSE) {
  .Deprecated("str_split_by_nums")
  str_split_by_nums(string, decimals, leading.decimals,
                    negs)
}

#' @rdname filesstrings-deprecated
#' @param index An integer. Negative indexing is allowed as in
#'   [stringr::str_sub()].
#' @export
StrElem <- function(string, index) {
  .Deprecated("str_elem")
  str_elem(string, index)
}

#' @rdname filesstrings-deprecated
#' @param indices A numeric vector of positive integers detailing the indices of
#'   the characters of `string` that we wish to paste together.
#' @export
StrElemsPasted <- function(string, indices) {
  .Deprecated("str_paste_elems")
  str_paste_elems(string, indices)
}

#' @rdname filesstrings-deprecated
#' @export
StringToVec <- function(string) {
  .Deprecated("str_to_vec")
  str_to_vec(string)
}

#' @rdname filesstrings-deprecated
#' @param patterns Regular expressions.
#' @param ignore.case Do we want to ignore case when matching patterns?
#' @param any Set this to `TRUE` if you want to see which strings match
#'   \emph{any} of the patterns and not \emph{all} (all is the default).
#' @export
StringsWithPatterns <- function(strings, patterns, ignore.case = FALSE,
                                any = FALSE) {
  .Deprecated("str_with_patterns")
  str_with_patterns(strings, patterns, ignore.case, any)
}

#' @rdname filesstrings-deprecated
#' @param n A natural number to identify the \eqn{n}th occurrence (defaults to
#'   first (`n = 1`)). This can be negatively indexed, so if you wish to select
#'   the \emph{last} occurrence, you need `n = -1`, for the second-last, you
#'   need `n = -2` and so on.
#' @export
StrAfterNth <- function(strings, pattern, n = 1) {
  .Deprecated("str_after_nth")
  str_after_nth(strings, pattern, n)
}

#' @rdname filesstrings-deprecated
#' @export
StrBeforeNth <- function(strings, pattern, n = 1) {
  .Deprecated("str_before_nth")
  str_before_nth(strings, pattern, n)
}

#' @rdname filesstrings-deprecated
#' @export
BeforeLastDot <- function(string) {
  .Deprecated("before_last_dot")
  before_last_dot(string)
}

#' @rdname filesstrings-deprecated
#' @param char.vec A character vector. The thing you wish to expand.
#' @param extend.by A non-negative integer. By how much do you wish to extend
#'   the vector?
#' @param length.out A positive integer. How long do you want the output vector
#'   to be?
#' @export
ExtendCharVec <- function(char.vec, extend.by = NA, length.out = NA) {
  .Deprecated("extend_char_vec")
  extend_char_vec(char.vec, extend.by, length.out)
}

#' @rdname filesstrings-deprecated
#' @param files A character vector of files to be read in via `readLines`
#'   to be pasted. If you would like to use this function on vectors already in
#'   the environment (without reading in files), pass them into this argument as
#'   a list (see the examples).
#' @param sep What (if anything) do you want to paste in between things as
#'   you're pasting them together.
#' @export
PasteDifferentLengths <- function(files, sep = "") {
  .Deprecated("paste_different_lengths")
  paste_different_lengths(files, sep)
}

#' @rdname filesstrings-deprecated
#' @param positions The indices of the charachter vector to be occupied by the
#'   elements of strings. Must be the same length as strings or of length 1.
#' @export
PutInPos <- function(strings, positions) {
  .Deprecated("put_in_pos")
  put_in_pos(strings, positions)
}

#' @rdname filesstrings-deprecated
#' @param side Which side do you want to trim from? `"both"` is the
#'   default, but you can also have just either `"left"` or `"right"`
#'   (or optionally the shorthands `"b"`, `"l"` and `"r"`).
#' @export
TrimAnything <- function(string, pattern, side = "both") {
  .Deprecated("trim_anything")
  trim_anything(string, pattern, side)
}

#' @rdname filesstrings-deprecated
#' @export
CountMatches <- function(string, pattern) {
  .Deprecated("count_matches")
  count_matches(string, pattern)
}

#' @rdname filesstrings-deprecated
#' @export
LocateBraces <- function(string) {
  .Deprecated("locate_braces")
  locate_braces(string)
}

#' @rdname filesstrings-deprecated
#' @export
RemoveQuoted <- function(string) {
  .Deprecated("remove_quoted")
  remove_quoted(string)
}

#' @rdname filesstrings-deprecated
#' @param ext The intended file extension (with or without the ".").
#' @param replace If the file has an extension already, replace it (or append
#'   the new extension name)?
#' @export
GiveExt <- function(string, ext, replace = FALSE) {
  .Deprecated("give_ext")
  give_ext(string, ext, replace)
}

#' @rdname filesstrings-deprecated
#' @param lower Do you want the output to be all lower case (or as is)?
#' @export
SplitCamelCase <- function(string, lower = FALSE) {
  .Deprecated("str_split_camel_case")
  str_split_camel_case(string, lower)
}

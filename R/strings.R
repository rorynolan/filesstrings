#' Check if a string could be considered as numeric.
#'
#' After padding is removed, could the input string be considered to be numeric,
#' i.e. could it be coerced to numeric. This function is vectorized over its one
#' argument.
#' @param string A character vector.
#' @return A character vector. `TRUE` if the argument can be considered to be
#'   numeric or `FALSE` otherwise.
#' @examples
#' can_be_numeric("3")
#' can_be_numeric("5 ")
#' can_be_numeric(c("1a", "abc"))
#' @export
can_be_numeric <- function(string) !is.na(suppressWarnings(as.numeric(string)))

#' Get the currencies of numbers within a string.
#'
#' The currency of a number is defined as the character coming before the number
#' in the string. If nothing comes before (i.e. if the number is the first thing
#' in the string), the currency is the empty string, similarly the currency can
#' be a space, comma or any manner of thing.
#' \itemize{
#'   \item `get_currency`
#' takes a string and returns the currency of the first number therein. It is
#' vectorized over string.
#'   \item `get_currencies` takes a string and returns
#' the currencies of all of the numbers within that string. It is not
#' vectorized. }
#'
#' These functions do not allow for leading decimal points.
#'
#' @name currency
#'
#' @param string A string.
#' @param strings A character vector.
#'
#' @return
#' \itemize{
#' \item `get_currency` returns a character vector.
#' \item `get_currencies` returns a data frame with one column for the
#' currency symbol and one for the amount.
#' }
#' @examples
#' get_currencies("35.00 $1.14 abc5 $3.8 77")
#' @export
get_currencies <- function(string) {
  stopifnot(is.character(string), length(string) == 1)
  ssbn <- str_split_by_nums(string, decimals = TRUE, negs = TRUE)[[1]]
  num_indices <- which(can_be_numeric(ssbn))
  numbers <- as.numeric(ssbn[num_indices])
  before_num_indices <- num_indices - 1
  before_num_strings <- vapply(before_num_indices, function(x) {
    ifelse(x %in% num_indices || x < 1, "", ssbn[x])
  }, character(1))
  currencies <- str_elem(before_num_strings, -1)
  tibble::tibble(currency = currencies, amount = numbers)
}

#' @rdname currency
#' @examples
#' get_currency(c("ab3 13", "$1"))
#' @export
get_currency <- function(strings) {
  num_starts <- str_locate(strings, "[0-9]")[, "start"]
  before_indices <- num_starts - 1
  str_elem(strings, before_indices)
}

#' Remove back-to-back duplicates of a pattern in a string.
#'
#' If a string contains a given pattern duplicated back-to-back a number of
#' times, remove that duplication, leaving the pattern appearing once in that
#' position (works if the pattern is duplicated in different parts of a string,
#' removing all instances of duplication). This is vectorized over string and
#' pattern.
#' @param string A character vector. The string(s) to be purged of duplicates.
#' @param pattern A character vector. Regular expression(s) of the pattern(s)
#'   that we wish not to be duplicated.
#' @return The string with the duplicates fixed.
#' @examples
#' singleize("abc//def", "/")
#' singleize("abababcabab", "ab")
#' singleize(c("abab", "cdcd"), "cd")
#' singleize(c("abab", "cdcd"), c("ab", "cd"))
#' @export
singleize <- function(string, pattern) {
  dup_patt <- str_c("(", pattern, ")+")
  str_replace_all(string, dup_patt, pattern)
}

#' Make string numbers comply with alphabetical order
#'
#' If strings are numbered, their numbers may not \emph{comply} with
#' alphabetical order, i.e. "abc2" comes after "abc10" in alphabetical order. We
#' might (for whatever reason) wish to change them such that they come in the
#' order \emph{that we would like}. This function alters the strings such that
#' they comply with alphabetical order, so here "abc2" would be renamed to
#' "abc02". It works on file names with more than one number in them e.g.
#' "abc01def3" (a string with 2 numbers). All the file names that it works on
#' must have the same number of numbers, and the non-number bits must be the
#' same.
#'
#' @param strings A vector of strings.
#' @examples
#' strings <- paste0("abc", 1:12)
#' strings
#' nice_nums(strings)
#'
#' nice_nums(c("abc9def55", "abc10def7"))
#' nice_nums(c("01abc9def55", "5abc10def777", "99abc4def4"))
#' nice_nums(1:10)
#'
#' \dontrun{
#' nice_nums(c("abc9def55", "abc10xyz7"))}
#' @export
nice_nums <- function(strings) {
  checkmate::assert(checkmate::check_numeric(strings),
                    checkmate::check_character(strings))
  if (is.numeric(strings)) strings %<>% as.character()
  have_nums <- str_detect(strings, "\\d")
  if (!all(have_nums)) {
    stop("Some of the input strings have no numbers in them.")
  }
  non_nums <- extract_non_numerics(strings)
  if (!all_equal(non_nums)) {
    stop("The non-number bits of the strings are different.")
  }
  nums <- extract_numbers(strings, leave_as_string = TRUE)
  if (!all_equal(lengths(nums))) {
    stop("Some of the strings contain different numbers of numbers")
  }
  nums <- simplify2array(nums)
  if (!is.matrix(nums)) nums <- t(nums)
  ncn <- nchar(nums)
  max_lengths <- matrixStats::rowMaxs(ncn)
  min_length <- min(ncn)
  to_prefix <- rep("0", max(max_lengths) - min_length) %>% str_c(collapse = "")
  nums <- str_c(to_prefix, nums)
  starts <- -rep(max_lengths, ncol(ncn))
  nums <- str_sub(nums, starts, -1) %>%
    split(rep(seq_len(ncol(ncn)), each = nrow(ncn)))
  num_first <- str_elem(strings, 1) %>% can_be_numeric
  if (!all_equal(num_first)) {
    stop("Some strings start with numbers and some don't")
  }
  if (num_first[1]) {
    interleaves <- interleave_char_lists(nums, non_nums)
  } else {
    interleaves <- interleave_char_lists(non_nums, nums)
  }
  paste_collapse_list_elems(interleaves)
}

#' Extract numbers (or non-numbers) from a string.
#'
#' `extract_numbers` extracts the numbers (or non-numbers) from a string where
#' decimals are optionally allowed. `extract_non_numerics` extracts the bits of
#' the string that aren't extracted by `extract_numbers`. `nth_number` is a
#' convenient wrapper for `extract_numbers`, allowing you to choose which number
#' you want. Similarly `nth_non_numeric`. Please view the examples at the bottom
#' of this page to ensure that you understand how these functions work, and
#' their limitations. These functions are vectorized over `string`.
#'
#' If any part of a string contains an ambiguous number (e.g. `1.2.3` would be
#' ambiguous if `decimals = TRUE` (but not otherwise)), the value returned for
#' that string will be `NA`. Note that these functions do not know about
#' scientific notation (e.g. `1e6` for 1000000).
#'
#' \itemize{
#' \item `first_number(...)` is just `nth_number(..., n = 1)`.
#' \item `last_number(...)` is just `nth_number(..., n = -1)`.
#' \item `first_non_numeric(...)` is just `nth_non_numeric(..., n = 1)`.
#' \item `last_non_numeric(...)` is just `nth_non_numeric(..., n = -1)`.
#' }
#'
#' @param string A string.
#' @param leave_as_string Do you want to return the number as a string (`TRUE`)
#'   or as numeric (`FALSE`, the default)?
#' @param decimals Do you want to include the possibility of decimal numbers
#'   (`TRUE`) or not (`FALSE`, the default).
#' @param leading_decimals Do you want to allow a leading decimal point to be
#'   the start of a number?
#' @param negs Do you want to allow negative numbers? Note that double negatives
#'   are not handled here (see the examples).
#' @return For `extract_numbers` and `extract_non_numerics`, a list of numeric or
#'   character vectors, one list element for each element of `string`. For
#'   `nth_number` and `nth_non_numeric`, a vector the same length as `string` (as
#'   in `length(string)`, not `nchar(string)`).
#' @examples
#' extract_numbers(c("abc123abc456", "abc1.23abc456"))
#' extract_numbers(c("abc1.23abc456", "abc1..23abc456"), decimals = TRUE)
#' extract_numbers("abc1..23abc456", decimals = TRUE)
#' extract_numbers("abc1..23abc456", decimals = TRUE, leading_decimals = TRUE)
#' extract_numbers("abc1..23abc456", decimals = TRUE, leading_decimals = TRUE,
#'                 leave_as_string = TRUE)
#' extract_numbers("-123abc456")
#' extract_numbers("-123abc456", negs = TRUE)
#' extract_numbers("--123abc456", negs = TRUE)
#' extract_non_numerics("abc123abc456")
#' extract_non_numerics("abc1.23abc456")
#' extract_non_numerics("abc1.23abc456", decimals = TRUE)
#' extract_non_numerics("abc1..23abc456", decimals = TRUE)
#' extract_non_numerics("abc1..23abc456", decimals = TRUE,
#' leading_decimals = TRUE)
#' extract_non_numerics(c("-123abc456", "ab1c"))
#' extract_non_numerics("-123abc456", negs = TRUE)
#' extract_non_numerics("--123abc456", negs = TRUE)
#' extract_numbers(c(rep("abc1.2.3", 2), "a1b2.2.3", "e5r6"), decimals = TRUE)
#' extract_numbers("ab.1.2", decimals = TRUE, leading_decimals = TRUE)
#' nth_number("abc1.23abc456", 2)
#' nth_number("abc1.23abc456", 2, decimals = TRUE)
#' nth_number("-123abc456", -2, negs = TRUE)
#' extract_non_numerics("--123abc456", negs = TRUE)
#' nth_non_numeric("--123abc456", 1)
#' nth_non_numeric("--123abc456", -2)
#' @export
extract_numbers <- function(string, leave_as_string = FALSE, decimals = FALSE,
         leading_decimals = FALSE, negs = FALSE) {
  if (leading_decimals == TRUE && decimals == FALSE) {
    stop("To allow leading decimals, you need to first allow decimals.")
  }
  stopifnot(is.character(string))
  if (decimals) {
    pattern <- "(?:[0-9]+(?:\\.?[0-9]+)*)+"
    if (leading_decimals) pattern <- str_c("\\.?", pattern)
  } else {
    pattern <- "[0-9]+"
  }
  if (negs) pattern <- str_c("-?", pattern)
  numbers <- str_extract_all(string, pattern)
  numerics <- suppressWarnings(lst_char_to_num(numbers))
  if (!decimals && isTRUE(checkmate::check_integerish(unlist(numerics))))
    numerics %<>% purrr::map(as.integer)
  na_pos <- purrr::map_lgl(numerics, anyNA)
  if (leave_as_string) {
    numbers[na_pos] <- NA_character_
  } else {
    numbers <- numerics
    if (decimals) {
      numbers[na_pos] <- NA_real_
    } else {
      numbers[na_pos] <- NA_integer_
    }
  }
  numbers
}

#' @rdname extract_numbers
#' @export
extract_non_numerics <- function(string, decimals = FALSE,
                               leading_decimals = FALSE, negs = FALSE) {
  if (leading_decimals == TRUE && decimals == FALSE) {
    stop("To allow leading decimals, you need to first allow decimals.")
  }
  stopifnot(is.character(string))
  if (decimals) {
    pattern <- "(?:[0-9]+(?:\\.?[0-9]+)*)+"
    if (leading_decimals) pattern <- str_c("\\.?", pattern)
  } else {
    pattern <- "[0-9]+"
  }
  if (negs) pattern <- str_c("-?", pattern)
  non_numerics <- str_split(string, pattern) %>% str_list_remove_empties
  numerics <- extract_numbers(string, decimals = decimals,
                             leading_decimals = leading_decimals, negs = negs)
  na_pos <- purrr::map_lgl(numerics, anyNA)
  non_numerics[na_pos] <- NA_character_
  non_numerics
}

#' @param n The index of the number (or non-numeric) that you seek. Negative
#'   indexing is allowed i.e. `n = 1` (the default) will give you the first
#'   number (or non-numeric) whereas `n = -1` will give you the last number (or
#'   non-numeric), `n = -2` will give you the second last number and so on.
#' @rdname extract_numbers
#' @export
nth_number <- function(string, n, leave_as_string = FALSE, decimals = FALSE,
                       leading_decimals = FALSE, negs = FALSE) {
  # this function doesn't work for strings with decimal numbers
  if (n == 0) stop("n must be a nonzero integer.")
  numbers <- extract_numbers(string, leave_as_string = TRUE, negs = negs,
                      decimals = decimals, leading_decimals = leading_decimals)
  nth_numbers <- str_list_nth_elems(numbers, n)
  if (leave_as_string) {
    nth_numbers
  } else {
    if (decimals) {
      as.numeric(nth_numbers)
    } else {
      as.integer(nth_numbers)
    }
  }
}

#' @rdname extract_numbers
#' @export
first_number <- function(string, leave_as_string = FALSE, decimals = FALSE,
                         leading_decimals = FALSE, negs = FALSE) {
  nth_number(string, n = 1, leave_as_string = leave_as_string,
             decimals = decimals, leading_decimals = leading_decimals,
             negs = negs)
}

#' @rdname extract_numbers
#' @export
last_number <- function(string, leave_as_string = FALSE, decimals = FALSE,
                         leading_decimals = FALSE, negs = FALSE) {
  nth_number(string, n = -1, leave_as_string = leave_as_string,
             decimals = decimals, leading_decimals = leading_decimals,
             negs = negs)
}

#' @rdname extract_numbers
#' @export
nth_non_numeric <- function(string, n, decimals = FALSE,
                            leading_decimals = FALSE, negs = FALSE) {
  if (n == 0) stop("n must be a nonzero integer.")
  non_numerics <- extract_non_numerics(string, decimals = decimals, negs = negs,
                                     leading_decimals = leading_decimals)
  str_list_nth_elems(non_numerics, n)
}

#' @rdname extract_numbers
#' @export
first_non_numeric <- function(string, decimals = FALSE,
                              leading_decimals = FALSE, negs = FALSE) {
  nth_non_numeric(string, n = 1,
                  decimals = decimals, leading_decimals = leading_decimals,
                  negs = negs)
}

#' @rdname extract_numbers
#' @export
last_non_numeric <- function(string, decimals = FALSE,
                             leading_decimals = FALSE, negs = FALSE) {
  nth_non_numeric(string, n = -1,
                  decimals = decimals, leading_decimals = leading_decimals,
                  negs = negs)
}

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
  correct_interleave(string, non_nums, nums)
}


#' Extract a single character from a string, using its index.
#'
#' If the element does not exist, this function returns the empty string.
#'
#' @param string A string.
#' @param index An integer. Negative indexing is allowed as in
#'   [stringr::str_sub()].
#' @return A one-character string.
#' @examples
#' str_elem(c("abcd", "xyz"), 3)
#' str_elem("abcd", -2)
#' @export
str_elem <- function(string, index) {
  if (!is.character(string)) stop("string must be of character type")
  str_sub(string, index, index)
}

#' Extract bits of a string and paste them together
#'
#' Extract characters - specified by their indices - from a string and paste
#' them together
#' @param string A string.
#' @param indices A numeric vector of positive integers detailing the indices of
#'   the characters of `string` that we wish to paste together.
#' @return A string.
#' @examples
#' str_paste_elems("abcdef", c(2, 5:6))
#' @export
str_paste_elems <- function(string, indices) {
  if (!is.character(string)) stop("string must be of character type")
  stopifnot(length(string) == 1)
  elems <- str_elem(string, indices)
  pasted <- paste(elems, collapse = "")
  return(pasted)
}

#' Convert a string to a vector of characters
#'
#' Go from a string to a vector whose \eqn{i}th element is the \eqn{i}th
#' character in the string.
#' @param string A string.
#' @return A character vector.
#' @examples
#' str_to_vec("abcdef")
#' @export
str_to_vec <- function(string) {
  if (!is.character(string)) stop("string must be of character type")
  strsplit(string, NULL)[[1]]
}

#' Which strings match the patterns?
#'
#' Given a character vector of strings and one of patterns (in regular
#' expression), which of the strings match all (or any) of the patterns.
#'
#' For huge character vectors, this can be quite slow and you're better off
#' implementing your own solution with [stringr::str_detect()].
#'
#' @param strings A character vector.
#' @param patterns Regular expressions.
#' @param ignore_case Do we want to ignore case when matching patterns?
#' @param any Set this to `TRUE` if you want to see which strings match
#'   \emph{any} of the patterns and not \emph{all} (all is the default).
#'
#' @return A character vector of strings matching the patterns.
#'
#' @examples
#' str_with_patterns(c("abc", "bcd", "cde"), c("b", "c"))
#' str_with_patterns(c("abc", "bcd", "cde"), c("b", "c"), any = TRUE)
#' str_with_patterns(toupper(c("abc", "bcd", "cde")), c("b", "c"), any = TRUE)
#' str_with_patterns(toupper(c("abc", "bcd", "cde")), c("b", "c"), any = TRUE,
#'                   ignore_case = TRUE)
#' @export
str_with_patterns <- function(strings, patterns, ignore_case = FALSE,
                              any = FALSE) {
  if (!is.character(strings)) stop("strings must be of character type")
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

#' Text before or after \eqn{n}th occurrence of pattern.
#'
#' Extract the part of a string which is before or after the *n*th occurrence of
#' a specified pattern, vectorized over the string. `n` can be negatively
#' indexed. See 'Arguments'.
#'
#' \itemize{ \item `str_after_first(...)` is just `str_after_nth(..., n = 1)`.
#' \item `str_after_last(...)` is just `str_after_nth(..., n = -1)`. \item
#' `str_before_first(...)` is just `str_before_nth(..., n = 1)`. \item
#' `str_before_last(...)` is just `str_before_nth(..., n = -1)`. }
#'
#' @param strings A character vector.
#' @param pattern A regular expression.
#' @param n A natural number to identify the \eqn{n}th occurrence (defaults to
#'   first (`n = 1`)). This can be negatively indexed, so if you wish to select
#'   the \emph{last} occurrence, you need `n = -1`, for the second-last, you
#'   need `n = -2` and so on.
#' @return A character vector of the desired strings.
#' @examples
#' string <- "ab..cd..de..fg..h"
#' str_after_nth(string, "\\.\\.", 3)
#' str_before_nth(string, "e", 1)
#' str_before_nth(string, "\\.", -3)
#' str_before_nth(string, ".", -3)
#' str_before_nth(rep(string, 2), fixed("."), -3)
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
#' This is usually used to get the part of a file name that doesn't include the
#' file extension. It is vectorized over `string`. If there is no period in
#' `string`, the input is returned.
#'
#' @param string A character vector.
#'
#' @return A character vector.
#'
#' @examples
#' before_last_dot(c("spreadsheet1.csv", "doc2.doc", ".R"))
#'
#' @export
before_last_dot <- function(string) {
  string %>%
    tools::file_path_sans_ext() %T>% {
      .[(string == .) & (str_elem(., 1) == ".")] <- ""
    }
}

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
  if (length(extend_by) != 1) stop("extend.by must have length 1.")
  if (is.na(extend_by) && is.na(length_out)) {
    stop("One of extend.by or length.out must be specified.")
  }
  if (is.numeric(char_vec)) char_vec <- as.character(char_vec)
  if (!is.character(char_vec))
    stop("char.vec must be of numeric or character type.")
  if (!is.na(extend_by)) {
    if (!is.numeric(extend_by)) stop("If specified, extend.by must be numeric.")
    return(c(char_vec, rep("", extend_by)))
  }
  if (!is.na(length_out)) {
    if (!(is.numeric(length_out) && length_out >= length(char_vec))) {
      stop("If specified, length.out must be numeric and at least equal to ",
           "the length of char.vec.")
    }
    c(char_vec, rep("", length_out - length(char_vec)))
  }
}

#' Put specified strings in specified positions in an otherwise empty character
#' vector.
#'
#' Create a character vector with a set of strings at specified positions in
#' that character vector, with the rest of it taken up by empty strings.
#' @param strings A character vector of the strings to put in positions
#'   (coerced by [as.character] if not character already).
#' @param positions The indices of the character vector to be occupied by the
#'   elements of strings. Must be the same length as strings or of length 1.
#' @return A character vector.
#' @examples
#' put_in_pos(1:3, c(1, 8, 9))
#' put_in_pos(c("Apple", "Orange", "County"), c(5, 7, 8))
#' put_in_pos(1:2, 5)
#' @export
put_in_pos <- function(strings, positions) {
  ls <- length(strings)
  if (ls > 1 && length(positions) == 1) {
    positions <- positions:(positions + ls - 1)
  }
  strings <- as.character(strings)
  stopifnot(length(strings) == length(positions))
  out <- rep("", max(positions))
  out[positions] <- strings
  out
}

#' Trim something other than whitespace
#'
#' The `stringi` and `stringr` packages let you trim whitespace, but
#' what if you want to trim something else from either (or both) side(s) of a
#' string? This function lets you select which pattern to trim and from which
#' side(s).
#'
#' @param string A string.
#' @param pattern A string. The pattern to be trimmed (\emph{not} interpreted as
#'   regular expression). So to trim a period, use `char = "."` and not
#'   `char = "\\\\."`).
#' @param side Which side do you want to trim from? `"both"` is the
#'   default, but you can also have just either `"left"` or `"right"`
#'   (or optionally the shortened `"b"`, `"l"` and `"r"`).
#' @return A string.
#' @examples
#' trim_anything("..abcd.", ".", "left")
#' trim_anything("-ghi--", "-")
#' trim_anything("-ghi--", "--")
#' @export
trim_anything <- function(string, pattern, side = "both") {
  stopifnot(nchar(string) > 0)
  side <- tolower(side)  # give side some case lenience
  side <- match.arg(side, c("both", "left", "right"))
  pattern <- ore::ore.escape(pattern) %>%
    str_c("(?:", ., ")")
  switch(side,
         both = str_replace(string, str_c("^", pattern, "*"), "") %>%
           str_replace(str_c(pattern, "*$"), ""),
         left = str_replace(string, str_c("^", pattern, "*"), ""),
         right = str_replace(string, str_c(pattern, "*$"), "")
  )
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

#' Locate the braces in a string.
#'
#' Give the positions of `(`, `)`, `[`, `]`, `\{`, `\}` within a string.
#'
#' @param string A character vector
#'
#' @return A list of data frames, one for each member of the string character
#'   vector. Each data frame has a "position" and "brace" column which give the
#'   positions and types of braces in the given string.
#'
#' @examples
#' locate_braces(c("a{](kkj)})", "ab(]c{}"))
#' @export
locate_braces <- function(string) {
  locations <- str_locate_all(string, "[\\(\\)\\[\\]\\{\\}]") %>%
    int_lst_first_col()
  braces <- str_elems(string, locations)
  lst_df_pos_brace(locations, braces)
}

#' Remove the quoted parts of a string.
#'
#' If any parts of a string are quoted (between quotation marks), remove those
#' parts of the string, including the quotes. Run the examples and you'll know
#' exactly how this function works.
#'
#' @param string A character vector.
#'
#' @return A character vector.
#' @examples
#' string <- "\"abc\"67a\'dk\'f"
#' cat(string)
#' remove_quoted(string)
#' @export
remove_quoted <- function(string) {
  string <- str_replace_all(string, "(?:\".*?\")", "")
  string <- str_replace_all(string, "(?:\'.*?\')", "")
  string
}

#' Ensure a file name has the intended extension.
#'
#' Say you want to ensure a name is fit to be the name of a csv file. Then, if
#' the input doesn't end with ".csv", this function will tack ".csv" onto the
#' end of it. This is vectorized over the first argument.
#'
#' @param string The intended file name.
#' @param ext The intended file extension (with or without the ".").
#' @param replace If the file has an extension already, replace it (or append
#'   the new extension name)?
#'
#' @return A string: the file name in your intended form.
#'
#' @examples
#' give_ext(c("abc", "abc.csv"), "csv")
#' give_ext("abc.csv", "pdf")
#' give_ext("abc.csv", "pdf", replace = TRUE)
#' @export
give_ext <- function(string, ext, replace = FALSE) {
  stopifnot(is.character(string), length(ext) == 1)
  ext <- str_match(ext, "^\\.*(.*)")[, 2]
  if (replace) {
    string <- tools::file_path_sans_ext(string)
  } else {
    correct_ext <- str_detect(string, str_c("\\.", ext, "$"))
    string[correct_ext] <- tools::file_path_sans_ext(string[correct_ext])
  }
  str_c(string, ".", ext)
}

#' Split a string based on CamelCase
#'
#' Vectorized over `string`.
#'
#' @param string A character vector.
#' @param lower Do you want the output to be all lower case (or as is)?
#'
#' @return A list of character vectors, one list element for each element of
#'   `string`.
#'
#' @references Adapted from Ramnath Vaidyanathan's answer at
#' http://stackoverflow.com/questions/8406974/splitting-camelcase-in-r.
#'
#' @examples
#' str_split_camel_case(c("RoryNolan", "NaomiFlagg", "DepartmentOfSillyHats"))
#' @export
str_split_camel_case <- function(string, lower = FALSE) {
  string <- gsub("^[^[:alnum:]]+|[^[:alnum:]]+$", "", string)
  string <- gsub("(?!^)(?=[[:upper:]])", " ", string, perl = TRUE)
  if (lower) string <- tolower(string)
  str_split(string, " ")
}

#' Get the indices of the \eqn{n}th instance of a pattern.
#'
#' The \eqn{n}th instance of an pattern will cover a series of character
#' indices. These functions tell you which indices those are.
#'
#' \itemize{ \item `str_first_instance_indices(...)` is just
#' `str_nth_instance_indices(..., n = 1)`. \item
#' `str_last_instance_indices(...)` is just `str_nth_instance_indices(..., n =
#' -1)`. }
#'
#' @param string A character vector. These functions are vectorized over this
#'   argument.
#' @param pattern A character vector. Pattern(s) specified like the pattern(s)
#'   in the `stringr`` package (e.g. look at [stringr::str_locate()]). If this
#'   has length >1 its length must be the same as that of `string`.
#' @param n Then \eqn{n} for the \eqn{n}th instance of the pattern.
#'
#' @return A two-column matrix. The \eqn{i}th row of this matrix gives the start
#'   and end indices of the \eqn{n}th instance of `pattern` in the \eqn{i}th
#'   element of `string`.
#'
#' @examples
#' str_nth_instance_indices(c("abcdabcxyz", "abcabc"), "abc", 2)
#'
#' @export
str_nth_instance_indices <- function(string, pattern, n) {
  n %<>% as.integer()
  if (n == 0) stop("'n' cannot be zero.")
  instances <- str_locate_all(string, pattern)
  n_instances <- lengths(instances) / 2
  bad <- n_instances < abs(n)
  if (any(bad)) {
    first_bad <- match(T, bad)
    stop("There aren't ", abs(n),
         " instances of pattern in one or more of the strings. ",
         "The first such bad string is string ", first_bad, ": '",
         string[first_bad], "' which has ", n_instances[first_bad],
         " instances.")
  }
  l <- length(string)
  if (n > 0) {
    n %<>% rep(l)
  } else {
    n <- n_instances + n + 1
  }
  intmat_list_bind_nth_rows(instances, n - 1) %>%
    magrittr::set_colnames(c("start", "end"))
}

#' @rdname str_nth_instance_indices
#' @export
str_first_instance_indices <- function(string, pattern) {
  str_nth_instance_indices(string = string, pattern = pattern, n = 1)
}

#' @rdname str_nth_instance_indices
#' @export
str_last_instance_indices <- function(string, pattern) {
  str_nth_instance_indices(string = string, pattern = pattern, n = -1)
}

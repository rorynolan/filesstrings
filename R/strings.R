#' Check if a string could be considered as numeric.
#'
#' After padding is removed, could the input string be considered to be numeric,
#' i.e. could it be coerced to numeric. This function is vectorised over its one
#' argument.
#' @param string A character vector.
#' @return A character vector. `TRUE` if the argument can be considered to be numeric or `FALSE`
#'   otherwise.
#' @examples
#' CanBeNumeric("3")
#' CanBeNumeric("5 ")
#' CanBeNumeric(c("1a", "abc"))
#' @export
CanBeNumeric <- function(string) !is.na(suppressWarnings(as.numeric(string)))

#' Get the currencies of numbers within a string.
#'
#' The currency of a number is defined as the character coming before the number
#' in the string. If nothing comes before (i.e. if the number is the first thing
#' in the string), the currency is the empty string, similarly the currency can
#' be a space, comma or any manner of thing.
#' \itemize{
#'   \item `GetCurrency`
#' takes a string and returns the currency of the first number therein. It is
#' vectorised over string.
#'   \item `GetCurrencies` takes a string and returns
#' the currencies of all of the numbers within that string. It is not
#' vectorised. }
#'
#' These functions do not allow for leading decimal points.
#'
#' @param string A string.
#' @param strings A character vector.
#'
#' @return
#' \itemize{
#' \item `GetCurrency` returns a character vector.
#' \item `GetCurrencies` returns a data frame with one column for the
#' currency symbol and one for the amount.
#' }
#' @examples
#' GetCurrencies("35.00 $1.14 abc5 $3.8 77")
#' @export
GetCurrencies <- function(string) {
  stopifnot(is.character(string), length(string) == 1)
  ssbn <- StrSplitByNums(string, decimals = TRUE, negs = TRUE)[[1]]
  num.indices <- which(CanBeNumeric(ssbn))
  numbers <- as.numeric(ssbn[num.indices])
  before.num.indices <- num.indices - 1
  before.num.strings <- vapply(before.num.indices, function(x) {
    ifelse(x %in% num.indices || x < 1, "", ssbn[x])
  }, character(1))
  currencies <- StrElem(before.num.strings, -1)
  tibble::tibble(currency = currencies, amount = numbers)
}

#' @rdname GetCurrencies
#' @examples
#' GetCurrency(c("ab3 13", "$1"))
#' @export
GetCurrency <- function(strings) {
  num.starts <- str_locate(strings, "[0-9]")[, "start"]
  before.indices <- num.starts - 1
  StrElem(strings, before.indices)
}

#' Remove back-to-back duplicates of a pattern in a string.
#'
#' If a string contains a given pattern duplicated back-to-back a number of
#' times, remove that duplication, leaving the pattern appearing once in that
#' position (works if the pattern is duplicated in different parts of a string,
#' removing all instances of duplication). This is vectorised over string and
#' pattern.
#' @param string A character vector. The string(s) to be purged of duplicates.
#' @param pattern A character vector. Tegular expression(s) of the pattern(s)
#'   that we wish not to be duplicated.
#' @return The string with the duplicates fixed.
#' @examples
#' DuplicatesToSingles("abc//def", "/")
#' DuplicatesToSingles("abababcabab", "ab")
#' DuplicatesToSingles(c("abab", "cdcd"), "cd")
#' DuplicatesToSingles(c("abab", "cdcd"), c("ab", "cd"))
#' @export
DuplicatesToSingles <- function(string, pattern) {
  dup.patt <- str_c("(", pattern, ")+")
  str_replace_all(string, dup.patt, pattern)
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
#' NiceNums(strings)
#'
#' NiceNums(c("abc9def55", "abc10def7"))
#' NiceNums(c("01abc9def55", "5abc10def777", "99abc4def4"))
#'
#' \dontrun{
#' NiceNums(c("abc9def55", "abc10xyz7"))}
#' @export
NiceNums <- function(strings) {
  if (!is.character(strings)) stop("str.vec must be a vector of strings")
  non.nums <- ExtractNonNumerics(strings)
  if (!AllEqual(non.nums)) {
    stop("The non-number bits of the strings are different.")
  }
  nums <- ExtractNumbers(strings, leave.as.string = TRUE)
  if (!AllEqual(lengths(nums))) {
    stop("Some of the strings contain different numbers of numbers")
  }
  nums <- simplify2array(nums)
  if (!is.matrix(nums)) nums <- t(nums)
  ncn <- nchar(nums)
  max.lengths <- matrixStats::rowMaxs(ncn)
  min.length <- min(ncn)
  to.prefix <- rep("0", max(max.lengths) - min.length) %>% str_c(collapse = "")
  nums <- str_c(to.prefix, nums)
  starts <- -rep(max.lengths, ncol(ncn))
  nums <- str_sub(nums, starts, -1) %>%
    split(rep(seq_len(ncol(ncn)), each = nrow(ncn)))
  num.first <- StrElem(strings, 1) %>% CanBeNumeric
  if (!AllEqual(num.first)) {
    stop("Some strings start with numbers and some don't")
  }
  if (num.first[1]) {
    interleaves <- InterleaveStringList(nums, non.nums)
  } else {
    interleaves <- InterleaveStringList(non.nums, nums)
  }
  PasteCollapseListElems(interleaves)
}

#' Extract numbers (or non-numbers) from a string.
#'
#' `ExtractNumbers` extracts the numbers (or non-numbers) from a string where
#' decimals are optionally allowed. `ExtractNonNumerics` extracts the bits of
#' the string that aren't extracted by `ExtractNumbers`. `NthNumber` is a
#' convenient wrapper for `ExtractNumbers`, allowing you to choose which number
#' you want. Similarly `NthNonNumeric`. Please view the examples at the bottom
#' of this page to ensure that you understand how these functions work, and
#' their limitations. These functions are vectorised over `string`.
#'
#' If any part of a string contains an ambiguous number (e.g. `1.2.3` would be
#' ambiguous if `decimals = TRUE` (but not otherwise)), the value returned for
#' that string will be `NA`. Note that these functions do not know about
#' scientific notation (e.g. `1e6` for 1000000).
#'
#' @param string A string.
#' @param leave.as.string Do you want to return the number as a string (`TRUE`)
#'   or as numeric (`FALSE`, the default)?
#' @param decimals Do you want to include the possibility of decimal numbers
#'   (`TRUE`) or not (`FALSE`, the default).
#' @param leading.decimals Do you want to allow a leading decimal point to be
#'   the start of a number?
#' @param negs Do you want to allow negative numbers? Note that double negatives
#'   are not handled here (see the examples).
#' @return For `ExtractNumbers` and `ExtractNonNumerics`, a list of numeric or
#'   character vectors, one list element for each element of `string`. For
#'   `NthNumber` and `NthNonNumeric`, a vector the same length as `string` (as
#'   in `length(string)`, not `nchar(string)`).
#' @examples
#' ExtractNumbers(c("abc123abc456", "abc1.23abc456"))
#' ExtractNumbers(c("abc1.23abc456", "abc1..23abc456"), decimals = TRUE)
#' ExtractNumbers("abc1..23abc456", decimals = TRUE)
#' ExtractNumbers("abc1..23abc456", decimals = TRUE, leading.decimals = TRUE)
#' ExtractNumbers("abc1..23abc456", decimals = TRUE, leading.decimals = TRUE,
#' leave.as.string = TRUE)
#' ExtractNumbers("-123abc456")
#' ExtractNumbers("-123abc456", negs = TRUE)
#' ExtractNumbers("--123abc456", negs = TRUE)
#' ExtractNonNumerics("abc123abc456")
#' ExtractNonNumerics("abc1.23abc456")
#' ExtractNonNumerics("abc1.23abc456", decimals = TRUE)
#' ExtractNonNumerics("abc1..23abc456", decimals = TRUE)
#' ExtractNonNumerics("abc1..23abc456", decimals = TRUE,
#' leading.decimals = TRUE)
#' ExtractNonNumerics(c("-123abc456", "ab1c"))
#' ExtractNonNumerics("-123abc456", negs = TRUE)
#' ExtractNonNumerics("--123abc456", negs = TRUE)
#' ExtractNumbers(c(rep("abc1.2.3", 2), "a1b2.2.3", "e5r6"), decimals = TRUE)
#' ExtractNumbers("ab.1.2", decimals = TRUE, leading.decimals = TRUE)
#' NthNumber("abc1.23abc456", 2)
#' NthNumber("abc1.23abc456", 2, decimals = TRUE)
#' NthNumber("-123abc456", -2, negs = TRUE)
#' ExtractNonNumerics("--123abc456", negs = TRUE)
#' NthNonNumeric("--123abc456", 1)
#' NthNonNumeric("--123abc456", -2)
#' @export
ExtractNumbers <- function(string, leave.as.string = FALSE, decimals = FALSE,
                           leading.decimals = FALSE, negs = FALSE) {
  if (leading.decimals == TRUE && decimals == FALSE) {
    stop("To allow leading decimals, you need to first allow decimals.")
  }
  stopifnot(is.character(string))
  if (decimals) {
    pattern <- "(?:[0-9]+(?:\\.?[0-9]+)*)+"
    if (leading.decimals) pattern <- str_c("\\.?", pattern)
  } else {
    pattern <- "[0-9]+"
  }
  if (negs) pattern <- str_c("-?", pattern)
  numbers <- str_extract_all(string, pattern)
  numerics <- suppressWarnings(lapply(numbers, as.numeric))
  na.pos <- vapply(numerics, anyNA, logical(1))
  if (leave.as.string) {
    numbers[na.pos] <- NA_character_
  } else {
    numbers <- numerics
    numbers[na.pos] <- NA_real_
  }
  numbers
}

#' @rdname ExtractNumbers
#' @export
ExtractNonNumerics <- function(string, decimals = FALSE,
                               leading.decimals = FALSE, negs = FALSE) {
  if (leading.decimals == TRUE && decimals == FALSE) {
    stop("To allow leading decimals, you need to first allow decimals.")
  }
  stopifnot(is.character(string))
  if (decimals) {
    pattern <- "(?:[0-9]+(?:\\.?[0-9]+)*)+"
    if (leading.decimals) pattern <- str_c("\\.?", pattern)
  } else {
    pattern <- "[0-9]+"
  }
  if (negs) pattern <- str_c("-?", pattern)
  non.numerics <- str_split(string, pattern) %>% StrListRemoveEmpties
  numerics <- ExtractNumbers(string, decimals = decimals,
                             leading.decimals = leading.decimals, negs = negs)
  na.pos <- vapply(numerics, anyNA, logical(1))
  non.numerics[na.pos] <- NA_character_
  non.numerics
}

#' @param n The index of the number (or non-numeric) that you seek. Negative
#'   indexing is allowed i.e. `n = 1` (the default) will give you the first
#'   number (or non-numeric) whereas `n = -1` will give you the last number (or
#'   non-numeric), `n = -2` will give you the second last number and so on.
#' @rdname ExtractNumbers
#' @export
NthNumber <- function(string, n = 1, leave.as.string = FALSE, decimals = FALSE,
                      leading.decimals = FALSE, negs = FALSE) {
  # this function doesn't work for strings with decimal numbers
  if (n == 0) stop("n must be a nonzero integer.")
  numbers <- ExtractNumbers(string, leave.as.string = TRUE, negs = negs,
                      decimals = decimals, leading.decimals = leading.decimals)
  nth.numbers <- CharListElemsNthElem(numbers, n)
  if (leave.as.string) {
    nth.numbers
  } else {
    as.numeric(nth.numbers)
  }
}

#' @rdname ExtractNumbers
#' @export
NthNonNumeric <- function(string, n = 1, decimals = FALSE,
                          leading.decimals = FALSE, negs = FALSE) {
  if (n == 0) stop("n must be a nonzero integer.")
  non.numerics <- ExtractNonNumerics(string, decimals = decimals, negs = negs,
                                     leading.decimals = leading.decimals)
  CharListElemsNthElem(non.numerics, n)
}

#' Split a string by its numeric charachters.
#'
#' Break a string wherever you go from a numeric charachter to a non-numeric or
#' vice-versa.
#' @inheritParams ExtractNumbers
#' @examples
#' StrSplitByNums(c("abc123def456.789gh", "a1b2c344"))
#' StrSplitByNums("abc123def456.789gh", decimals = TRUE)
#' StrSplitByNums("22")
#' @export
StrSplitByNums <- function(string, decimals = FALSE, leading.decimals = FALSE,
                           negs = FALSE) {
  nums <- ExtractNumbers(string, leave.as.string = TRUE, decimals = decimals,
                         leading.decimals = leading.decimals, negs = negs)
  if (all(CanBeNumeric(string))) {
    non.nums <- list(character(0))[rep(1, length(string))]
  } else {
    non.nums <- ExtractNonNumerics(string, decimals = decimals, negs = negs,
                                   leading.decimals = leading.decimals)
  }
  CorrectInterleave(string, non.nums, nums)
}


#' Extract a single character of a string, using its index.
#'
#' @param string A string.
#' @param index An integer. Negative indexing is allowed as in
#'   [stringr::str_sub()].
#' @return A one-character string.
#' @examples
#' StrElem("abcd", 3)
#' StrElem("abcd", -2)
#' @export
StrElem <- function(string, index) {
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
#' StrElemsPasted("abcdef", c(2, 5:6))
#' @export
StrElemsPasted <- function(string, indices) {
  if (!is.character(string)) stop("string must be of character type")
  stopifnot(length(string) == 1)
  elems <- StrElem(string, indices)
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
#' StringToVec("abcdef")
#' @export
StringToVec <- function(string) {
  if (!is.character(string)) stop("string must be of character type")
  strsplit(string, NULL)[[1]]
}

#' Which strings match the patterns?
#'
#' Given a character vector of strings and one of patterns (in regular
#' expression), which of the strings match all (or any) of the patterns.
#' @param strings A character vector.
#' @param patterns Regular expressions.
#' @param ignore.case Do we want to ignore case when matching patterns?
#' @param any Set this to `TRUE` if you want to see which strings match
#'   \emph{any} of the patterns and not \emph{all} (all is the default).
#' @return A character vector of strings matching the patterns.
#' @examples
#' StringsWithPatterns(c("abc", "bcd", "cde"), c("b", "c"))
#' StringsWithPatterns(c("abc", "bcd", "cde"), c("b", "c"), any = TRUE)
#' StringsWithPatterns(toupper(c("abc", "bcd", "cde")), c("b", "c"), any = TRUE)
#' StringsWithPatterns(toupper(c("abc", "bcd", "cde")), c("b", "c"), any = TRUE,
#' ignore.case = TRUE)
#' @export
StringsWithPatterns <- function(strings, patterns, ignore.case = FALSE,
                                any = FALSE) {
  if (!is.character(strings)) stop("strings must be of character type")
  strings.orig <- strings
  if (ignore.case) {
    strings <- tolower(strings)
    patterns <- tolower(patterns)
  }
  matches <- as.matrix(vapply(strings, str_detect, logical(length(patterns)),
                              patterns))
  if (any) {
    keeps <- apply(matches, 2, any)
  } else {
    keeps <- apply(matches, 2, all)
  }
  return(strings.orig[keeps])
}

#' Text before or after \eqn{n}th occurrence of pattern.
#'
#' Extract the part of a string which is before or after the \eqn{n}th
#' occurrence of a specified pattern, vectorised over the string. One can also
#' choose \eqn{n} to be the \emph{last} occurrence of the pattern. See argument
#' `n`.
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
#' StrAfterNth(string, "\\.\\.", 3)
#' StrBeforeNth(string, "e", 1)
#' StrBeforeNth(string, "\\.", -3)
#' StrBeforeNth(string, ".", -3)
#' StrBeforeNth(rep(string, 2), fixed("."), -3)
#' @export
StrAfterNth <- function(strings, pattern, n = 1) {
  nth.instance.indices <- StrNthInstanceIndices(strings, pattern, n)
  mapply(str_sub, strings, nth.instance.indices[, "end"] + 1, nchar(strings))
}

#' @rdname StrAfterNth
#' @export
StrBeforeNth <- function(strings, pattern, n = 1) {
  nth.instance.indices <- StrNthInstanceIndices(strings, pattern, n)
  mapply(str_sub, strings, 1, nth.instance.indices[, "start"] - 1)
}

#' Get the part of a string before the last period.
#'
#' This is usually used to get the part of a file name that doesn't include the
#' file extension. It is vectorised over `string`.
#'
#' @param string A character vector.
#'
#' @return A character vector.
#'
#' @examples
#' BeforeLastDot(c("spreadsheet1.csv", "doc2.doc"))
#'
#' @export
BeforeLastDot <- function(string) {
  StrBeforeNth(string, coll("."), -1)
}

#' Pad a character vector with empty strings.
#'
#' Extend a character vector by appending empty strings at the end.
#'
#' @param char.vec A character vector. The thing you wish to expand.
#' @param extend.by A non-negative integer. By how much do you wish to extend
#'   the vector?
#' @param length.out A positive integer. How long do you want the output vector
#'   to be?
#'
#' @return A character vector.
#'
#' @examples
#' ExtendCharVec(1:5, extend.by = 2)
#' ExtendCharVec(c("a", "b"), length.out = 10)
#' @export
ExtendCharVec <- function(char.vec, extend.by = NA, length.out = NA) {
  if (length(extend.by) != 1) stop("extend.by must have length 1.")
  if (is.na(extend.by) && is.na(length.out)) {
    stop("One of extend.by or length.out must be specified.")
  }
  if (is.numeric(char.vec)) char.vec <- as.character(char.vec)
  if (!is.character(char.vec))
    stop("char.vec must be of numeric or character type.")
  if (!is.na(extend.by)) {
    if (!is.numeric(extend.by)) stop("If specified, extend.by must be numeric.")
    return(c(char.vec, rep("", extend.by)))
  }
  if (!is.na(length.out)) {
    if (!(is.numeric(length.out) && length.out >= length(char.vec))) {
      stop("If specified, length.out must be numeric and at least equal to ",
           "the length of char.vec.")
    }
    c(char.vec, rep("", length.out - length(char.vec)))
  }
}

#' Paste vectors/files with different lengths/numbers of lines.
#'
#' Paste character vectors of different lengths, optionally inputting the
#' vectors as file names, which become character vectors via `readLines`.
#' Vectors are first extended to all be the same length via
#' [ExtendCharVec()] and are then pasted together, with no separator
#' put in when pasting empty strings. See the examples if you don't understand.
#'
#' @param files A character vector of files to be read in via `readLines`
#'   to be pasted. If you would like to use this function on vectors already in
#'   the environment (without reading in files), pass them into this argument as
#'   a list (see the examples).
#' @param sep What (if anything) do you want to paste in between things as
#'   you're pasting them together.
#' @return A character vector.
#' @examples
#' PasteDifferentLengths(list(1:3, 1:4))
#' PasteDifferentLengths(list(1:3, 1:4), sep = "sSs")
#' writeLines(as.character(1:3), "PasteDifferentLengths1.txt")
#' writeLines(as.character(1:4), "PasteDifferentLengths2.txt")
#' PasteDifferentLengths(list.files(pattern = "PasteDifferentLengths"),
#' sep = "sSsepPp")
#' # clean up working directory
#' file.remove(list.files(pattern = "PasteDifferentLengths"))
#' @export
PasteDifferentLengths <- function(files, sep = "") {
  if (is.character(files)) {
    files <- lapply(files, readLines)
  }
  if (!is.list(files)) stop("files must be either a character vector or ",
                            "a list of objects.")
  max.length <- max(vapply(files, length, integer(1)))
  files <- vapply(files, ExtendCharVec, character(max.length),
                  length.out = max.length)
  if (length(sep) != 1) stop("sep must have length 1.")
  if (sep == "") {
    apply(files, 1, paste, collapse = "")
  } else {
    apply(files, 1, function(x) paste(x[as.logical(nchar(x))], collapse = sep))
  }
}

#' Put specified strings in specified positions
#'
#' Create a charachter vector with a set of strings at specified positions in
#' that charachter vector, with the rest of it taken up by empty strings.
#' @param strings A charachter vector of the strings to put in positions
#'   (coerced by [as.character] if not charachter already).
#' @param positions The indices of the charachter vector to be occupied by the
#'   elements of strings. Must be the same length as strings or of length 1.
#' @return A charachter vector.
#' @examples
#' PutInPos(1:3, c(1, 8, 9))
#' PutInPos(c("Apple", "Orange", "County"), c(5, 7, 8))
#' PutInPos(1:2, 5)
#' @export
PutInPos <- function(strings, positions) {
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
#'   (or optionally the shorthands `"b"`, `"l"` and `"r"`).
#' @return A string.
#' @examples
#' TrimAnything("..abcd.", ".", "left")
#' TrimAnything("-ghi--", "-")
#' TrimAnything("-ghi--", "--")
#' @export
TrimAnything <- function(string, pattern, side = "both") {
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
#' Vectorised over `string` and pattern.
#'
#' @param string A character vector.
#' @param pattern A character vector. Pattern(s) specified like the pattern(s)
#'   in the stringr package (e.g. look at [stringr::str_locate()]). If
#'   this has length >1 its length must be the same as that of `string`.
#'
#' @return A numeric vector giving the number of matches in each string.
#' @examples
#' CountMatches("abacad", "a")
#' CountMatches("2.1.0.13", ".")
#' CountMatches("2.1.0.13", stringr::coll("."))
#' @export
CountMatches <- function(string, pattern) {
  lp <- length(pattern)
  ls <- length(string)
  if (lp > 1) {
    if (lp != ls) {
      stop("If pattern has length greater than 1, ",
           "it must have the same length as string.")
    }
  }
  locations <- str_locate_all(string, pattern)
  n_matches <- vapply(locations, nrow, integer(1)) %>% sum
  n_matches
}

#' Locate the braces in a string.
#'
#' Give the positions of `(`, `)`, `[`, `]`, `\{`, `\}` within a string.
#'
#' @param string A character vector
#'
#' @return A list of data frames ([tibble][tibble::tibble]s), one for each member
#'   of the string character vector. Each data frame has a "position" and
#'   "brace" column which give the positions and types of braces in the given
#'   string.
#'
#' @examples
#' LocateBraces(c("a{](kkj)})", "ab(]c{}"))
#' @export
LocateBraces <- function(string) {
  locations <- str_locate_all(string, "[\\(\\)\\[\\]\\{\\}]") %>%
    lapply(function(x) x[, 1])
  braces <- mapply(StrElem, string, locations, SIMPLIFY = FALSE)
  dfs <- mapply(function(x, y) tibble::tibble(position = x, brace = y),
                locations, braces, SIMPLIFY = FALSE)
  dfs
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
#' RemoveQuoted(string)
#' @export
RemoveQuoted <- function(string) {
  string <- str_replace_all(string, "(?:\".*?\")", "")
  string <- str_replace_all(string, "(?:\'.*?\')", "")
  string
}

#' Ensure a file name has the intended extension.
#'
#' Say you want to ensure a name is fit to be the name of a csv file. Then, if
#' the input doesn't end with ".csv", this function will tack ".csv" onto the
#' end of it. This is vectorised over the first argument.
#'
#' @param string The intended file name.
#' @param ext The intended file extension (with or without the ".").
#' @param replace If the file has an extension already, replace it (or append
#'   the new extension name)?
#'
#' @return A string: the file name in your intended form.
#'
#' @examples
#' GiveExt(c("abc", "abc.csv"), "csv")
#' GiveExt("abc.csv", "pdf")
#' GiveExt("abc.csv", "pdf", replace = TRUE)
#' @export
GiveExt <- function(string, ext, replace = FALSE) {
  stopifnot(is.character(string), length(ext) == 1)
  ext <- str_match(ext, "^\\.*(.*)")[, 2]
  if (replace) {
    string <- tools::file_path_sans_ext(string)
  } else {
    correct.ext <- str_detect(string, str_c("\\.", ext, "$"))
    string[correct.ext] <- tools::file_path_sans_ext(string[correct.ext])
  }
  str_c(string, ".", ext)
}

#' Split a string based on camelcase
#'
#' Vectorised over `string`.
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
#' SplitCamelCase(c("RoryNolan", "NaomiFlagg", "DepartmentOfSillyHats"))
#' @export
SplitCamelCase <- function(string, lower = FALSE) {
  string <- gsub("^[^[:alnum:]]+|[^[:alnum:]]+$", "", string)
  string <- gsub("(?!^)(?=[[:upper:]])", " ", string, perl = TRUE)
  if (lower) string <- tolower(string)
  str_split(string, " ")
}

StrNthInstanceIndices <- function(strings, pattern, n) {
  instances <- str_locate_all(strings, pattern)
  instances %>% vapply(function(x, n) {
    l <- length(x)
    if (n < 0) n <- nrow(x) + n + 1
    if (n < 1 || n > l) {
      stop("There aren't n instances of pattern in one or more of the strings.")
    }
    x[n, ]
  }, integer(2), n) %>% t
}

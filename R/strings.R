#' Check if a string could be considered as numeric.
#'
#' After padding is removed, could the input string be considered to be numeric,
#' i.e. could it be constructed via \code{toString(num)} where \code{num} is
#' numeric. This function is vectorised over its one argument.
#' @param A character vector of any length
#' @return \code{TRUE} if the argument can be considered to be numeric or
#'   \code{FALSE} otherwise.
#' @examples
#' CanBeNumeric("3")
#' CanBeNumeric("5 ")
#' CanBeNumeric(c("1a", "abc"))
CanBeNumeric <- function(string) !is.na(suppressWarnings(as.numeric(string)))

#' Remove back-to-back duplicates of a pattern in a string.
#'
#' If a string contains a given pattern duplicated back-to-back a number of
#' times, remove that duplication, leaving the pattern appearing once in that
#' position (works if the pattern is duplicated in different parts of a string,
#' removing all instances of duplication).
#' @param string The string to be purged of duplicates.
#' @param pattern A regular expression of the pattern that we wish not to be
#'   duplicated.
#' @return The string with the duplicates fixed.
#' @examples
#' DuplicatesToSingles("abc//def", "/")
#' DuplicatesToSingles("abababcabab", "ab")
DuplicatesToSingles <- function(string, pattern) {
  old <- string
  double <- str_c(pattern, pattern)
  new <- str_replace_all(string, double, pattern)
  while (!AllEqual(new, old)) {
    old <- new
    new <- str_replace_all(new, double, pattern)
  }
  new
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
#' @param str.vec A vector of strings.
#' @examples
#' strings <- paste0("abc", 1:12)
#' strings
#' NiceNums(strings)
#'
#' NiceNums(c("abc9def55", "abc10def9"))
#'
#' \dontrun{
#' NiceNums(c("abc9def55", "abc10xyz9"))}
NiceNums <- function(str.vec) {
  if (!is.character(str.vec)) stop("str.vec must be a vector of strings")
  strings <- lapply(str.vec, ExtractNonNumerics)
  if (!AllEqual(strings)) stop("The non-number bits of the strings are different.")
  nums <- lapply(str.vec, ExtractNumbers)
  if (!AllEqual(sapply(nums, length))) stop("some of the strings contain different numbers of numbers")
  nums <- simplify2array(nums)
  if (is.vector(nums)) nums <- t(as.matrix(nums))
  max.lengths <- apply(nums, 1, function(x) max(nchar(x)))
  for (i in 1:nrow(nums)) {  # add leading zeroes to numbers as necessary
    for (j in 1:ncol(nums)) {
      while(nchar(nums[i, j]) < max.lengths[i]) {
        nums[i, j] <- paste0(0, nums[i, j])
      }
    }
  }
  num.first <- sapply(str.vec, function(x) CanBeNumeric(StrElem(x, 1)))
  if (!AllEqual(num.first)) stop("some file names start with numbers and some don't")
  numbers <- Mat2ColList(nums)
  if (num.first[1]) {
    interleaves <- mapply(Interleave, numbers, strings)
  } else {
    interleaves <- mapply(Interleave, strings, numbers)
  }
  new.names <- apply(interleaves, 2, str_c, collapse = "")
  return(new.names)
}

#' Extract numbers (or non-numbers) from a string.
#'
#' \code{ExtractNumbers} extracts the numbers (or non-numbers) from a string
#' where decimals are optionally allowed. \code{ExtractNonNumerics} extracts the
#' bits of the string that aren't extracted by \code{ExtractNumbers}.
#' \code{NthNumber} is a convenient wrapper for \code{ExtractNumbers}, allowing
#' you to choose which number you want. Please view the examples at the bottom
#' of this page to ensure that you understand how these functions work, and
#' their limitations.
#'
#' \code{ExtractNonNumerics} uses \code{ExtractNumerics} to tell it what the
#' numbers in the string are and then it extracts the bits in between those
#' numbers. For this reason, errors you see whilst using
#' \code{ExtractNonNumerics} might be errors from \code{ExtractNumerics}.
#'
#' @param string A string.
#' @param leave.as.string Do you want to return the number as a string
#'   (\code{TRUE}) or as numeric (\code{FALSE}, the default)?
#' @param decimals Do you want to include the possibility of decimal numbers
#'   (\code{TRUE}) or not (\code{FALSE}, the default).
#' @param leading.decimals Do you want to allow a leading decimal point to be
#'   the start of a number?
#' @param negs Do you want to allow negative numbers? Note that double negatives
#'   are not handled here (see the examples).
#' @examples
#' ExtractNumbers("abc123abc456")
#' ExtractNumbers("abc1.23abc456")
#' ExtractNumbers("abc1.23abc456", decimals = TRUE)
#' ExtractNumbers("abc1..23abc456", decimals = TRUE)
#' ExtractNumbers("abc1..23abc456", decimals = TRUE, leading.decimals = TRUE)
#' ExtractNumbers("-123abc456")
#' ExtractNumbers("-123abc456", negs = TRUE)
#' ExtractNumbers("--123abc456", negs = TRUE)
#' ExtractNumbers("abc1.2.3", decimals = TRUE, err.na = TRUE)
#' ExtractNonNumerics("abc123abc456")
#' ExtractNonNumerics("abc1.23abc456")
#' ExtractNonNumerics("abc1.23abc456", decimals = TRUE)
#' ExtractNonNumerics("abc1..23abc456", decimals = TRUE)
#' ExtractNonNumerics("abc1..23abc456", decimals = TRUE, leading.decimals = TRUE)
#' ExtractNonNumerics("-123abc456")
#' ExtractNonNumerics("-123abc456", negs = TRUE)
#' ExtractNonNumerics("--123abc456", negs = TRUE)
#' ExtractNumbers("abc1.2.3", decimals = TRUE)
#' ExtractNumbers("ab.1.2", decimals = TRUE, leading.decimals = TRUE)
#' NthNumber("abc1.23abc456", 2)
#' NthNumber("abc1.23abc456", 2, decimals = TRUE)
#' NthNumber("-123abc456", -2, negs = TRUE)
#' ExtractNonNumerics("--123abc456", negs = TRUE)
ExtractNumbers <- function(string, leave.as.string = FALSE, decimals = FALSE,
                           leading.decimals = FALSE, negs = FALSE) {
  if (leading.decimals == TRUE && decimals == FALSE) {
    stop("To allow leading decimals, you need to first allow decimals.")
  }
  stopifnot(length(string) == 1)
  stopifnot(is.character(string))
  if (decimals) {
    pattern <- "([0-9]+(\\.?[0-9]+)*)+"
    if (leading.decimals) pattern <- str_c("\\.?", pattern)
  } else {
    pattern <- "[0-9]+"
  }
  if (negs) pattern <- str_c("-?", pattern)
  numbers <- str_extract_all(string, pattern)[[1]]
  n.periods <- sapply(str_extract_all(numbers, "\\."), length)
  if (any(n.periods > 1)) {
    bad.num <- numbers[match(T, n.periods > 1)]
    warning.message <- str_c("There was at least one ambiguity, the first of which was '",
                             bad.num,
                             "' so returning NA.")
    warning(warning.message)
    return(NA)
  }
  if (!leave.as.string) numbers <- as.numeric(numbers)
  numbers
}

#' @rdname ExtractNumbers
ExtractNonNumerics <- function(string, decimals = FALSE,
                               leading.decimals = FALSE, negs = FALSE) {
  stopifnot(is.character(string) && length(string) == 1)
  if (!any(CanBeNumeric(StringToVec(string)))) return(string)
  numerics <- ExtractNumbers(string, leave.as.string = TRUE,
                             decimals = decimals, leading.decimals = leading.decimals,
                             negs = negs)
  if (is.na(numerics[1])) return(NA)
  if (string == numerics[1]) return(StopOrNA(!err.na, "No non-numerics for ExtractNonNumerics to find."))
  non.numerics <- string  # this non.numerics will eventually be at the value we want, but here it is not
  for (n in numerics) {
    split <- str_split_fixed(Last(non.numerics), n, 2)
    non.numerics <- c(non.numerics[-length(non.numerics)], split)
  }
  non.numerics <- non.numerics[as.logical(nchar(non.numerics))]  # remove empty strings (str_split can produce those)
  return(non.numerics)
}

#' @param n The index of the number (or non-numeric) that you seek. Negative
#'   indexing is allowed i.e. \code{n = 1} will give you the first number (or non-numeric)
#'   whereas \code{n = -1} will give you the last number (or non-numeric), \code{n = -2} will
#'   give you the second last number and so on.
#' @rdname ExtractNumbers
NthNumber <- function(string, n, leave.as.string = FALSE, decimals = FALSE,
                      leading.decimals = FALSE, negs = FALSE) {
  # this function doesn't work for strings with decimal numbers
  if (n == 0) stop("n must be a nonzero integer.")
  numbers <- ExtractNumbers(string, leave.as.string = leave.as.string,
                            decimals = decimals, leading.decimals = leading.decimals,
                            negs = negs)
  l <- length(numbers)
  if (n > l) stop("There aren't |n| numbers in string.")
  if (n > 0) {
    nth.number <- numbers[n]
  } else {
    nth.number <- numbers[l + n + 1]
  }
  nth.number
}

#' @rdname ExtractNumbers
NthNonNumeric <- function(string, n, leave.as.string = FALSE, decimals = FALSE,
                      leading.decimals = FALSE, negs = FALSE) {
  # this function doesn't work for strings with decimal numbers
  if (n == 0) stop("n must be a nonzero integer.")
  non.numerics <- ExtractNonNumerics(string, decimals = decimals, negs = negs,
                                     leading.decimals = leading.decimals)
  l <- length(non.numerics)
  if (n > l) stop("There aren't |n| non-numerics in string.")
  if (n > 0) {
    nth.non.numeric <- non.numerics[n]
  } else {
    nth.non.numeric <- non.numerics[l + n + 1]
  }
  nth.non.numeric
}

#' Split a string by its numeric charachters.
#'
#' Break a string wherever you go from a numeric charachter to a non-numeric or
#' vice-versa.
#' @param string A string.
#' @examples
#' StrSplitByNumerics("abc123def456.789gh")
StrSplitByNums <- function(string) {
  nums <- ExtractNumbers(string, leave.as.string = TRUE, err.na = TRUE)
  if (is.na(nums[1])) return(string)
  non.nums <- ExtractNonNumerics(string)
  if (CanBeNumeric(StrElem(string, 1))) {
    split <- Interleave(nums, non.nums)
  } else {
    split <- Interleave(non.nums, nums)
  }
  split
}

#' Extract a single character of a string, using its index.
#'
#' @param string A string.
#' @param index A positive integer.
#' @return A one-character string.
#' @examples
#' StrElem("abcd", 3)
StrElem <- function(string, index) {
  if (!is.character(string)) stop("string must be of character type")
  str_sub(string, index, index)
}

#' Extract bits of a string and paste them together
#'
#' Extract characters - specified by their indices - from a string and paste them together
#' @param string A string.
#' @param indices A numeric vector of positive integers detailing the indices of the characters of \code{string} that we wish to paste together.
#' @return A string.
#' @examples
#' StrElemsPasted("abcdef", c(2, 5:6))
StrElemsPasted <- function(string, elem.indices) {
  if (!is.character(string)) stop("string must be of character type")
  elems <- sapply(elem.indices, StrElem, string = string)
  pasted <- paste(elems, collapse = "")
  return(pasted)
}

#' Convert a string to a vector of characters
#'
#' Go from a string to a vector whose \eqn{i}th element is the \eqn{i}th character in the string.
#' @param string A string.
#' @return A character vector.
#' @examples
#' StringToVec("abcdef")
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
#' @param any Set this to \code{TRUE} if you want to see which strings match \emph{any} of the patterns and not \emph{all} (all is the default).
#' @return A character vector of strings matching the patterns.
#' @examples
#' StringsWithPatterns(c("abc", "bcd", "cde"), c("b", "c"))
#' StringsWithPatterns(c("abc", "bcd", "cde"), c("b", "c"), any = TRUE)
StringsWithPatterns <- function(strings, patterns, ignore.case = FALSE, any = FALSE) {
  if (!is.character(strings)) stop("strings must be of character type")
  strings.orig <- strings
  if (ignore.case) {
    strings <- tolower(strings)
    patterns <- tolower(patterns)
  }
  matches <- sapply(strings, str_detect, patterns)
  if (any) {
    keeps <- apply(matches, 2, any)
  } else {
    keeps <- apply(matches, 2, all)
  }
  return(strings.orig[keeps])
}

#' Reverse a string.
#'
#' Reverse the order of a string; put it backwards.
#' @param string A string.
#' @return A string.
#' @examples
#' StrReverse("abcdef")
StrReverse <- function(string) {
  if (!is.character(string)) stop("string must be of character type")
  char.vec <- StringToVec(string)
  char.vec.reversed <- rev(char.vec)
  string.reversed <- paste(char.vec.reversed, collapse = "")
  return(string.reversed)
}

#' Text before or after \eqn{n}th occurrence of pattern.
#'
#' Extract the part of a string which is before or after the \eqn{n}th occurrence
#' of a specified pattern, vectorised over the string. One can also choose
#' \eqn{n} to be the \emph{last} occurrence of the pattern. See argument
#' \code{n}.
#'
#' @param strings A character vector.
#' @param pattern A regular expression.
#' @param n A natural number to identify the \eqn{n}th occurrence, or, if you
#'   wish to select the \emph{last} occurrence, you need \code{n = "last"} (or
#'   the abbreviated \code{n = "l"}).
#' @return A character vector of the desired strings.
#' @examples
#' string <- "ab..cd..de..fg..h"
#' StrAfterNth(string, "\\.\\.", 3)
#' StrBeforeNth(string, "e", 1)
StrAfterNth <- function(strings, pattern, n) {
  instances.indices <- gregexpr(pattern, strings)
  instances.lengths <- lapply(instances.indices, function(x) attr(x, "match.length"))
  if (is.character(n)) {
    if (! n %in% c("l", "last")) {
      stop("If n is of character type, it must be either 'l' or 'last'.")
    }
    nth.instance.indices <- sapply(instances.indices, Last)
    nth.lengths <- sapply(instances.lengths, Last)
  } else {
    nth.instance.indices <- sapply(instances.indices, function(x) x[n])
    nth.lengths <- sapply(instances.lengths, function(x) x[n])
  }
  mapply(str_sub, strings, nth.instance.indices + nth.lengths, nchar(strings))
}

#' @rdname StrAfterNth
StrBeforeNth <- function(strings, pattern, n) {
  instances.indices <- gregexpr(pattern, strings)
  if (is.character(n)) {
    if (! n %in% c("l", "last")) {
      stop("If n is of character type, it must be either 'l' or 'last'.")
    }
    nth.instance.indices <- sapply(instances.indices, Last)
  } else {
    nth.instance.indices <- sapply(instances.indices, function(x) x[n])
  }
  mapply(substr, strings, 1, nth.instance.indices - 1)
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
ExtendCharVec <- function(char.vec, extend.by = NA, length.out = NA) {
  if (length(extend.by) != 1) stop("extend.by must have length 1.")
  if (length(extend.by) != 1) stop("extend.by must have length 1.")
  if (is.na(extend.by) && is.na(length.out)) stop("One of extend.by or length.out must be specified.")
  if (is.numeric(char.vec)) char.vec <- as.character(char.vec)
  if (!is.character(char.vec)) stop("char.vec must be of numeric or character type.")
  if (!is.na(extend.by)) {
    if (!is.numeric(extend.by)) stop("If specified, extend.by must be numeric.")
    return(c(char.vec, rep("", extend.by)))
  }
  if (!is.na(length.out)) {
    if (!(is.numeric(length.out) && length.out >= length(char.vec))) {
      stop("If specified, length.out must be numeric and at least equal to the length of char.vec.")
    }
    c(char.vec, rep("", length.out - length(char.vec)))
  }
}

#' Paste vectors/files with different lengths/numbers of lines.
#'
#' Paste character vectors of different lengths, optionally inputting the
#' vectors as file names, which become character vectors via \code{readLines}.
#' Vectors are first extended to all be the same length via
#' \code{\link{ExtendCharVec}} and are then pasted together, with no separator
#' put in when pasting empty strings. See the examples if you don't understand.
#'
#' @param files A character vector of files to be read in via \code{readLines}
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
#' PasteDifferentLengths(list.files(pattern = "PasteDifferentLengths"), sep = "sSsepPp")
#' file.remove(list.files(pattern = "PasteDifferentLengths"))  # run this to clean up your working directory from the teporary files that were created for these examples
PasteDifferentLengths <- function(files, sep = "") {
  if (is.character(files)) {
    files <- lapply(files, readLines)
  }
  if (!is.list(files)) stop("files must be either a character vector or a list of objects.")
  max.length <- max(sapply(files, length))
  files <- sapply(files, ExtendCharVec, length.out = max.length)
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
#'   (coerced by as.characher if not charachter already).
#' @param positions The indices of the charachter vector to be occupied by the
#'   elements of strings. Must be the same length as strings.
#' @return A charachter vector.
#' @examples
#' PutInPos(1:3, c(1, 8, 9))
#' PutInPos(c("Apple", "Orange", "County"), c(5, 7, 8))
PutInPos <- function(strings, positions) {
  strings <- as.character(strings)
  stopifnot(length(strings) == length(positions))
  out <- rep("", max(positions))
  out[positions] <- strings
  out
}

#' Trim something other than whitespace
#'
#' The \code{stringi} and \code{stringr} packages let you trim whitespace, but
#' what if you want to trim something else from either (or both) side(s) of a
#' string? This function lets you select which charachter to trim and from which
#' side(s).
#'
#' @param string A string.
#' @param char A single charachter, \emph{not} in regular expression. So to trim
#'   a period, use \code{char = "."} and not \code{char = "\\\\."}.
#' @param side Which side do you want to trim from? \code{"both"} is the
#'   default, but you can also have just either \code{"left"} or \code{"right"}
#'   (or optionally the shorthands \code{"b"}, \code{"l"} and \code{"r"}).
#' @return A string.
#' @examples
#' TrimAnything("..abcd.", ".", "left")
#' TrimAnything("-ghi--", "-")
TrimAnything <- function(string, char, side = "both") {
  stopifnot(nchar(string) > 0)
  stopifnot(nchar(char) == 1)
  side <- tolower(side)  # give side some case lenience
  stopifnot(side %in% c("both", "b", "left", "l", "right", "r"))
  vec <- StringToVec(string)
  if (AllEqual(vec, char)) return("")
  occurrences <- which(vec == char)
  if (!length(occurrences)) return(string)
  groups <- GroupClose(occurrences)
  side <- StrElem(side, 1)
  if (side %in% c("b", "r")) {
    last.occurrences <- Last(groups)
    if (nchar(string) %in% last.occurrences) vec <- vec[-last.occurrences]
  }
  if (side %in% c("b", "l")) {
    first.occurrences <- groups[[1]]
    if (1 %in% first.occurrences) vec <- vec[-first.occurrences]
  }
  return(paste(vec, collapse = ""))
}

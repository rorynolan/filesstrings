#' A more flexible version of [all.equal] for vectors.
#'
#' @description
#' This function will return `TRUE` whenever [base::all.equal()]
#' would return `TRUE`, however it will also return `TRUE` in some other cases:
#' * If `a` is given and `b` is not, `TRUE` will be returned if all of the
#' elements of `a` are the same.
#' * If `a` is a scalar and `b` is a vector or array, `TRUE` will be returned
#' if every element in `b` is equal to `a`.
#' * If `a` is a vector or array and `b` is a scalar, `TRUE` will be returned
#' if every element in `a` is equal to `b`.
#'
#' When this function does not return `TRUE`, it returns `FALSE` (unless it
#' errors). This is unlike [base::all.equal()].
#'
#' @note \itemize{\item This behaviour is totally different from
#'   [base::all.equal()]. \item There's also [dplyr::all_equal()], which is
#'   different again. To avoid confusion, always use the full
#'   `filesstrings::all_equal()` and never `library(filesstrings)` followed by
#'   just `all_equal()`.}
#'
#' @param a A vector, array or list.
#' @param b Either `NULL` or a vector, array or list of length either 1 or
#'   `length(a)`.
#' @return `TRUE` if "equality of all" is satisfied (as detailed in
#'   'Description' above) and `FALSE` otherwise.
#' @examples
#' all_equal(1, rep(1, 3))
#' all_equal(2, 1:3)
#' all_equal(1:4, 1:4)
#' all_equal(1:4, c(1, 2, 3, 3))
#' all_equal(rep(1, 10))
#' all_equal(c(1, 88))
#' all_equal(1:2)
#' all_equal(list(1:2))
#' all_equal(1:4, matrix(1:4, nrow = 2))  # note that this gives TRUE
#' @export
all_equal <- function(a, b = NULL) {
  checkmate::assert(checkmate::check_null(a),
                    checkmate::check_vector(a),
                    checkmate::check_list(a),
                    checkmate::check_array(a))
  checkmate::assert(checkmate::check_null(b),
                    checkmate::check_vector(b),
                    checkmate::check_list(b),
                    checkmate::check_array(b))
  if (is.array(a) && isTRUE(checkmate::check_scalar(b)))
    b %<>% array(dim = dim(a))
  if (is.array(b) && isTRUE(checkmate::check_scalar(a)))
    a %<>% array(dim = dim(b))
  if (is.null(b)) {
    if (rlang::is_atomic(a)) return(isTRUE(all(a == a[[1]])) || all(is.na(a)))
    return(length(unique(a)) == 1)
  }
  if (is.array(a)) {
    if (!is.array(b)) return(FALSE)
    if (!all_equal(dim(a), dim(b))) return(FALSE)
    a %<>% as.vector()
    b %<>% as.vector()
  }
  if (is.array(b)) if (!is.array(a)) return(FALSE)
  if (is.null(a) && (!is.null(b))) return(FALSE)
  if (length(a) == 1) {
    if (length(b) == 0) return(FALSE)
    a <- rep(a, length(b))
  }
  if (length(b) == 1) {
    if (length(a) == 0) return(FALSE)
    b <- rep(b, length(a))
  }
  return(isTRUE(all.equal(a, b)))
}

#' Group together close adjacent elements of a vector.
#'
#' Given a strictly increasing vector (each element is bigger than the last),
#' group together stretches of the vector where *adjacent* elements are
#' separated by at most some specified distance. Hence, each element in each
#' group has at least one other element in that group that is *close* to
#' it. See the examples.
#' @param vec_ascending A strictly increasing numeric vector.
#' @param max_gap The biggest allowable gap between adjacent elements for them
#'   to be considered part of the same *group*.
#' @return A where each element is one group, as a numeric vector.
#' @examples
#' group_close(1:10, 1)
#' group_close(1:10, 0.5)
#' group_close(c(1, 2, 4, 10, 11, 14, 20, 25, 27), 3)
#' @export
group_close <- function(vec_ascending, max_gap = 1) {
  lv <- length(vec_ascending)
  if (lv == 0) stop("vec_ascending must have length greater than zero.")
  test <- all(diff(vec_ascending) > 0)
  if (is.na(test) || !test) stop("vec_ascending must be strictly increasing.")
  if (lv == 1) {
    return(list(vec_ascending))
  } else {
    gaps <- vec_ascending[2:lv] - vec_ascending[1:(lv-1)]
    big_gaps <- gaps > max_gap
    nbgaps <- sum(big_gaps)  # number of big gaps
    if (!nbgaps) {
      return(list(vec_ascending))
    } else {
      ends <- which(big_gaps)  # vertical end of lines
      group1 <- vec_ascending[1:ends[1]]
      lg <- list(group1)
      if (nbgaps == 1){
        lg[[2]] <- vec_ascending[(ends[1] + 1):lv]
      } else {
        for (i in 2:nbgaps){
          lg[[i]] <- vec_ascending[(ends[i - 1] + 1):ends[i]]
          ikeep <- i
        }
        lg[[ikeep + 1]] <- vec_ascending[(ends[nbgaps] + 1):lv]
      }
      return(lg)
    }
  }
}

#' Argument Matching
#'
#' Match `arg` against a series of candidate `choices` where `NULL` means take
#' the first one. `arg` _matches_ an element of `choices` if `arg` is a prefix
#' of that element.
#'
#' `ERROR`s are thrown when a match is not made and where the match is
#' ambiguous. However, sometimes ambiguities are inevitable. Consider the case
#' where `choices = c("ab", "abc")`, then there's no way to choose `"ab"`
#' because `"ab"` is a prefix for `"ab"` and `"abc"`. If this is the case, you
#' need to provide a full match, i.e. using `arg = "ab"` will get you `"ab"`
#' without an error, however `arg = "a"` will throw an ambiguity error.
#'
#' This function inspired by `RSAGA::match.arg.ext()`. Its behaviour is almost
#' identical (the difference is that `RSAGA::match.arg.ext(..., ignore.case =
#' TRUE)` guarantees that the function returns strings in all lower case, but
#' that is not so with `filesstrings::match_arg(..., ignore_case = TRUE)`)
#' but `RSAGA` is a heavy package to depend upon so `filesstrings::match_arg()`
#' might be handy for package developers.
#'
#' @param arg A character vector (of length one unless `several_ok = TRUE`).
#' @param choices A character vector of candidate values.
#' @param index Return the index of the match rather than the match itself?
#'   Default no.
#' @param several_ok Allow `arg` to have length greater than one to match
#'   several arguments at once? Default no.
#' @param ignore_case Ignore case while matching. Default no. If this is `TRUE`,
#'   the returned value is the matched element of `choices` (with its original
#'   casing).
#'
#' @examples
#' choices <- c("Apples", "Pears", "Bananas", "Oranges")
#' match_arg(NULL, choices)
#' match_arg("A", choices)
#' match_arg("B", choices, index = TRUE)
#' match_arg(c("a", "b"), choices, several_ok = TRUE, ignore_case = TRUE)
#' match_arg(c("b", "a"), choices, ignore_case = TRUE, index = TRUE,
#'           several_ok = TRUE)
#'
#' @export
match_arg <- function(arg, choices, index = FALSE, several_ok = FALSE,
                      ignore_case = FALSE) {
  checkmate::assert_character(choices, min.len = 1)
  checkmate::assert_flag(index)
  checkmate::assert_flag(several_ok)
  checkmate::assert_flag(ignore_case)
  if (is.null(arg)) return(choices[1])
  checkmate::assert_character(arg, min.len = 1)
  first_dup <- anyDuplicated(choices)
  if (first_dup) {
    stop("`choices` must not have duplicate elements. ", "\n",
         "    * Element ", first_dup,
         " of your `choices` ('", choices[first_dup], "') is a duplicate.")
  }
  if (ignore_case) {
    lower_choices <- str_to_lower(choices)
    first_dup <- anyDuplicated(lower_choices)
    if (first_dup) {
      dupair_indices <- c(match(lower_choices[first_dup], lower_choices),
                          first_dup)
      dupair <- choices[dupair_indices]
      stop("`choices` must not have duplicate elements. ", "\n",
           "    * Since you have set `ignore_case = TRUE`, elements ",
           dupair_indices[1], " and ", dupair_indices[2],
           " of your `choices` ('", dupair[1], "' and '", dupair[2], "') ",
           "are effectively duplicates.")
    }
  }
  arg_len <- length(arg)
  if ((!several_ok) && arg_len > 1) {
    stop("`arg` must have length 1.", "\n",
         "    * Your `arg` has length ", arg_len, ".", "\n",
         "    * To use an `arg` with length greater than one, ",
         "use `several_ok = TRUE`. ")
  }
  if (ignore_case) {
    indices <- match_arg_index(str_to_lower(arg), lower_choices)
  } else {
    indices <- match_arg_index(arg, choices)
  }
  bads <- indices < 0
  if (any(bads)) {
    first_bad_index <- match(T, bads)
    first_bad_type <- indices[first_bad_index]
    stopifnot(first_bad_type %in% -(1:2))  # should never happen
    if (first_bad_type == -1) {
      stop("`arg` must be a prefix of exactly one element of `choices`.", "\n",
           "    * Your `arg` '", arg[first_bad_index], "' is not a prefix of ",
           "any element of `choices`.")
    } else {
      if (ignore_case) {
        two_ambigs <- str_detect(tolower(choices),
                                 str_c("^", tolower(arg)[first_bad_index]))
      } else {
        two_ambigs <- str_detect(choices, str_c("^", arg[first_bad_index]))
      }
      two_ambigs %<>%
        {choices[.]} %>%
        {.[1:2]}
      stop("`arg` must be a prefix of exactly one element of `choices`.", "\n",
           "    * Your `arg` '", arg[first_bad_index], "' is a prefix of two ",
           "or more element of `choices`.", "\n",
           "    * The first two of these are '", two_ambigs[1], " 'and '",
           two_ambigs[2], "'.")
    }
  }
  indices %<>% {. + 1}
  if (index) return(indices)
  choices[indices]
}

#' A more flexible version of [all.equal] for vectors.
#'
#' If one argument is specified, check that all elements of that argument are
#' equal. If two arguments of equal length are specified, check equality of all
#' of their corresponding elements. If two arguments are specified, `a` of
#' length 1 and `b` of length greater than 1, check that all elements of
#' `b` are equal to the element in a. If two arguments are specified, `a` of
#' length greater than 1 and `b` of 1, check that all elements of
#' `a` are equal to the element in `b`.
#'
#' @param a A vector.
#' @param b Either `NULL` or a vector of length either 1 or `length(a)`.
#' @return `TRUE` if "equality of all" is satisfied (as detailed in
#'   "Description" above) and `FALSE` otherwise.
#' @examples
#' all_equal(1, rep(1, 3))
#' all_equal(2, 1:3)
#' all_equal(1:4, 1:4)
#' all_equal(1:4, c(1, 2, 3, 3))
#' all_equal(rep(1, 10))
#' all_equal(c(1, 88))
#' @export
all_equal <- function(a, b = NULL) {
  if (is.null(a) && (!is.null(b))) return(FALSE)
  if (is.null(b[1])) {
    return(length(unique(a)) == 1)
  } else {
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
}

#' Group together close adjacent elements of a vector.
#'
#' Given a strictly increasing vector (each element is bigger than the last),
#' group together stretches of the vector where \emph{adjacent} elements are
#' separeted by at most some specified distance. Hence, each element in each
#' group has at least one other element in that group that is \emph{close} to
#' it. See the examples.
#' @param vec_ascending A strictly increasing numeric vector.
#' @param max_gap The biggest allowable gap between adjacent elements for them
#'   to be considered part of the same \emph{group}.
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


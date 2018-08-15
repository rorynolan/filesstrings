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

#' Get the nth element of each vector in a list of numeric or character vectors.
#'
#' These are faster implementations of procedures that could very easily be done
#' with [purrr::map_dbl] or [purrr::map_chr].
#'
#' @param char_list A list of character vectors.
#' @param n The index of the element that you want from each vector. If
#'   `char_list` (or `num_list`) is of length 1, this can be any length and
#'   those indices will be extracted from `char_list[[1]]` (or `num_list[[1]]`).
#'   Otherwise, this must either be of length 1 or the same length as
#'   `char_list`. All of this is to say that the function is vectorised over
#'   this argument.
#'
#' @return A list.
#'
#' @examples
#' str_list_nth_elems_(list(c("a", "b", "c"), c("d", "f", "a")), 2)
#' num_list_nth_elems_(list(1:5, 0:2), 4)
#'
#' @noRd
str_list_nth_elems <- function(char_list, n) {
  checkmate::assert_list(char_list, min.len = 1)
  checkmate::assert_integerish(n, min.len = 1)
  lcl <- length(char_list)
  ln <- length(n)
  if (lcl > 1 && ln > 1 && lcl != ln) {
    stop("If both `char_list` and `n` have lengths greater than 1, ",
         "then their lengths must be equal.", "\n",
         "    * Your `char_list has length ", length(char_list), " and ",
         "your `n` has length ", length(n), ".")
  }
  str_list_nth_elems_(char_list, n)
}


#' @rdname str_list_nth_elems_
#' @param num_list A list of numeric vectors.
#' @noRd
num_list_nth_elems <- function(num_list, n) {
  checkmate::assert_list(num_list, min.len = 1)
  checkmate::assert_integerish(n, min.len = 1)
  lnl <- length(num_list)
  ln <- length(n)
  if (lnl > 1 && ln > 1 && lnl != ln) {
    stop("If both `num_list` and `n` have lengths greater than 1, ",
         "then their lengths must be equal.", "\n",
         "    * Your `num_list has length ", length(num_list), " and ",
         "your `n` has length ", length(n), ".")
  }
  num_list_nth_elems_(num_list, n)
}

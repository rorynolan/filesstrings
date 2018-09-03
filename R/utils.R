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


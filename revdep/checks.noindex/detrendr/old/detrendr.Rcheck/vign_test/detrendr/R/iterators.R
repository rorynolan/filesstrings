iter_mat_col_sets <- function(mat, n_sets) {
  if (n_sets < 2) return(mat)
  if (n_sets >= ncol(mat)) {
    return(iterators::iapply(mat, 2))
  } else {
    start_end <- matrix(NA, ncol = n_sets, nrow = 2) %>%
      magrittr::set_rownames(c("start", "end"))
    start_end[1] <- 1
    start_end["end", ] <- floor(ncol(mat) * seq_len(n_sets) / n_sets)
    start_end["start", 2:n_sets] <- start_end["end", 1:(n_sets - 1)] + 1
  }
  set <- 0
  nextEl <- function() {
    set <<- set + 1
    if (set > n_sets) stop("StopIteration")
    mat[, seq(start_end["start", set], start_end["end", set])]
  }
  obj <- list(nextElem = nextEl)
  class(obj) <- c("irep", "abstractiter", "iter")
  obj
}

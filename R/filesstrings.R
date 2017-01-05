#' @import stringr
#' @importFrom magrittr "%>%"
#' @useDynLib filesstrings
#' @importFrom Rcpp sourceCpp
#' @importFrom Rcpp sourceCpp
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if (getRversion() >= "2.15.1") {
  utils::globalVariables(c("."))
}

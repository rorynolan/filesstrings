#' @useDynLib autothresholdr, .registration = TRUE
#' @importFrom Rcpp sourceCpp
#' @importFrom magrittr '%>%' '%T>%' '%<>%'
#' @importFrom ijtiff ijtiff_img
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if (getRversion() >= "2.15.1")  utils::globalVariables(".")

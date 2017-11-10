#' @import stringr
#' @importFrom magrittr '%>%' '%<>%' '%T>%'
#' @useDynLib filesstrings, .registration = TRUE
#' @importFrom Rcpp sourceCpp
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if (getRversion() >= "2.15.1") {
  utils::globalVariables(c("."))
}

.onUnload <- function (libpath) {
  library.dynam.unload("filesstrings", libpath)
}

#' Defunct functions
#'
#' These functions have been made defunct, mostly because the naming style of
#' the package has been changed. Some have been removed because they were not
#' well-done.
#'
#' @param ... Defunct function arguments.
#'
#' @name filesstrings-defunct
NULL

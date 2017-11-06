#' @import stringr
#' @importFrom magrittr "%>%" "%<>%"
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

#' Deprecated functions
#'
#' These functions have been deprecated, mostly because the naming style of the
#' package has been changed. To figure out which function to run instead, run
#' the deprecated function of interest and you'll get a warning message telling
#' you which function to use instead.
#'
#' @name filesstrings-deprecated
NULL

#' Defunct functions
#'
#' These functions have been made defunct, mostly because the naming style of
#' the package has been changed. To figure out which function to run instead,
#' run the defunct function of interest and you'll get an error message
#' telling you which function to use instead.
#'
#' @name filesstrings-defunct
NULL

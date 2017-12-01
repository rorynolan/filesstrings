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

#' `filesstrings`: handy file and string manipulation
#'
#' Convenient functions for moving files, deleting directories, and a variety of
#' string operations that facilitate manipulating files and extracting
#' information from strings.
#'
#' @docType package
#' @name filesstrings
#' @alias filesstrings-package
#' @references Rory Nolan and Sergi Padilla-Parra (2017). filesstrings: An R
#'   package for file and string manipulation. The Journal of Open Source
#'   Software, 2(14).  \doi{10.21105/joss.00260}.
NULL

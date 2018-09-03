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
#' This started out as a package for file and string manipulation. Since then,
#' the `fs` file manipulation package and the `strex string manipulation package
#' emerged, offering functionality previously given by this package (but
#' slightly better). Those packages have hence almost pushed 'filesstrings' into
#' extinction. However, it still has a small number of unique, handy file
#' manipulation functions which can be seen in the
#' [vignette](https://cran.r-project.org/package=filesstrings/vignettes/files.html).
#' One example is a function to remove spaces from all file names in a
#' directory.
#'
#' @docType package
#' @name filesstrings
#' @aliases filesstrings-package
#' @references Rory Nolan and Sergi Padilla-Parra (2017). filesstrings: An R
#'   package for file and string manipulation. The Journal of Open Source
#'   Software, 2(14).  \doi{10.21105/joss.00260}.
NULL

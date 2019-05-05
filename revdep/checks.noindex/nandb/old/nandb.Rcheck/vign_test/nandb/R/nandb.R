#' @useDynLib nandb, .registration = TRUE
#' @importFrom Rcpp sourceCpp
#' @importFrom magrittr '%>%' '%T>%' '%<>%'
#' @importFrom rlang '%||%'
NULL

## quiet concerns of R CMD check re: the .'s that appear in pipelines
if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(".", "Var1", "Var2", "value", "x", "y", "colour"))
}

.onUnload <- function(libpath) {
  library.dynam.unload("nandb", libpath)
}

#' nandb: Number and brightness in R.
#'
#' The `nandb` package gives functions for calculation of molecular number and
#' brightness from images, as detailed in Digman et al. 2008. It comes with an
#' implementation of the novel 'automatic detrending' technique.
#'
#' @docType package
#' @name nandb
#' @aliases nandb-package
#' @references Digman MA, Dalal R, Horwitz AF, Gratton E. Mapping the Number of
#'   Molecules and Brightness in the Laser Scanning Microscope. Biophysical
#'   Journal. 2008;94(6):2320-2332. \doi{10.1529/biophysj.107.114645}.
NULL

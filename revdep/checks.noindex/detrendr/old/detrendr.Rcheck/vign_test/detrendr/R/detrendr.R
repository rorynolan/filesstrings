#' @importFrom RcppParallel RcppParallelLibs
#' @importFrom Rcpp sourceCpp
#' @importFrom magrittr '%>%' '%T>%' '%<>%'
#' @importFrom foreach '%dopar%'
#' @useDynLib detrendr, .registration = TRUE
NULL

## quiet concerns of R CMD check re: the .'s that appear in pipelines
if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(".", "var", "maxl", "l", "seed", "frame"))
}

.onUnload <- function(libpath) {
  library.dynam.unload("detrendr", libpath)
}

#' detrendr: Image detrending in R.
#'
#' The `detrendr` package gives functions for detrending images, most often used
#' for preprocessing in fluorescence fluctuation and correlation spectroscopy
#' (FFS and FCS).
#'
#' @docType package
#' @name detrendr
#' @aliases detrendr-package
#' @references Rory Nolan, Luis A. J. Alvarez, Jonathan Elegheert, Maro
#'   Iliopoulou, G. Maria Jakobsdottir, Marina Rodriguez-Muñoz, A. Radu
#'   Aricescu, Sergi Padilla-Parra; nandb—number and brightness in R with a
#'   novel automatic detrending algorithm, Bioinformatics,
#'   https://doi.org/10.1093/bioinformatics/btx434.
NULL

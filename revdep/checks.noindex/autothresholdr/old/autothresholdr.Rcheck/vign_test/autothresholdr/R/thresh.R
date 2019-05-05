#' Automatically threshold an array of non-negative integers.
#'
#' These functions apply the ImageJ "Auto Threshold" plugin's image thresholding
#' methods. The available methods are "IJDefault", "Huang", "Huang2",
#' "Intermodes", "IsoData", "Li", "MaxEntropy", "Mean", "MinErrorI", "Minimum",
#' "Moments", "Otsu", "Percentile", "RenyiEntropy", "Shanbhag", "Triangle" and
#' "Yen". Read about them at \url{http://imagej.net/Auto_Threshold}.
#'
#'
#' \itemize{\item Values greater than or equal to the found threshold
#' \emph{pass} the thresholding and values less than the threshold \emph{fail}
#' the thresholding.
#'
#' \item{For `ignore_white = TRUE`, if the maximum value in the array is one of
#' `2^8-1`, `2^12-1`, `2^16-1` or `2^32-1`, then those max values are ignored.
#' That's because they're the white values in 8, 12, 16 and 32-bit images
#' respectively (and these are the common image bit sizes to work with). This
#' guesswork has to be done because `R` does not know how many bits the image
#' was on disk. This guess is very unlikely to be wrong, and if it is, the
#' consequences are negligible anyway. If you're very concerned, then just
#' specify the white value as an integer in this `ignore_white` argument.}
#'
#' \item{If you have set `ignore_black = TRUE` and/or `ignore_white = TRUE` but
#' you are still getting error/warning messages telling you to try them, then
#' your chosen method is not working for the given array, so you should try a
#' different method.}
#'
#' \item For a given array, if all values are less than `2^8`, saturated value
#' is `2^8 - 1`, otherwise, saturated value is `2^16 - 1`. }
#'
#' @param int_arr An array (or vector) of non-negative \emph{integers}.
#' @param method The name of the thresholding method you wish to use. The
#'   available methods are `"IJDefault"`, `"Huang"`, `"Huang2"`, `"Intermodes"`,
#'   `"IsoData"`, `"Li"`, `"MaxEntropy"`, `"Mean"`, `"MinErrorI"`, `"Minimum"`,
#'   `"Moments"`, `"Otsu"`, `"Percentile"`, `"RenyiEntropy"`, `"Shanbhag"`,
#'   `"Triangle"` and `"Yen"`. Partial matching is performed i.e. `method = "h"`
#'   is enough to get you `"Huang"` and `method = "in"` is enough to get you
#'   `"Intermodes"`. To perform \emph{manual} thresholding (where you set the
#'   threshold yourself), supply the threshold here as a number e.g. `method =
#'   3`; so note that this would \emph{not} select the third method in the above
#'   list of methods.
#' @param ignore_black Ignore black pixels/elements (zeros) when performing the
#'   thresholding?
#' @param ignore_white Ignore white pixels when performing the thresholding? If
#'   set to `TRUE`, the function makes a good guess as to what the white
#'   (saturated) value would be (see 'Details'). If this is set to a number, all
#'   pixels with value greater than or equal to that number are ignored.
#' @param fail When using `auto_thresh_apply_mask()`, to what value do you wish
#'   to set the pixels which fail to exceed the threshold? `fail = 'saturate'`
#'   sets them to saturated value (see "Details"). `fail = 'zero'` sets them to
#'   zero. You can also specify directly here a natural number (must be between
#'   `0` and `2^16 - 1`) to use.
#' @param ignore_na This should be `TRUE` if `NA`s in `int_arr` should be
#'   ignored or `FALSE` if you want the presence of `NA`s in `int_arr` to throw
#'   an error.
#'
#' @return `auto_thresh()` returns an object of class [th] containing the
#'   threshold value. Pixels exceeding this threshold pass the thresholding,
#'   pixels at or below this level fail.
#'
#'   `auto_thresh_mask()` returns an object of class [arr_mask] which is a
#'   binarized version of the input, with a value of `TRUE` at points which
#'   exceed the threshold and `FALSE` at those which do not.
#'
#'   `auto_thresh_apply_mask()` returns and object of class [threshed_arr] which is the original input masked by the
#'   threshold, i.e. all points not exceeding the threshold are set to a
#'   user-defined value (default `NA`).
#'
#'   `mask()` is the same as `auto_thresh_mask()` and `apply_mask()` is the same
#'   as `auto_thresh_apply_mask()`.
#'
#' @references \itemize{ \item{Huang, L-K & Wang, M-J J (1995), "Image
#'   thresholding by minimizing the measure of fuzziness", Pattern Recognition
#'   28(1): 41-51} \item{Prewitt, JMS & Mendelsohn, ML (1966), "The analysis of
#'   cell images", Annals of the New York Academy of Sciences 128: 1035-1053}
#'   \item{Ridler, TW & Calvard, S (1978), "Picture thresholding using an
#'   iterative selection method", IEEE Transactions on Systems, Man and
#'   Cybernetics 8: 630-632} \item{Li, CH & Lee, CK (1993), "Minimum Cross
#'   Entropy Thresholding", Pattern Recognition 26(4): 617-625} \item{Li, CH &
#'   Tam, PKS (1998), "An Iterative Algorithm for Minimum Cross Entropy
#'   Thresholding", Pattern Recognition Letters 18(8): 771-776} \item{Sezgin, M
#'   & Sankur, B (2004), "Survey over Image Thresholding Techniques and
#'   Quantitative Performance Evaluation", Journal of Electronic Imaging 13(1):
#'   146-165} \item{Kapur, JN; Sahoo, PK & Wong, ACK (1985), "A New Method for
#'   Gray-Level Picture Thresholding Using the Entropy of the Histogram",
#'   Graphical Models and Image Processing 29(3): 273-285} \item{Glasbey, CA
#'   (1993), "An analysis of histogram-based thresholding algorithms", CVGIP:
#'   Graphical Models and Image Processing 55: 532-537} \item{Kittler, J &
#'   Illingworth, J (1986), "Minimum error thresholding", Pattern Recognition
#'   19: 41-47} \item{Prewitt, JMS & Mendelsohn, ML (1966), "The analysis of
#'   cell images", Annals of the New York Academy of Sciences 128: 1035-1053}
#'   \item{Tsai, W (1985), "Moment-preserving thresholding: a new approach",
#'   Computer Vision, Graphics, and Image Processing 29: 377-393} \item{Otsu, N
#'   (1979), "A threshold selection method from gray-level histograms", IEEE
#'   Trans. Sys., Man., Cyber. 9: 62-66, doi:10.1109/TSMC.1979.4310076}
#'   \item{Doyle, W (1962), "Operation useful for similarity-invariant pattern
#'   recognition", Journal of the Association for Computing Machinery 9:
#'   259-267, doi:10.1145/321119.321123} \item{Kapur, JN; Sahoo, PK & Wong, ACK
#'   (1985), "A New Method for Gray-Level Picture Thresholding Using the Entropy
#'   of the Histogram", Graphical Models and Image Processing 29(3): 273-285}
#'   \item{Shanbhag, Abhijit G. (1994), "Utilization of information measure as a
#'   means of image thresholding", Graph. Models Image Process. (Academic Press,
#'   Inc.) 56 (5): 414--419, ISSN 1049-9652} \item{Zack GW, Rogers WE, Latt SA
#'   (1977), "Automatic measurement of sister chromatid exchange frequency", J.
#'   Histochem. Cytochem. 25 (7): 74153, PMID 70454} \item{Yen JC, Chang FJ,
#'   Chang S (1995), "A New Criterion for Automatic Multilevel Thresholding",
#'   IEEE Trans. on Image Processing 4 (3): 370-378, ISSN 1057-7149,
#'   doi:10.1109/83.366472} \item{Sezgin, M & Sankur, B (2004), "Survey over
#'   Image Thresholding Techniques and Quantitative Performance Evaluation",
#'   Journal of Electronic Imaging 13(1): 146-165} }
#'
#' @section Acknowledgements: Gabriel Landini coded all of these functions
#'   in Java. These java functions were then translated to C++.
#'
#' @examples
#' img_location <- system.file("extdata", "eg.tif", package = "autothresholdr")
#' img <- ijtiff::read_tif(img_location)
#' auto_thresh(img, "huang")
#' auto_thresh(img, "tri")
#' auto_thresh(img, "Otsu")
#' auto_thresh(img, 9)
#' mask <- auto_thresh_mask(img, "huang")
#' ijtiff::display(mask[, , 1, 1])
#' masked <- auto_thresh_apply_mask(img, "huang")
#' ijtiff::display(masked[, , 1, 1])
#' masked <- auto_thresh_apply_mask(img, 25)
#' ijtiff::display(masked[, , 1, 1])
#' @export
auto_thresh <- function(int_arr, method,
                        ignore_black = FALSE, ignore_white = FALSE,
                        ignore_na = FALSE) {
  checkmate::assert_scalar(method)
  checkmate::assert(checkmate::check_number(method),
                    checkmate::check_string(method))
  checkmate::assert_integerish(int_arr, min.len = 1)
  checkmate::assert_flag(ignore_black)
  checkmate::assert(checkmate::check_flag(ignore_white),
                    checkmate::check_number(ignore_white, lower = 0))
  checkmate::assert_flag(ignore_na)
  if (all(is.na(int_arr))) {
    custom_stop("`int_arr` must not be all `NA`s.",
                "Every element of your `int_arr` is `NA`.")
  }
  if (anyNA(int_arr)) {
    if (!ignore_na) {
      custom_stop(
        "
        The input `int_arr` has NA values, but you have `ignore_na = FALSE`, so
        the function `auto_thresh()` has errored.
        ",
        "
        To tell `auto_thresh()` to ignore `NA` values, set the argument
        `ignore_na = TRUE`.
        ")
    } else {
      int_arr <- int_arr[!is.na(int_arr)]
    }
  }
  checkmate::assert(checkmate::check_vector(int_arr, any.missing = FALSE),
                    checkmate::check_array(int_arr, any.missing = FALSE))
  checkmate::assert_numeric(int_arr, any.missing = FALSE,
                            lower = 0, upper = .Machine$integer.max)
  checkmate::assert_integerish(int_arr, any.missing = FALSE)
  if (is.numeric(method)) {
    thresh <- method
    return(th(thresh, NA, NA, NA, NA))
  }
  method <- tolower(method)
  if (startsWith("default", method)) method <- "IJDefault"
  if (startsWith("huang", method)) method <- "Huang"
  available_methods <- c("IJDefault", "Huang", "Huang2", "Intermodes",
                         "IsoData", "Li", "MaxEntropy", "Mean", "MinErrorI",
                         "Minimum", "Moments", "Otsu", "Percentile",
                         "RenyiEntropy", "Shanbhag", "Triangle", "Yen")
  method <- filesstrings::match_arg(method, available_methods,
                                    ignore_case = TRUE)
  if (ignore_black) int_arr[int_arr == 0] <- NA
  if (ignore_white) {
    if (isTRUE(ignore_white)) {
      mx <- max(int_arr)
      if (mx %in% (2 ^ c(8, 12, 16, 32) - 1)) int_arr[int_arr == mx] <- NA
    } else {
      int_arr[int_arr >= ignore_white] <- NA
    }
  }
  rim <- range(int_arr, na.rm = TRUE)
  im_hist <- factor(int_arr, levels = rim[1]:rim[2]) %>%
    table() %>%
    as.vector()
  if (length(im_hist) < 2) {
    unq_val <- stats::na.omit(int_arr)[1]
    custom_stop("Cannot threshold an array with only one unique value. ",
                "
                Your `int_arr` has only one unique value which is
                {format(unq_val, scientific = FALSE)}.
                ")
  }
  thresh <- eval_text(method)(im_hist)
  if (thresh < 0) {
    custom_stop("'{method}' method failed to find threshold.")
  }
  th(thresh = thresh, ignore_black = ignore_black, ignore_white = ignore_white,
     ignore_na = ignore_na, autothresh_method = method)
}

#' @rdname auto_thresh
#' @export
auto_thresh_mask <- function(int_arr, method,
                             ignore_black = FALSE, ignore_white = FALSE,
                             ignore_na = FALSE) {
  thresh <- auto_thresh(int_arr, method,
                        ignore_black = ignore_black,
                        ignore_white = ignore_white,
                        ignore_na = ignore_na)
  mask <- int_arr >= thresh
  arr_mask(arr = mask, thresh = thresh)
}

#' @rdname auto_thresh
#' @export
auto_thresh_apply_mask <- function(int_arr, method, fail = NA,
                                   ignore_black = FALSE, ignore_white = FALSE,
                                   ignore_na = FALSE) {
  mask <- auto_thresh_mask(int_arr, method,
                           ignore_black = ignore_black,
                           ignore_white = ignore_white,
                           ignore_na = ignore_na)
  fail <- translate_fail(int_arr, fail)
  int_arr[!mask] <- fail
  threshed_arr(arr = int_arr, thresh = attr(mask, "thresh"))
}

#' @rdname auto_thresh
#' @export
mask <- auto_thresh_mask

#' @rdname auto_thresh
#' @export
apply_mask <- auto_thresh_apply_mask

#' Threshold every image frame in an image stack based on their mean.
#'
#' An [ijtiff_img][ijtiff::ijtiff_img] is a 4-dimensional array indexed by
#' `img[y, x, channel, frame]`. For each channel (which consists of a stack of
#' frames), this function finds a threshold based on the sum all of the frames,
#' uses this to create a mask and then applies this mask to every frame in the
#' stack (so for a given pillar in the image stack, either all the pixels
#' therein are thresholded away or all are untouched, where pillar `x,y` of
#' channel `ch` is `img[y, x, ch, ]`).
#'
#' It's called `mean_stack_thresh()` and not `sum_stack_thresh()` because its
#' easier for people to visualize the mean of an image series than to visualize
#' the sum, but for the sake of this procedure, both are equivalent, except for
#' the fact that the thresholding routine invoked inside this function prefers
#' integers, which we get by using a sum but not by using a mean.
#'
#' \itemize{\item Values greater than or equal to the found threshold
#' \emph{pass} the thresholding and values less than the threshold \emph{fail}
#' the thresholding.
#'
#' \item{For `ignore_white = TRUE`, if the maximum value in the array is one of
#' `2^8-1`, `2^16-1` or `2^32-1`, then those max values are ignored.
#' That's because they're the white values in 8, 16 and 32-bit images
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
#' @param img A 4-dimensional array in the style of an
#'   [ijtiff_img][ijtiff::ijtiff_img] (indexed by `img[y, x, channel, frame]`)
#'   or a 3-dimensional array which is a single channel of an
#'   [ijtiff_img][ijtiff::ijtiff_img] (indexed by `img[y, x, frame]`).
#' @param method The name of the thresholding method you wish to use. The
#'   available methods are `"IJDefault"`, `"Huang"`, `"Huang2"`, `"Intermodes"`,
#'   `"IsoData"`, `"Li"`, `"MaxEntropy"`, `"Mean"`, `"MinErrorI"`, `"Minimum"`,
#'   `"Moments"`, `"Otsu"`, `"Percentile"`, `"RenyiEntropy"`, `"Shanbhag"`,
#'   `"Triangle"` and `"Yen"`. Partial matching is performed i.e. `method = "h"`
#'   is enough to get you `"Huang"` and `method = "in"` is enough to get you
#'   `"Intermodes"`. To perform \emph{manual} thresholding (where you set the
#'   threshold yourself), supply the threshold here as a number e.g. `method =
#'   3.8` (so note that this would \emph{not} select the third method in the
#'   above list of methods). This manual threshold will then be used to
#'   threshold the sum stack to create a 2D mask and then this mask will be
#'   applied to all frames in the stack. If you want a different method for each
#'   channel, specify this parameter as a vector or list, one element per
#'   channel.
#' @param ignore_black Ignore black pixels/elements (zeros) when performing the
#'   thresholding?
#' @param ignore_white Ignore white pixels when performing the thresholding? If
#'   set to `TRUE`, the function makes a good guess as to what the white
#'   (saturated) value would be (see 'Details'). If this is set to a number, all
#'   pixels with value greater than or equal to that number are ignored.
#' @param fail When using `auto_thresh_apply_mask()`, to what value do you wish
#'   to set the pixels which fail to exceed the threshold? `fail = 'saturate'`
#'   sets them to saturated value (see 'Details'). `fail = 'zero'` sets them to
#'   zero. You can also specify directly here a natural number (must be between
#'   `0` and `2^16 - 1`) to use.
#' @param ignore_na This should be `TRUE` if `NA`s in `int_arr` should be
#'   ignored or `FALSE` if you want the presence of `NA`s in `int_arr` to throw
#'   an error.
#'
#' @return An object of class [stack_threshed_img] which is the thresholded
#'   image (an array in the style of an [ijtiff_img][ijtiff::ijtiff_img]).
#'   Pillars not exceeding the threshold are set to the `fail` value (default
#'   `NA`).
#'
#' @examples
#' img <- ijtiff::read_tif(system.file('extdata', '50.tif',
#'                                     package = 'autothresholdr'))
#' ijtiff::display(img[, , 1, 1])
#' img_thresh_mask <- mean_stack_thresh(img, 'Otsu')
#' ijtiff::display(img_thresh_mask[, , 1, 1])
#' ijtiff::display(img[, , 1, 1])
#' img_thresh_mask <- mean_stack_thresh(img, 'Huang')
#' ijtiff::display(img_thresh_mask[, , 1, 1])
#'
#' @export
mean_stack_thresh <- function(img, method, fail = NA,
                              ignore_black = FALSE, ignore_white = FALSE,
                              ignore_na = FALSE) {
  checkmate::assert_array(img, min.d = 3, max.d = 4)
  checkmate::assert_numeric(img, lower = 0)
  if (all(is.na(img))) {
    custom_stop(
      "`img` cannot be all `NA`s.",
      "Every element of your `img` is `NA`."
    )
  }
  if (max(img, na.rm = TRUE) > .Machine$integer.max) {
    custom_stop(
      "All elements of `img` must be in the integer range.",
      "
                Your maximum element is
                {format(max(img, na.rm = TRUE), scientific = FALSE)},
                which is greater than the maximum allowed value for an `int`,
                {format(.Machine$integer.max, scientific = FALSE)}.
                "
    )
  }
  if (!isTRUE(all.equal(floor(img), img, check.attributes = FALSE))) {
    img[is.na(img)] <- 0
    bad_index <- match(
      FALSE,
      as.vector(img) %>% {
      . != floor(.)
    })
    custom_stop(
      "`img` must be an array of integers.",
      "
      Element {format(bad_index, scientific = FALSE)} of `img` is
      {format(img[bad_index], scientific = FALSE)}, which is not an integer.
      "
    )
  }
  if (length(dim(img)) == 3) {
    dim(img) %<>% {
      c(.[1:2], 1, .[3])
    }
  }
  d <- dim(img)
  n_ch <- dim(img)[3]
  out <- array(as.vector(img), dim = d)
  if (length(method) == 1) method %<>% rep(n_ch)
  thresh <- as.list(seq_len(n_ch))
  for (i in seq_len(n_ch)) {
    if (length(unique(as.vector(img[, , i, ]))) == 1) {
      custom_stop(
        "Constant arrays cannot bbe thresholded.",
        "The array given for thresholding is constant. ",
        "All values are equal to {format(img[[1]], scientific = FALSE)}."
      )
    }
    fail <- translate_fail(img, fail)
    if (is.numeric(method[[i]])) {
      thresh[[i]] <- method[[i]]
    } else {
      sum_stack <- sum_pillars(img[, , i, ])
      scaling_factor <- 1 # we do this in case the sum stack has big elements
      max32int <- 2^31 - 1
      mx <- max(sum_stack)
      if (mx > max32int) {
        scaling_factor <- max32int / mx
        sum_stack <- round(sum_stack * scaling_factor)
      }
      thresh[[i]] <- auto_thresh(sum_stack, method[[i]],
        ignore_black = ignore_black,
        ignore_white = ignore_white,
        ignore_na = ignore_na
      ) %T>% {
        thresh_atts <- attributes(.)
        . <- . / (scaling_factor * d[4])
        attributes(.) <- thresh_atts
      }
    }
    if ((inherits(thresh[[i]], "integer")) &&
      (!isTRUE(checkmate::check_integerish(as.vector(thresh[[i]]))))) {
      class(thresh[[i]]) %<>% setdiff("integer")
    }
    mean_stack <- mean_pillars(img[, , i, ])
    mean_stack_mask <- mean_stack >= thresh[[i]]
    set_indices <- rep(!as.vector(mean_stack_mask), d[4])
    out[, , i, ][set_indices] <- fail
  }
  if (length(thresh) == 1) thresh <- thresh[[1]]
  stack_threshed_img(
    img = out, thresh = thresh, fail_value = fail,
    stack_thresh_method = "mean"
  )
}

#' Threshold every image frame in a stack based on their median.
#'
#' An [ijtiff_img][ijtiff::ijtiff_img] is a 4-dimensional array indexed by
#' `img[y, x, channel, frame]`. For each channel (which consists of a stack of
#' frames), this function finds a threshold based on all of the frames, then
#' takes the median of all the frames in the stack image, uses this to create a
#' mask with the found threshold and then applies this mask to every frame in
#' the stack (so for a given pillar in the image stack, either all the pixels
#' therein are thresholded away or all are untouched, where pillar `x,y` of
#' channel `ch` is `img[y, x, ch, ]`).
#'
#' \itemize{\item Values greater than or equal to the found threshold
#' \emph{pass} the thresholding and values less than the threshold \emph{fail}
#' the thresholding.
#'
#' \item{For `ignore_white = TRUE`, if the maximum value in the array is one of
#' `2^8-1`, `2^16-1` or `2^32-1`, then those max values are ignored.
#' That's because they're the white values in 8, 16 and 32-bit images
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
#' @param img A 3-dimensional array (the image stack, possibly a time-series of
#'   images) where the \eqn{n}th slice is the \eqn{n}th image in the stack.
#' @param method The name of the thresholding method you wish to use. The
#'   available methods are `"IJDefault"`, `"Huang"`, `"Huang2"`, `"Intermodes"`,
#'   `"IsoData"`, `"Li"`, `"MaxEntropy"`, `"Mean"`, `"MinErrorI"`, `"Minimum"`,
#'   `"Moments"`, `"Otsu"`, `"Percentile"`, `"RenyiEntropy"`, `"Shanbhag"`,
#'   `"Triangle"` and `"Yen"`. Partial matching is performed i.e. `method = "h"`
#'   is enough to get you `"Huang"` and `method = "in"` is enough to get you
#'   `"Intermodes"`. To perform \emph{manual} thresholding (where you set the
#'   threshold yourself), supply the threshold here as a number e.g. `method =
#'   3` (so note that this would \emph{not} select the third method in the above
#'   list of methods). This manual threshold will then be used to threshold the
#'   median stack to create a 2D mask and then this mask will be applied to all
#'   frames in the stack. If you want a different method for each channel,
#'   specify this parameter as a vector or list, one element per channel.
#' @param ignore_black Ignore black pixels/elements (zeros) when performing the
#'   thresholding?
#' @param ignore_white Ignore white pixels when performing the thresholding? If
#'   set to `TRUE`, the function makes a good guess as to what the white
#'   (saturated) value would be (see 'Details').
#' @param fail When using `auto_thresh_apply_mask()`, to what value do you wish
#'   to set the pixels which fail to exceed the threshold? `fail = 'saturate'`
#'   sets them to saturated value (see 'Details'). `fail = 'zero'` sets them to
#'   zero. You can also specify directly here a natural number (must be between
#'   `0` and `2^32 - 1`) to use.
#' @param ignore_na This should be `TRUE` if `NA`s in `int_arr` should be
#'   ignored or `FALSE` if you want the presence of `NA`s in `int_arr` to throw
#'   an error.
#'
#' @return An object of class [stack_threshed_img] which is the thresholded
#'   image (an array in the style of an [ijtiff_img][ijtiff::ijtiff_img]).
#'   Pillars not exceeding the threshold are set to the `fail` value (default
#'   `NA`).
#'
#' @examples
#' img <- ijtiff::read_tif(system.file('extdata', '50.tif',
#'                                     package = 'autothresholdr'))
#' ijtiff::display(img[, , 1, 1])
#' img_thresh_mask <- med_stack_thresh(img, 'Otsu')
#' ijtiff::display(img_thresh_mask[, , 1, 1])
#' ijtiff::display(img[, , 1, 1])
#' img_thresh_mask <- med_stack_thresh(img, 'Triangle')
#' ijtiff::display(img_thresh_mask[, , 1, 1])
#'
#' @export
med_stack_thresh <- function(img, method, fail = NA,
                             ignore_black = FALSE, ignore_white = FALSE,
                             ignore_na = FALSE) {
  checkmate::assert_array(img, min.d = 3, max.d = 4)
  checkmate::assert_numeric(img, lower = 0)
  if (all(is.na(img))) {
    custom_stop(
      "`img` cannot be all `NA`s.",
      "Every element of your `img` is `NA`."
    )
  }
  if (max(img, na.rm = TRUE) > .Machine$integer.max) {
    custom_stop(
      "All elements of `img` must be in the integer range.",
      "
      Your maximum element is
      {format(max(img, na.rm = TRUE), scientific = FALSE)},
      which is greater than the maximum allowed value for an `int`,
      {format(.Machine$integer.max, scientific = FALSE)}.
      "
    )
  }
  if (!isTRUE(all.equal(floor(img), img, check.attributes = FALSE))) {
    img[is.na(img)] <- 0
    bad_index <- match(
      FALSE,
      as.vector(img) %>% {
        . != floor(.)
      })
    custom_stop(
      "`img` must be an array of integers.",
      "
      Element {format(bad_index, scientific = FALSE)} of `img` is
      {format(img[bad_index], scientific = FALSE)}, which is not an integer.
      "
    )
  }
  if (length(dim(img)) == 3) {
    dim(img) %<>% {
      c(.[1:2], 1, .[3])
    }
  }
  d <- dim(img)
  n_ch <- dim(img)[3]
  out <- array(as.vector(img), dim = d)
  if (length(method) == 1) method %<>% rep(n_ch)
  thresh <- as.list(seq_len(n_ch))
  for (i in seq_len(n_ch)) {
    if (length(unique(as.vector(img[, , i, ]))) == 1) {
      custom_stop(
        "Constant arrays cannot bbe thresholded.",
        "The array given for thresholding is constant. ",
        "All values are equal to {format(img[[1]], scientific = FALSE)}."
      )
    }
    fail <- translate_fail(img, fail)
    thresh[[i]] <- auto_thresh(img[, , i, ], method[[i]],
      ignore_black = ignore_black,
      ignore_white = ignore_white,
      ignore_na = ignore_na
    )
    med_stack <- median_pillars(img[, , i, ])
    med_stack_mask <- med_stack >= thresh[[i]]
    set_indices <- rep(!as.vector(med_stack_mask), d[4])
    out[, , i, ][set_indices] <- fail
  }
  stack_threshed_img(
    img = out, thresh = thresh, fail_value = fail,
    stack_thresh_method = "median"
  )
}

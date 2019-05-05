img_detrend_smoothed <- function(arr3d, arr3d_smoothed, purpose, parallel) {
  checkmate::assert_string(purpose)
  purpose %<>% filesstrings::match_arg(c("fcs", "ffs"), ignore_case = TRUE)
  arr3d_smoothed[arr3d_smoothed < 0] <- 0
  deviations_from_smoothed <- arr3d - arr3d_smoothed
  pillar_means <- as.vector(mean_pillars(arr3d, parallel = parallel))
  if (purpose == "ffs") {
    variance_correction_factors <- square_root(pillar_means /
      arr3d_smoothed,
    parallel = parallel
    ) %T>% {
      .[!is.finite(.)] <- 1
    }
    deviations_from_smoothed <- deviations_from_smoothed *
      variance_correction_factors
    rm(variance_correction_factors)
  }
  out_real <- pillar_means + deviations_from_smoothed
  rm(deviations_from_smoothed)
  out_int <- floor(out_real) %>% {
    . + myrbern(out_real - ., parallel = parallel)
  }
  dim(out_int) <- dim(arr3d)
  out_int
}

img_detrend_tau_specified <- function(arr3d, tau, cutoff, purpose,
                                      parallel) {
  checkmate::assert_array(arr3d)
  if (length(dim(arr3d)) == 4 && dim(arr3d)[3] == 1) {
    dim(arr3d) %<>% {
      .[-3]
    }
  }
  checkmate::assert_array(arr3d, d = 3)
  if (is.na(tau)) return(arr3d)
  d <- dim(arr3d)
  l <- min(floor(-tau * log(cutoff)), d[3] %>% {
    (. - 1) + (. - 2)
  })
  smoothed <- exp_smooth_pillars(arr3d, tau, l, parallel = parallel)
  img_detrend_smoothed(arr3d, smoothed,
    purpose = purpose,
    parallel = parallel
  )
}

img_detrend_l_specified <- function(arr3d, l, purpose, parallel) {
  checkmate::assert_array(arr3d)
  if (length(dim(arr3d)) == 4 && dim(arr3d)[3] == 1) {
    dim(arr3d) %<>% {
      .[-3]
    }
  }
  checkmate::assert_array(arr3d, d = 3)
  if (is.na(l)) return(arr3d)
  d <- dim(arr3d)
  l <- min(l, d[3] %>% {
    (. - 1) + (. - 2)
  })
  smoothed <- boxcar_smooth_pillars(arr3d, l, parallel = parallel)
  img_detrend_smoothed(arr3d, smoothed,
    purpose = purpose,
    parallel = parallel
  )
}

img_detrend_degree_specified <- function(arr3d, degree, purpose,
                                         parallel) {
  checkmate::assert_array(arr3d)
  if (length(dim(arr3d)) == 4 && dim(arr3d)[3] == 1) {
    dim(arr3d) %<>% {
      .[-3]
    }
  }
  checkmate::assert_array(arr3d, d = 3)
  if (is.na(degree)) return(arr3d)
  d <- dim(arr3d)
  smoothed <- poly_fit_pillars(arr3d, degree, parallel = parallel)
  img_detrend_smoothed(arr3d, smoothed,
    purpose = purpose,
    parallel = parallel
  )
}

img_detrend_swaps_specified <- function(arr3d, swaps) {
  checkmate::assert_array(arr3d)
  if (length(dim(arr3d)) == 4 && dim(arr3d)[3] == 1) {
    dim(arr3d) %<>% {
      .[-3]
    }
  }
  checkmate::assert_array(arr3d, d = 3)
  checkmate::assert_numeric(arr3d, lower = 0, upper = .Machine$integer.max)
  checkmate::assert_integerish(arr3d)
  d <- dim(arr3d)
  frame_sums <- sum_frames(arr3d, na_rm = TRUE)
  frame_sum_mean <- mean(frame_sums)
  frame_weights <- frame_sums - frame_sum_mean
  frame_balls <- frame_weights %T>% {
    .[. < 0] <- 0
  }
  frame_ball_leftovers <- frame_balls %% 1
  frame_balls %<>% {
    floor(.) + rtoboxes(floor(sum(frame_ball_leftovers)), d[3],
      weights = frame_ball_leftovers,
      capacities = rep(1, d[3])
    )
  }
  frame_capacities <- -frame_weights %T>% {
    .[. < 0] <- 0
  }
  frame_capacity_leftovers <- frame_capacities %% 1
  frame_capacities %<>% {
    floor(.) + rtoboxes(floor(sum(frame_capacity_leftovers)), d[3],
      weights = frame_capacity_leftovers,
      capacities = rep(1, d[3])
    )
  }
  max_swaps <- min(sum(frame_balls), sum(frame_capacities))
  if (max_swaps == 0) {
    custom_stop(
      "Your image is too close to zero.",
      "Can't detrend an image with so few nonzero values.",
      "
      `img` has {length(arr3d)} elements and just {sum(arr3d > 0)}
      of them are greater than zero.
      "
    )
  }
  swaps %<>% min(max_swaps)
  weights <- frame_weights %T>% {
    .[. < 0] <- 0
  }
  frames_losing <- rfromboxes(
    n = swaps, balls = frame_balls,
    weights = weights
  )
  px_losing <- px_take_arr3d(arr3d, frames_losing, get_seed())
  arr3d %<>% {
    . - px_losing
  }
  px_getting <- sum_pillars(px_losing) %>%
    as.vector() %>%
    rep(seq_along(.), times = .) %>%
    arrayInd(.dim = d[1:2])
  weights <- (-frame_weights) %T>% {
    .[. < 0] <- 0
  }
  frames_getting <- rtoboxes(swaps, d[3],
    weights = weights,
    capacities = frame_capacities
  ) %>%
    rep(seq_along(.), times = .) %>%
    {
      if (length(.) <= 1) return(.)
      sample(.)
    }
  elems_getting <- cbind(px_getting, frames_getting) %>%
    myarray2vec(d)
  arr3d %<>% vec_add1s(elems_getting)
  arr3d
}


#' Detrend images.
#'
#' Correct images for bleaching (or any other effect that introduces an unwanted
#' trend) by *detrending*.
#'
#' There are 4 detrending methods available: *Robin Hood*, *boxcar*,
#' *exponential filtering* and *polynomial*. *Robin Hood* is described in Nolan
#' et al., 2018. The others are described in Nolan et al., 2017. \itemize{\item
#' *Robin Hood* is a method whereby counts are taken from frames with higher
#' mean intensity and given directly to frames of lower intensity. \item
#' *Boxcar* detrending with parameter \eqn{l} is a moving average detrending
#' method using a sliding window of size \eqn{2l + 1}. \item *Exponential
#' filtering* detrending is a moving weighted average method where for parameter
#' \eqn{tau} the weights are calculated as exp\eqn{(- t / tau)} where \eqn{t} is
#' the distance from the point of interest. \item *Polynomial* detrending works
#' by fitting a polynomial line to a series of points and then correcting the
#' series to remove the trend detailed by this polynomial fit.}
#'
#' @param img A 4-dimensional array in the style of an
#'   [ijtiff_img][ijtiff::ijtiff_img] (indexed by `img[y, x, channel, frame]`)
#'   or a 3-dimensional array which is a single channel of an
#'   [ijtiff_img][ijtiff::ijtiff_img] (indexed by `img[y, x, frame]`).
#' @param swaps The number of swaps (giving of one count from rich to poor) to
#'   perform during the _Robin Hood_ detrending. Set this to "auto" (the
#'   default) to use Nolan's algorithm to automatically find a suitable value
#'   for this parameter (recommended). For multi-channel images, it is possible
#'   to have a different `swaps` for each channel by specifying `swaps` as a
#'   vector or list.
#' @param l The length parameter for *boxcar* detrending. The size of the
#'   sliding window will be `2 * l + 1`. This must be a positive integer. Set
#'   this to "auto" to use Nolan's algorithm to automatically find a suitable
#'   value for this parameter (recommended). For multi-channel images, it is
#'   possible to have a different `l` for each channel by specifying `l` as a
#'   vector or list.
#' @param tau The \eqn{tau} parameter for *exponential filtering* detrending.
#'   This must be a positive number. Set this to "auto" to use Nolan's algorithm
#'   to automatically find a suitable value for this parameter (recommended).
#'   For multi-channel images, it is possible to have a different `tau` for each
#'   channel by specifying `tau` as a vector or list.
#' @param cutoff In *exponential filtering* detrending, for the weighted
#'   average, every point gets a weight. This can slow down the computation
#'   massively. However, many of the weights will be approximately zero. With
#'   cutoff, we say that any point with weight less than or equal to `cutoff`
#'   times the maximum weight may be ignored; so with `cutoff = 0.05`, any
#'   weight less than 5\% of the maximum weight may be ignored. The default
#'   value of this parameter is sensible and its value should not be set to
#'   anything else without good reason.
#' @param degree The degree of the polynomial to use for the polynomial
#'   detrending. This must be a positive integer. Set this to "auto" to use
#'   Nolan's algorithm to automatically find a suitable value for this parameter
#'   (recommended). For multi-channel images, it is possible to have a different
#'   `degree` for each channel by specifying `degree` as a vector or list.
#' @param purpose What type of calculation do you intend to perform on the
#'   detrended image? If it is an FFS (fluorescence fluctuation spectroscopy)
#'   calculation (like number and brightness), choose 'FFS'. If it is an FCS
#'   (fluorescence correlation spectroscopy) calculation (like cross-correlated
#'   number and brightness or autocorrelation), choose 'FCS'. The difference is
#'   that if `purpose` is 'FFS', the time series is corrected for non-stationary
#'   mean and variance, whereas if `purpose` is 'FCS', the time series is
#'   corrected for non-stationary mean only. `purpose`` is not required for
#'   _Robin Hood_ detrending.
#' @param parallel Would you like to use multiple cores to speed up this
#'   function? If so, set the number of cores here, or to use all available
#'   cores, use `parallel = TRUE`.
#'
#' @return The detrended image, an object of class [detrended_img].
#'
#' @name detrending
NULL



#' @rdname detrending
#'
#' @inheritParams best_swaps
#'
#' @references Rory Nolan, Luis A. J. Alvarez, Jonathan Elegheert, Maro
#'   Iliopoulou, G. Maria Jakobsdottir, Marina Rodriguez-Muñoz, A. Radu
#'   Aricescu, Sergi Padilla-Parra; nandb—number and brightness in R with a
#'   novel automatic detrending algorithm, Bioinformatics,
#'   https://doi.org/10.1093/bioinformatics/btx434.
#'
#' @examples
#' \dontrun{
#' ## These examples are not run on CRAN because they take too long.
#' ## You can still try them for yourself.
#'
#' img <- ijtiff::read_tif(system.file('extdata', 'bleached.tif',
#'                                     package = 'detrendr'))
#' corrected <- img_detrend_rh(img)
#' corrected <- img_detrend_boxcar(img, "auto", purpose = "fcs", parallel = 2)
#' corrected10 <- img_detrend_boxcar(img, 10, purpose = "fcs", parallel = 2)
#' corrected50 <- img_detrend_boxcar(img, 50, purpose = "fcs", parallel = 2)
#' corrected <- img_detrend_exp(img, "auto", purpose = "ffs", parallel = 2)
#' corrected10 <- img_detrend_exp(img, 10, purpose = "ffs", parallel = 2)
#' corrected50 <- img_detrend_exp(img, 50, purpose = "fcs", parallel = 2)
#' corrected <- img_detrend_polynom(img, "auto", purpose = "ffs", parallel = 2)
#' corrected2 <- img_detrend_polynom(img, 2, purpose = "ffs", parallel = 2)
#' }
#' @export
img_detrend_robinhood <- function(img, swaps = "auto", quick = FALSE) {
  checkmate::assert_array(img, min.d = 3, max.d = 4)
  checkmate::assert_integerish(img, lower = 0)
  img %<>% ijtiff::as_ijtiff_img()
  n_ch <- dim(img)[3]
  out <- array(0, dim = dim(img))
  if (length(swaps) == 1) swaps %<>% rep(n_ch)
  swaps %<>% as.list()
  if (length(swaps) != n_ch) {
    custom_stop("
      Argument `swaps` must have length 1 or length equal to
      the number of channels.
      ", "
      Your `swaps` argument has length {length(swaps)} and your
      image has {n_ch} channels.
      ")
  }
  auto <- rep(FALSE, n_ch)
  for (i in seq_len(n_ch)) {
    if (is.na(swaps[[i]])) {
      out[, , i, ] <- img[, , i, ]
    } else if (is.numeric(swaps[[i]]) |
      (is.character(swaps[[i]]) &&
        filesstrings::can_be_numeric(swaps[[i]]))) {
      swaps[[i]] %<>% as.numeric() %>% floor(.)
      if (swaps[[i]] == 0) {
        out[, , i, ] <- img[, , i, ]
      } else {
        if (swaps[[i]] < 0) {
          custom_stop(
            "`swaps` must be greater than or equal to zero.",
            "You have `swaps` equal to {swaps[[i]]}."
          )
        }
        out[, , i, ] <- img_detrend_swaps_specified(
          img[, , i, , drop = FALSE],
          swaps[[i]]
        ) %>%
          as.vector()
      }
    } else if (is.character(swaps[[i]])) {
      swaps[[i]] %<>% tolower()
      if (stringi::stri_startswith_coll("auto", swaps[[i]])) {
        auto[[i]] <- TRUE
        swaps[[i]] <- best_swaps(img[, , i, , drop = FALSE], quick = quick)
        out[, , i, ] <- img_detrend_swaps_specified(
          img[, , i, , drop = FALSE],
          swaps[[i]]
        ) %>%
          as.vector()
      } else {
        custom_stop(
          "If `swaps` is a string, the only permissible value is 'auto'.",
          "You have used '{swaps[[i]]}'."
        )
      }
    } else {
      custom_stop(
        "`swaps` must be specified as a positive number or as 'auto'.",
        "You have used '{swaps[[i]]}'."
      )
    }
  }
  detrended_img(out, "rh", as.integer(unlist(swaps)), auto)
}

#' @rdname detrending
#' @export
img_detrend_rh <- img_detrend_robinhood

#' @rdname detrending
#' @export
img_detrend_boxcar <- function(img, l, purpose = c("FCS", "FFS"),
                               parallel = FALSE) {
  checkmate::assert_array(img, min.d = 3, max.d = 4)
  checkmate::assert_numeric(img, lower = 0)
  if (filesstrings::all_equal(purpose, c("FCS", "FFS"))) {
    custom_stop("You must choose *either* 'FCS' *or* 'FFS' for `purpose`.")
  }
  checkmate::assert_string(purpose)
  purpose %<>% filesstrings::match_arg(c("fcs", "ffs"), ignore_case = TRUE)
  img %<>% ijtiff::as_ijtiff_img()
  n_ch <- dim(img)[3]
  out <- array(0, dim = dim(img))
  if (length(l) == 1) l %<>% rep(n_ch)
  l %<>% as.list()
  if (length(l) != n_ch) {
    custom_stop("
      Argument `l` must have length 1 or length equal to the number of channels.
      ", "
      Your l argument has length {length(l)} and your image has
      {n_ch} channels.")
  }
  auto <- rep(FALSE, n_ch)
  for (i in seq_len(n_ch)) {
    if (is.na(l[[i]])) {
      out[, , i, ] <- img[, , i, ]
    } else if (is.numeric(l[[i]]) |
      (is.character(l[[i]]) &&
        filesstrings::can_be_numeric(l[[i]]))) {
      l[[i]] %<>% as.numeric()
      if (!isTRUE(checkmate::check_int(l[[i]]))) {
        custom_stop(
          "`l` must be an integer.",
          "You have `l` equal to {l[[i]]}."
        )
      }
      if (l[[i]] <= 0) {
        custom_stop(
          "`l` must be greater than zero.",
          "You have `l` equal to {l[[i]]}."
        )
      }
      out[, , i, ] <- img_detrend_l_specified(img[, , i, , drop = FALSE],
        l[[i]],
        purpose = purpose,
        parallel = parallel
      ) %>%
        as.vector()
    } else if (is.character(l[[i]])) {
      l[[i]] %<>% tolower()
      if (stringi::stri_startswith_coll("auto", l[[i]])) {
        auto[[i]] <- TRUE
        l[[i]] <- best_l(img[, , i, , drop = FALSE],
          parallel = parallel, purpose = purpose
        )
        out[, , i, ] <- img_detrend_l_specified(img[, , i, ], l[[i]],
          purpose = purpose,
          parallel = parallel
        ) %>%
          as.vector()
      } else {
        custom_stop(
          "If `l` is a string, the only permissible value is 'auto'.",
          "You have used '{l[[i]]}."
        )
      }
    } else {
      custom_stop(
        "`l` must be specified as a positive number or as 'auto'.",
        "You have used '{l[[i]]}'."
      )
    }
  }
  detrended_img(out, "boxcar", as.integer(unlist(l)), auto, purpose = purpose)
}

#' @rdname detrending
#' @export
img_detrend_exp <- function(img, tau, cutoff = 0.05, purpose = c("FCS", "FFS"),
                            parallel = FALSE) {
  checkmate::assert_array(img, min.d = 3, max.d = 4)
  if (filesstrings::all_equal(purpose, c("FCS", "FFS"))) {
    custom_stop("You must choose *either* 'FCS' *or* 'FFS' for `purpose`.")
  }
  checkmate::assert_string(purpose)
  purpose %<>% filesstrings::match_arg(c("fcs", "ffs"), ignore_case = TRUE)
  img %<>% ijtiff::as_ijtiff_img()
  n_ch <- dim(img)[3]
  out <- array(0, dim = dim(img))
  if (length(tau) == 1) tau %<>% rep(n_ch)
  tau %<>% as.list()
  if (length(tau) != n_ch) {
    custom_stop("
      Argument `tau` must have length 1 or length equal to
      the number of channels.
      ", "
      Your `tau` argument has length {length(tau)} and your
      image has {n_ch} channels.
      ")
  }
  auto <- rep(FALSE, n_ch)
  for (i in seq_len(n_ch)) {
    if (is.na(tau[[i]])) {
      out[, , i, ] <- img[, , i, ]
    } else if (is.numeric(tau[[i]]) |
      (is.character(tau[[i]]) &&
        filesstrings::can_be_numeric(tau[[i]]))) {
      tau[[i]] %<>% as.numeric() %>% floor(.)
      if (tau[[i]] <= 0) {
        custom_stop(
          "`tau` must be greater than zero.",
          "You have `tau` equal to {tau[[i]]}."
        )
      }
      out[, , i, ] <- img_detrend_tau_specified(img[, , i, , drop = FALSE],
        tau[[i]], cutoff,
        purpose = purpose,
        parallel = parallel
      ) %>%
        as.vector()
    } else if (is.character(tau[[i]])) {
      tau[[i]] %<>% tolower()
      if (stringi::stri_startswith_coll("auto", tau[[i]])) {
        auto[[i]] <- TRUE
        tau[[i]] <- best_tau(img[, , i, , drop = FALSE],
          cutoff = cutoff, purpose = purpose,
          parallel = parallel
        )
        out[, , i, ] <- img_detrend_tau_specified(img[, , i, , drop = FALSE],
          tau[[i]], cutoff,
          purpose = purpose,
          parallel = parallel
        ) %>%
          as.vector()
      } else {
        custom_stop(
          "If `tau` is a string, the only permissible value is 'auto'.",
          "You have used '{tau[[i]]}'."
        )
      }
    } else {
      custom_stop(
        "`tau` must be specified as a positive number or as 'auto'.",
        "You have used {tau[[i]]}'."
      )
    }
  }
  detrended_img(out, "exponential", as.numeric(unlist(tau)), auto,
    purpose = purpose
  )
}

#' @rdname detrending
#' @export
img_detrend_polynom <- function(img, degree, purpose = c("FCS", "FFS"),
                                parallel = FALSE) {
  checkmate::assert_array(img, min.d = 3, max.d = 4)
  if (filesstrings::all_equal(purpose, c("FCS", "FFS"))) {
    custom_stop("You must choose *either* 'FCS' *or* 'FFS' for `purpose`.")
  }
  checkmate::assert_string(purpose)
  purpose %<>% filesstrings::match_arg(c("fcs", "ffs"), ignore_case = TRUE)
  img %<>% ijtiff::as_ijtiff_img()
  n_ch <- dim(img)[3]
  out <- array(0, dim = dim(img))
  if (length(degree) == 1) degree %<>% rep(n_ch)
  degree %<>% as.list()
  if (length(degree) != n_ch) {
    custom_stop("
      Argument degree must have length 1 or length equal to
      the number of channels
      ", "
      Your degree argument has length {length(degree)} and your
      image has {n_ch} channels.
      ")
  }
  auto <- rep(FALSE, n_ch)
  for (i in seq_len(n_ch)) {
    if (is.na(degree[[i]])) {
      out[, , i, ] <- img[, , i, ]
    } else if (is.numeric(degree[[i]]) |
      (is.character(degree[[i]]) &&
        filesstrings::can_be_numeric(degree[[i]]))) {
      degree[[i]] %<>% as.numeric()
      if (!isTRUE(checkmate::check_int(degree[[i]]))) {
        custom_stop(
          "`degree` must be an integer.",
          "You have `degree` equal to {degree[[i]]}."
        )
      }
      if (degree[[i]] <= 0) {
        custom_stop(
          "`degree` must be greater than zero.",
          "You have `degree` equal to {degree[[i]]}."
        )
      }
      out[, , i, ] <- img_detrend_degree_specified(img[, , i, , drop = FALSE],
        degree[[i]],
        purpose = purpose,
        parallel = parallel
      ) %>%
        as.vector()
    } else if (is.character(degree[[i]])) {
      degree[[i]] %<>% tolower()
      if (stringi::stri_startswith_coll("auto", degree[[i]])) {
        degree[[i]] <- best_degree(img[, , i, , drop = FALSE],
          purpose = purpose, parallel = parallel
        )
        out[, , i, ] <- img_detrend_degree_specified(img[, , i, , drop = FALSE],
          degree[[i]],
          purpose = purpose,
          parallel = parallel
        ) %>%
          as.vector()
      } else {
        custom_stop(
          "If `degree` is a string, the only permissible value is 'auto'.",
          "You have used '{degree[[i]]}'."
        )
      }
    } else {
      custom_stop(
        "`degree` must be specified as a positive number or as 'auto'.",
        "You have used '{degree[[i]]}'."
      )
    }
  }
  detrended_img(out, "polynomial", as.numeric(unlist(degree)), auto,
    purpose = purpose
  )
}

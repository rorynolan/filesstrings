rows_detrend_tau_specified <- function(mat, tau, l, purpose, parallel) {
  smoothed <- exp_smooth_rows(mat, tau, l, parallel = parallel)
  rows_detrend_smoothed(mat, smoothed,
    purpose = purpose,
    parallel = parallel
  )
}

rows_detrend_tau_specified_mean_b <- function(mat, tau, l, purpose,
                                              parallel) {
  rows_detrend_tau_specified(mat, tau, l,
    purpose = purpose,
    parallel = parallel
  ) %>%
    brightness_rows(parallel = parallel) %>%
    mean(na.rm = TRUE)
}

#' Find the best tau parameter for exponential smoothing detrending.
#'
#' Use Nolan's algorithm to find the ideal tau parameter for exponential
#' smoothing detrending.
#'
#' @inheritParams detrending
#'
#' @return If no detrend is necessary, this function returns `NA`. If a detrend
#'   is required, this function returns a natural number which is the ideal
#'   `tau` parameter for exponential smoothing detrending. If there are multiple
#'   channels, the function returns a vector, one `tau` parameter for each
#'   channel.
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
#' img <- ijtiff::read_tif(system.file('extdata', 'bleached.tif',
#'                                     package = 'detrendr'))[, , 1, ]
#' best_tau(img, parallel = 2)
#' }
#'
#' @export
best_tau <- function(img, cutoff = 0.05, parallel = FALSE,
                     purpose = c("FCS", "FFS")) {
  checkmate::assert_numeric(img, lower = 0)
  checkmate::assert_array(img, min.d = 3, max.d = 4)
  if (filesstrings::all_equal(img)) {
    stop(
      "Your image is constant: all pixel values are equal to ",
      img[[1]], ". This type of image is not detrendable."
    )
  }
  if (filesstrings::all_equal(purpose, c("FCS", "FFS"))) {
    stop("You must choose *either* 'FCS' *or* 'FFS' for `purpose`.")
  }
  purpose %<>% filesstrings::match_arg(c("FCS", "FFS"), ignore_case = TRUE)
  checkmate::assert(
    checkmate::check_flag(parallel),
    checkmate::check_count(parallel)
  )
  d <- dim(img)
  if (length(d) == 4 && d[3] == 1) {
    d <- d[-3]
    dim(img) <- d
  }
  if (length(d) == 3) {
    frame_length <- sum(!anyNA_pillars(img))
    frame_means <- apply(img, 3, mean, na.rm = TRUE)
    sim_brightness <- NA
    for (i in 0:9) {
      if (is.na(sim_brightness)) {
        sim_mat <- myrpois_frames(frame_means, frame_length, parallel)
        if (!filesstrings::all_equal(sim_mat)) {
          sim_brightness <- brightness_rows(sim_mat, parallel = parallel) %>%
            mean(na.rm = TRUE)
        }
      }
    }
    msg <- paste(
      "Your image is too close to zero. Can't detrend an image with",
      "so few nonzero values. \n* `img` has",
      length(img), "elements",
      "and just", sum(img > 0), "of them are greater than zero."
    )
    if (is.na(sim_brightness)) stop(msg)
    if (sim_brightness <= 1) return(NA)
    big_tau <- 100
    big_tau_old <- big_tau
    max_l <- ncol(sim_mat) %>% {
      (. - 1) + (. - 2)
    }
    l <- min(floor(-big_tau * log(cutoff)), max_l)
    mean_brightness_big_tau <- rows_detrend_tau_specified_mean_b(
      sim_mat, big_tau, l,
      purpose = "ffs", parallel = parallel
    )
    if (is.na(mean_brightness_big_tau)) stop(msg)
    mean_brightness_big_tau_old <- mean_brightness_big_tau
    while (mean_brightness_big_tau <= 1) {
      big_tau_old <- big_tau
      big_tau <- 2 * big_tau
      l <- min(floor(-big_tau * log(cutoff)), max_l)
      mean_brightness_big_tau <- rows_detrend_tau_specified_mean_b(
        sim_mat, big_tau, l,
        purpose = "ffs", parallel = parallel
      )
      if (is.na(mean_brightness_big_tau)) stop(msg)
    }
    if (big_tau_old == big_tau) {
      while (mean_brightness_big_tau_old > 1) {
        big_tau_old <- big_tau_old / 2
        l <- min(floor(-big_tau_old * log(cutoff)), max_l)
        mean_brightness_big_tau_old <- rows_detrend_tau_specified_mean_b(
          sim_mat, big_tau_old, l,
          purpose = "ffs", parallel = parallel
        )
        if (is.na(mean_brightness_big_tau_old)) stop(msg)
      }
    }
    if (mean_brightness_big_tau_old == 1) stop(msg)
    tau_upper <- big_tau
    tau_lower <- big_tau_old
    mean_brightness_tau_upper <- mean_brightness_big_tau
    mean_brightness_tau_lower <- mean_brightness_big_tau_old
    while (tau_upper - tau_lower > 1) {
      middle_tau <- mean(c(tau_lower, tau_upper))
      l <- min(floor(-middle_tau * log(cutoff)), max_l)
      middle_brightness_mean <- rows_detrend_tau_specified_mean_b(
        sim_mat, middle_tau, l,
        purpose = "ffs", parallel = parallel
      )
      if (is.na(middle_brightness_mean)) stop(msg)
      if (middle_brightness_mean < 1) {
        tau_lower <- middle_tau
        mean_brightness_tau_lower <- middle_brightness_mean
      } else if (middle_brightness_mean > 1) {
        tau_upper <- middle_tau
        mean_brightness_tau_upper <- middle_brightness_mean
      } else {
        return(round(middle_tau)) # very unlikely to be needed
      }
    }
    upper_closer <- abs(mean_brightness_tau_upper - 1) >
      abs(mean_brightness_tau_lower - 1)
    dplyr::if_else(upper_closer, tau_upper, tau_lower)
  } else {
    purrr::map_dbl(
      seq_len(d[3]),
      ~best_tau(img[, , ., , drop = FALSE],
        cutoff = cutoff, purpose = purpose,
        parallel = parallel
      )
    )
  }
}

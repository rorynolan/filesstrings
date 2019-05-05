rows_detrend_smoothed <- function(mat, mat_smoothed, purpose, parallel) {
  checkmate::assert_string(purpose)
  purpose %<>% stringr::str_to_lower()
  assertthat::assert_that(purpose %in% c("fcs", "ffs"))
  deviations_from_smoothed <- mat - mat_smoothed
  row_means <- mean_rows(mat, parallel = parallel)
  if (purpose == "ffs") {
    variance_correction_factors <- square_root(row_means / mat_smoothed,
      parallel = parallel
    ) %T>% {
      .[!is.finite(.)] <- 1
    }
    deviations_from_smoothed <- deviations_from_smoothed *
      variance_correction_factors
    rm(variance_correction_factors)
  }
  out_real <- row_means + deviations_from_smoothed
  rm(deviations_from_smoothed)
  out_int <- floor(out_real) %>% {
    . + myrbern(out_real - ., parallel = parallel)
  } %T>% {
    .[. < 0] <- 0
  }
  dim(out_int) <- dim(mat)
  out_int
}

rows_detrend_l_specified <- function(mat, l, purpose, parallel) {
  smoothed <- boxcar_smooth_rows(mat, l, parallel = parallel)
  rows_detrend_smoothed(mat, smoothed,
    purpose = purpose,
    parallel = parallel
  )
}

rows_detrend_l_specified_mean_b <- function(mat, l, purpose, parallel) {
  rows_detrend_l_specified(mat, l,
    purpose = purpose,
    parallel = parallel
  ) %>%
    brightness_rows(parallel = parallel) %>%
    mean(na.rm = TRUE)
}


#' Find the best length parameter for boxcar detrending.
#'
#' Use Nolan's algorithm to find the ideal length parameter for boxcar
#' detrending. Boxcar detrending is also referred to as 'running average'.
#'
#' @inheritParams detrending
#'
#' @return If no detrend is necessary, this function returns `NA`. If a detrend
#'   is required, this function returns a natural number which is the ideal
#'   length parameter for boxcar detrending. If there are multiple channels, the
#'   function returns a vector, one `l` parameter for each channel.
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
#'                                     package = 'detrendr'))
#' best_l(img, parallel = 2, purpose = "FFS")
#' }
#'
#' @export
best_l <- function(img, parallel = FALSE, purpose = c("FCS", "FFS")) {
  checkmate::assert_numeric(img, lower = 0)
  checkmate::assert_array(img, min.d = 3, max.d = 4)
  if (filesstrings::all_equal(img)) {
    custom_stop(
      "Your image is constant: all pixel values are equal to {img[[1]]}.",
      "This type of image is not detrendable."
    )
  }
  if (filesstrings::all_equal(purpose, c("FCS", "FFS"))) {
    custom_stop("You must choose *either* 'FCS' *or* 'FFS' for `purpose`.")
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
    maxl <- d[3] - 1
    big_l <- min(10, maxl)
    big_l_old <- big_l
    max_l <- ncol(sim_mat) %>% {
      (. - 1) + (. - 2)
    }
    mean_brightness_big_l <- rows_detrend_l_specified_mean_b(
      sim_mat, big_l,
      purpose = "ffs", parallel = parallel
    )
    if (is.na(mean_brightness_big_l)) stop(msg)
    mean_brightness_big_l_old <- mean_brightness_big_l
    while (mean_brightness_big_l <= 1) {
      big_l_old <- big_l
      big_l <- min(maxl, 2 * big_l)
      mean_brightness_big_l <- rows_detrend_l_specified_mean_b(
        sim_mat, big_l,
        purpose = "ffs", parallel = parallel
      )
      if (is.na(mean_brightness_big_l)) stop(msg)
    }
    if (big_l_old == big_l) {
      while (mean_brightness_big_l_old > 1) {
        big_l_old <- big_l_old %/% 2
        if (big_l_old == 0) {
          custom_stop(
            "
            Even with box size `l = 1` (the most severe detrend),
            the brightness B was still above 1.
            ",
            "There is probably something wrong with your data."
          )
        }
        mean_brightness_big_l_old <- rows_detrend_l_specified_mean_b(
          sim_mat, big_l_old,
          purpose = "ffs", parallel = parallel
        )
        if (is.na(mean_brightness_big_l_old)) stop(msg)
      }
    }
    if (mean_brightness_big_l_old == 1) return(round(big_l_old))
    l_upper <- big_l
    l_lower <- big_l_old
    mean_brightness_l_upper <- mean_brightness_big_l
    mean_brightness_l_lower <- mean_brightness_big_l_old
    while (l_upper - l_lower > 1) {
      middle_l <- round(mean(c(l_lower, l_upper)))
      middle_brightness_mean <- rows_detrend_l_specified_mean_b(
        sim_mat, middle_l,
        purpose = "ffs", parallel = parallel
      )
      if (is.na(middle_brightness_mean)) stop(msg)
      if (middle_brightness_mean < 1) {
        l_lower <- middle_l
        mean_brightness_l_lower <- middle_brightness_mean
      } else if (middle_brightness_mean > 1) {
        l_upper <- middle_l
        mean_brightness_l_upper <- middle_brightness_mean
      } else {
        return(middle_l) # very unlikely to be needed
      }
    }
    upper_closer <- abs(mean_brightness_l_upper - 1) >
      abs(mean_brightness_l_lower - 1)
    dplyr::if_else(upper_closer, l_upper, l_lower) %>%
      as.integer()
  } else {
    purrr::map_int(
      seq_len(d[3]),
      ~best_l(img[, , ., , drop = FALSE],
        purpose = purpose,
        parallel = parallel
      )
    )
  }
}

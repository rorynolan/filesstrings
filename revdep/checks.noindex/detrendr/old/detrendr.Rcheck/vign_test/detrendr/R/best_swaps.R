mat_swap_n_more <- function(mat, mat_orig, n, frame_weights, frame_balls,
                            frame_capacities) {
  weights <- frame_weights %T>% {
    .[. < 0] <- 0
  }
  frames_losing <- rfromboxes(
    n = n, balls = frame_balls,
    weights = weights
  )
  loser_frames <- dplyr::tibble(
    frame = which(as.logical(frames_losing)),
    amount = frames_losing[frame]
  )
  px_losing <- px_take_mat(mat, mat_orig,
    frames_losing = frames_losing,
    seed = get_seed()
  )
  mat <- mat - px_losing
  px_getting <- rowSums(px_losing) %>%
    rep(seq_along(.), times = .)
  weights <- (-frame_weights) %T>% {
    .[. < 0] <- 0
  }
  frames_getting <- rtoboxes(n, ncol(mat),
    weights = weights,
    capacities = frame_capacities
  ) %>%
    rep(seq_along(.), times = .) %>%
    {
      if (length(.) <= 1) return(.)
      sample(.)
    }
  winner_frames <- plyr::count(frames_getting) %>%
    dplyr::rename(frame = "x", amount = "freq")
  elems_getting <- cbind(px_getting, frames_getting)
  mat %<>% mat_add1s(elems_getting)
  structure(mat,
    loser_frames = loser_frames, winner_frames = winner_frames
  )
}

#' Find the best `swaps` parameter for _Robin Hood_ detrending.
#'
#' Use Nolan's algorithm to find the ideal `swaps` parameter for _Robin Hood_
#' detrending.
#'
#' @inheritParams detrending
#' @param quick If `FALSE` (the default), the swap finding routine is run
#'   several times to get a consensus for the best parameter. If `TRUE`, the
#'   swap finding routine is run only once.
#'
#' @return A natural number. The ideal `swaps` parameter for boxcar detrending.
#'   If there are multiple channels, the function returns a vector, one `swaps`
#'   parameter for each channel.
#'
#' @examples
#' \dontrun{
#' ## These examples are not run on CRAN because they take too long.
#' ## You can still try them for yourself.
#' img <- ijtiff::read_tif(system.file('extdata', 'bleached.tif',
#'                                     package = 'detrendr'))
#' best_swaps(img)}
#'
#' @export
best_swaps <- function(img, quick = FALSE) {
  checkmate::assert_integerish(img, lower = 0)
  checkmate::assert_array(img, min.d = 3, max.d = 4)
  if (filesstrings::all_equal(img)) {
    custom_stop(
      "Your image is constant: all pixel values are equal to {img[[1]]}",
      "This type of image is not detrendable."
    )
  }
  checkmate::assert_flag(quick)
  d <- dim(img)
  if (length(d) == 4 && d[3] == 1) {
    d <- d[-3]
    dim(img) <- d
  }
  if (length(d) == 3) {
    if (quick) {
      out <- (best_swaps_naive(img) - best_swaps_naive(pois_mean_img(img))) %>%
        sigmoid::relu() %>%
        as.integer()
      return(out)
    }
    newest <- purrr::rerun(9, best_swaps_naive(img)) %>%
      purrr::map_int(1)
    if (filesstrings::all_equal(stats::median(newest), 0)) return(0L)
    mean_b_epss <- purrr::map_dbl(newest, ~mean_b_eps(img, .))
    mean_b_epss_std_mad_rel <- std_mad_rel(mean_b_epss)
    while (mean_b_epss_std_mad_rel > 0.05) {
      new_best_swaps_naive <- best_swaps_naive(img)
      newest %<>% c(new_best_swaps_naive)
      if (filesstrings::all_equal(stats::median(newest), 0)) return(0L)
      mean_b_epss %<>% c(mean_b_eps(img, new_best_swaps_naive))
      mean_b_epss_std_mad_rel <- std_mad_rel(mean_b_epss)
    }
    overestimates <- purrr::rerun(
      length(newest),
      best_swaps_naive(pois_mean_img(img))
    ) %>%
      purrr::map_int(1)
    (stats::median(newest) - stats::median(overestimates)) %>%
      sigmoid::relu() %>%
      as.integer()
  } else {
    purrr::map_int(seq_len(d[3]), ~best_swaps(img[, , ., , drop = FALSE],
      quick = quick
    ))
  }
}

mean_b_eps <- function(img, swaps) {
  checkmate::assert_array(img, d = 3)
  checkmate::assert_int(swaps, lower = 0)
  img_detrend_swaps_specified(img, swaps) %>%
    brightness_pillars() %>%
    mean(na.rm = TRUE) %>%
    {
      . - 1
    }
}

std_mad_rel <- function(x) {
  checkmate::assert_numeric(x, any.missing = FALSE, min.len = 1)
  stats::mad(x) / abs(stats::median(x)) / length(x)
}

pois_mean_img <- function(img) {
  img %T>% {
    nas <- is.na(as.vector(img))
    .[!nas] <- stats::rpois(sum(!nas), mean(img, na.rm = TRUE))
  }
}

best_swaps_naive <- function(img) {
  checkmate::assert_array(img, d = 3)
  d <- dim(img)
  n_frames <- d[3]
  frame_length <- sum(!anyNA_pillars(img))
  frame_means <- apply(img, 3, mean, na.rm = TRUE)
  sim_mean_b <- NA
  for (i in seq_len(99)) {
    if (is.na(sim_mean_b)) {
      sim_mat <- myrpois_frames(frame_means, frame_length)
      if (!filesstrings::all_equal(sim_mat)) {
        sim_mean_b <- brightness_rows(sim_mat) %>%
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
  if (is.na(sim_mean_b)) stop(msg)
  if (sim_mean_b <= 1) return(0L)
  sim_px_sums <- sum_rows(sim_mat)
  sim_px_means <- sim_px_sums / ncol(sim_mat)
  sim_frame_sums <- sum_cols(sim_mat)
  sim_frame_sum_mean <- mean(sim_frame_sums)
  frame_weights <- sim_frame_sums - sim_frame_sum_mean
  frame_balls <- frame_weights %T>% {
    .[. < 0] <- 0
  }
  frame_ball_leftovers <- frame_balls %% 1
  frame_balls %<>% {
    floor(.) + rtoboxes(floor(sum(frame_ball_leftovers)), n_frames,
      weights = frame_ball_leftovers,
      capacities = rep(1, n_frames)
    )
  }
  frame_capacities <- -frame_weights %T>% {
    .[. < 0] <- 0
  }
  frame_capacity_leftovers <- frame_capacities %% 1
  frame_capacities %<>% {
    floor(.) + rtoboxes(floor(sum(frame_capacity_leftovers)), n_frames,
      weights = frame_capacity_leftovers,
      capacities = rep(1, n_frames)
    )
  }
  max_swaps <- min(sum(frame_balls), sum(frame_capacities))
  if (max_swaps == 0) stop(msg)
  max_swaps_remaining <- max_swaps
  sim_mat_swapped_more <- sim_mat_swapped_fewer <- sim_mat
  n_swapped_more <- n_swapped_fewer <- 0
  sim_mat_swapped_more_mean_b <- sim_mat_swapped_fewer_mean_b <- sim_mean_b
  frame_balls_fewer <- frame_balls_more <- frame_balls
  frame_capacities_fewer <- frame_capacities_more <- frame_capacities
  now_swapping <- 1
  while (sim_mat_swapped_more_mean_b > 1) {
    if (max_swaps_remaining == 0) stop(msg)
    sim_mat_swapped_fewer <- sim_mat_swapped_more
    n_swapped_fewer <- n_swapped_more
    sim_mat_swapped_fewer_mean_b <- sim_mat_swapped_more_mean_b
    frame_balls_fewer <- frame_balls_more
    frame_capacities_fewer <- frame_capacities_more
    sim_mat_swapped_more %<>% mat_swap_n_more(sim_mat,
      n = now_swapping,
      frame_weights,
      frame_balls_more,
      frame_capacities_more
    )
    n_swapped_more %<>% {
      . + now_swapping
    }
    loser_frames <- attr(sim_mat_swapped_more, "loser_frames")
    winner_frames <- attr(sim_mat_swapped_more, "winner_frames")
    frame_balls_more[loser_frames$frame] %<>% {
      . - loser_frames$amount
    }
    frame_capacities_more[winner_frames$frame] %<>% {
      . - winner_frames$amount
    }
    sim_mat_swapped_more_mean_b <- sim_mat_swapped_more %>%
      brightness_rows_given_mean(sim_px_means) %>%
      mean(na.rm = TRUE)
    max_swaps_remaining %<>% {
      . - now_swapping
    }
    now_swapping %<>% {
      min(2 * ., max_swaps_remaining)
    }
    if (max_swaps_remaining == 0) break
  }
  if (sim_mat_swapped_more_mean_b <= 1) { # using brightness
    while (TRUE) {
      n_swapped_middle <- round(mean(c(n_swapped_fewer, n_swapped_more)))
      if (n_swapped_middle %in% c(n_swapped_fewer, n_swapped_more)) {
        more_closer <- abs(sim_mat_swapped_more_mean_b - 1) <
          abs(sim_mat_swapped_fewer_mean_b - 1)
        out <- dplyr::if_else(
          more_closer,
          n_swapped_more, n_swapped_fewer
        )
        break
      }
      now_swapping <- n_swapped_middle - n_swapped_fewer
      sim_mat_swapped_middle <- sim_mat_swapped_fewer %>%
        mat_swap_n_more(sim_mat, now_swapping,
          frame_weights,
          frame_balls = frame_balls_fewer,
          frame_capacities = frame_capacities_fewer
        )
      sim_mat_swapped_middle_mean_b <- sim_mat_swapped_middle %>%
        brightness_rows_given_mean(sim_px_means) %>%
        mean(na.rm = TRUE)
      loser_frames <- attr(sim_mat_swapped_middle, "loser_frames")
      winner_frames <- attr(sim_mat_swapped_middle, "winner_frames")
      frame_balls_middle <- frame_balls_fewer %T>% {
        .[loser_frames$frame] <- .[loser_frames$frame] - loser_frames$amount
      }
      frame_capacities_middle <- frame_capacities_fewer %T>% {
        .[winner_frames$frame] <-
          .[winner_frames$frame] - winner_frames$amount
      }
      if (sim_mat_swapped_middle_mean_b < 1) {
        sim_mat_swapped_more <- sim_mat_swapped_middle
        sim_mat_swapped_more_mean_b <- sim_mat_swapped_middle_mean_b
        frame_balls_more <- frame_balls_middle
        frame_capacities_more <- frame_capacities_middle
        n_swapped_more <- n_swapped_middle
      } else if (sim_mat_swapped_middle_mean_b > 1) {
        sim_mat_swapped_fewer <- sim_mat_swapped_middle
        sim_mat_swapped_fewer_mean_b <- sim_mat_swapped_middle_mean_b
        frame_balls_fewer <- frame_balls_middle
        frame_capacities_fewer <- frame_capacities_middle
        n_swapped_fewer <- n_swapped_middle
      } else {
        out <- n_swapped_middle
        break # this section will most likely never be run
      }
    }
  } else {
    out <- max_swaps
  }
  as.integer(out)
}

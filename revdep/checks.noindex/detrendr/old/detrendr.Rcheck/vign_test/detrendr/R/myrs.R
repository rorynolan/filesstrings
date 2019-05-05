myrpois <- function(means, parallel = FALSE) {
  n_cores <- translate_parallel(parallel)
  RcppParallel::setThreadOptions(n_cores)
  on.exit(RcppParallel::setThreadOptions(RcppParallel::defaultNumThreads()))
  myrpois_(means, get_seed())
}

## every column is a frame, every row is a pixel
myrpois_frames <- function(means, frame_length, parallel = FALSE) {
  n_cores <- translate_parallel(parallel)
  RcppParallel::setThreadOptions(n_cores)
  on.exit(RcppParallel::setThreadOptions(RcppParallel::defaultNumThreads()))
  myrpois_frames_(means, frame_length, get_seed())
}

## every column is a pixel, every row is a frame
myrpois_frames_t <- function(means, frame_length, parallel = FALSE) {
  n_cores <- translate_parallel(parallel)
  RcppParallel::setThreadOptions(n_cores)
  on.exit(RcppParallel::setThreadOptions(RcppParallel::defaultNumThreads()))
  myrpois_frames_t_(means, frame_length, get_seed())
}

myrbern <- function(p, parallel = FALSE) {
  n_cores <- translate_parallel(parallel)
  RcppParallel::setThreadOptions(n_cores)
  on.exit(RcppParallel::setThreadOptions(RcppParallel::defaultNumThreads()))
  myrbernoulli_(p, get_seed())
}

#' Randomly draw balls from boxes.
#'
#' Given a number of boxes with a specified number of balls in each, randomly
#' draw a number of balls from these boxes, recording how many balls was drawn
#' from each. An empty box cannot be drawn from.
#'
#' @param n A natural number. The number of balls to draw.
#' @param balls A vector of natural numbers. The number of balls in each box to
#'   begin with.
#' @param weights A non-negative numeric vector the same length as `balls`. The
#'   relative probabilities of drawing a ball from each box. Default is each box
#'   is equally likely to be drawn from.
#'
#' @return A vector of natural numbers with the same length as `balls`. The
#'   number of balls drawn from each box.
#'
#' @seealso rtoboxes
#'
#' @examples
#' balls <- 1:10
#' rfromboxes(40, balls)
#' rfromboxes(40, balls, weights = c(rep(1, 9), 0))
#'
#' @export
rfromboxes <- function(n, balls, weights = NULL) {
  checkmate::assert_number(n, lower = 0)
  checkmate::assert_numeric(balls, lower = 0, min.len = 1, any.missing = FALSE)
  quick <- rep(FALSE, 2)
  if (is.null(weights)) quick[2] <- TRUE
  if (is.null(weights)) weights <- rep(1, length(balls))
  checkmate::assert_numeric(weights, lower = 0, any.missing = FALSE)
  if (floor(n) == 0) return(rep(0, length(balls)))
  checkmate::assert_number(n, lower = 1)
  if (n > sum(balls)) {
    stop(
      "`n` must be less than or equal to the total number of balls.", "\n",
      "    * You have `n = ", n, "` and you have ", sum(balls), " balls."
    )
  }
  if (length(weights) != length(balls)) {
    stop(
      "The length of `weights` must be equal to the length of `balls`.",
      "\n", "    * You have ", length(balls), " elements in `balls` and ",
      length(weights), " elements in `weights`."
    )
  }
  rfromboxes_(
    n = n, balls = balls, weights = weights,
    seed = get_seed(), quick = quick
  )
}

#' Randomly place balls in boxes.
#'
#' Given a number of boxes, randomly distribute _n_ balls into these boxes.
#'
#' @param n A natural number. The number of balls to put into the boxes.
#' @param boxes A natural number. The number of boxes.
#' @param weights A non-negative numeric vector. The relative probabilities of
#'   putting a ball in each box. Default is each box is equally likely.
#' @param capacities A vector of natural numbers. The capacity of each box.
#'   Default is each box has infinite capacity.
#'
#' @return A vector of natural numbers with the same length as `boxes`. The
#'   number of balls placed in each box.
#'
#' @seealso rfromboxes
#'
#' @examples
#' rtoboxes(30, 7)
#' rtoboxes(30, 7, capacities = c(rep(1, 3), rep(7, 4)))
#' rtoboxes(30, 7, capacities = c(rep(1, 3), rep(70, 4)),
#'          weights = c(rep(0.1, 6), 1))
#'
#' @export
rtoboxes <- function(n, boxes, weights = NULL, capacities = NULL) {
  checkmate::assert_number(n, lower = 0, na.ok = FALSE)
  checkmate::assert_number(boxes, lower = 0, na.ok = FALSE)
  quick <- rep(FALSE, 2)
  if (is.null(weights)) quick[1] <- TRUE
  if (is.null(capacities)) quick[2] <- TRUE
  if (is.null(weights)) weights <- rep(1, boxes)
  checkmate::assert_numeric(weights, lower = 0, any.missing = FALSE)
  if (length(weights) != boxes) {
    stop(
      "The length of `weights` must be equal to the number of boxes.",
      "\n", "    * You have ", boxes, " boxes and ",
      length(weights), " elements in `weights`."
    )
  }
  if (is.null(capacities)) {
    capacities <- rep(-1, boxes)
  } else {
    if (length(capacities) != boxes) {
      stop(
        "The length of `capacities` must be equal to the number of boxes.",
        "\n", "    * You have ", boxes, " boxes and ",
        length(capacities), " elements in `capacities`."
      )
    }
    checkmate::assert_numeric(capacities, lower = 0, any.missing = FALSE)
    if (sum(capacities) < n) {
      stop(
        "Your boxes don't have enough capacity for `n` balls.", "\n",
        "    * You have `n = ", n, "` balls but your ", boxes, " boxes ",
        "only have the capacity for a total of ", sum(capacities), " balls."
      )
    }
  }
  if (n == 0) return(rep(0, boxes))
  capacities[is.infinite(capacities)] <- -1 # infinite capacity allowed
  rtoboxes_(
    n = n, boxes = boxes, weights = weights, capacities = capacities,
    seed = get_seed(), quick = quick
  )
}

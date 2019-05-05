#' Make a raster plot of a matrix.
#'
#' Given a matrix `mat`, make a raster plot of the matrix whereby in the
#' plot, the pixel at \eqn{x = }`i`, \eqn{y = }`j` has colour based on
#' the value of `mat[i, j]` and the \eqn{x} axis points right and the
#' \eqn{y} axis points down (see 'Details').
#'
#' @param mat The matrix you wish to plot.
#' @param scale_name A string. The title of the color scale on the right of the
#'   plot.
#' @param limits This gives the user the option to set all values outside a
#'   certain range to their nearest value within that range (if `clip =
#'   TRUE`) or to `NA` (if `clip = FALSE`. For example, to set all
#'   values outside the range [1.5, 2.6) to `NA`, use `limits = c(1.5,
#'   2.6), clip = FALSE`. The colour range will cover all values within these
#'   specified limits.
#' @param ranges A numeric vector. If you want specific ranges of values to have
#'   the same color, specify these ranges via an increasing numeric vector. For
#'   example, if you want the ranges 0.5-1.2 and 1.2-3, use `ranges =
#'   c(0.5, 1.2, 3)`. If `ranges` is specified as a number (a numeric
#'   vector of length 1) `n`, this is equivalent to setting ranges to be
#'   `n` equal-length intervals within the range of the matrix, i.e. it is
#'   equivalent to setting `ranges = seq(min(mat), max(mat), length.out = n
#'   + 1)`. At most one of `ranges` and `limits` should be set. If
#'   ranges is set, the behaviour for values which are not in any of the ranges
#'   are set by the `clip` arguments as in the `limits` argument.
#' @param range_names A character vector. If your colour scale is discrete, here
#'   you can set the names which will label each range in the legend.
#' @param colours If you have set `ranges`, here you may specify which
#'   colors you wish to colour each range. It must have the same length as the
#'   number of intervals you have specified in `ranges`.  If you have not
#'   specified `ranges`, here you may specify the colours (to be passed to
#'   [ggplot2::scale_fill_gradientn()]) to create the continuous
#'   colour band. It is specified as a character vector, with the colors
#'   specified either as the values in [colors()] or as in the value
#'   of the [rgb()] function. Note that this allows the use of
#'   [grDevices::rainbow()] and friends. The default uses
#'   [viridis::viridis()].
#' @param na_colour Which colour should the `NA` pixels be? Default is
#'   black.
#' @param clip If either `limits` or `ranges` are set (one should
#'   never set both), there may be values that fall outside the specified
#'   limits/ranges. If `clip = TRUE`, values outside these limits/ranges
#'   are set to their nearest values within them, but if `clip = FALSE`,
#'   these values are set to NA. Note that setting `clip = TRUE` is
#'   equivalent to setting both `clip_low` and `clip_high` to
#'   `TRUE`.
#' @param clip_low Setting this to `TRUE` (and leaving `clip = FALSE`,
#'   `clip_high = FALSE`) will set all values falling below the specified
#'   limits/ranges to their nearest value within them, but all values falling
#'   above those limits/ranges will be set to `NA`.
#' @param clip_high Setting this to `TRUE` (and leaving `clip =
#'   FALSE`, `clip_low = FALSE`) will set all values falling above the
#'   specified limits/ranges to their nearest value within them, but all values
#'   falling below those limits/ranges will be set to `NA`.
#' @param log_trans Do you want to log-transform the colour scaling?
#' @param breaks Where do you want tick marks to appear on the legend colour
#'   scale?
#' @param include_breaks If you don't want to specify all the breaks, but you
#'   want some specific ones to be included on the legend colour scale, specify
#'   those specific ones here.
#'
#' @return In the graphics console, a raster plot (via
#'   [ggplot2::geom_raster()]) will appear with the matrix values
#'   represented as pixel colours, with a named scale bar.
#'
#' @examples
#' img <- ijtiff::read_tif(system.file('extdata', '50.tif', package = 'nandb'))
#' ijtiff::display(img[, , 1, 1])
#' matrix_raster_plot(img[, , 1, 1])
#' b <- brightness(img, def = "B", detrend = FALSE, thresh = "Huang")
#' matrix_raster_plot(b, scale_name = 'brightness')
#' matrix_raster_plot(b, scale_name = 'brightness', log_trans = TRUE)
#' matrix_raster_plot(b, scale_name = 'brightness', log_trans = TRUE,
#'                    include_breaks = 1.35)
#' matrix_raster_plot(b, scale_name = 'brightness', log_trans = TRUE,
#'                    breaks = 1:3)
#' matrix_raster_plot(b, scale_name = 'brightness',
#'                    ranges = seq(0.5, 3, length.out = 6),
#'                    range_names = paste0(1:5, 'mer'))
#' matrix_raster_plot(b, scale_name = "brightness",
#'                    ranges = seq(0.5, 3, length.out = 6),
#'                    range_names = paste0(1:5, "mer"), log_trans = TRUE)
#' matrix_raster_plot(b, scale_name = "brightness",
#'                    include_breaks = 1.25, range_names = NULL,
#'                    log_trans = FALSE)
#' matrix_raster_plot(b, scale_name = "brightness",
#'                    include_breaks = 1.25, log_trans = TRUE)
#' matrix_raster_plot(b, scale_name = "brightness",
#'                    limits = c(1, 1.25), clip = TRUE)
#' matrix_raster_plot(b, scale_name = "brightness",
#'                    include_breaks = 1.25)
#'
#' @import ggplot2
#' @export
matrix_raster_plot <- function(mat, scale_name = "scale", limits = NULL,
                               ranges = NULL, range_names = NULL,
                               colours = NULL, na_colour = "black",
                               clip = FALSE,
                               clip_low = FALSE, clip_high = FALSE,
                               log_trans = FALSE,
                               breaks = NULL, include_breaks = NULL) {
  if (is.array(mat)) {
    if ((length(dim(mat)) == 4 && filesstrings::all_equal(dim(mat)[3:4], 1)) ||
      (length(dim(mat)) == 3 && dim(mat)[3] == 1)) {
      dim(mat) %<>% {
        .[1:2]
      }
      mat %<>% as.matrix()
    }
  }
  if (is.array(mat) && length(dim(mat)) != 2) {
    custom_stop(
      "`mat` must be a matrix.",
      "
       Your `mat` has dimension
       c({glue::glue_collapse(dim(mat), sep = ', ')}) and
       is therefore not a matrix.
      "
    )
  }
  checkmate::assert_matrix(mat)
  mat %<>% t()
  plain_theme <- theme(
    axis.ticks = element_blank(),
    axis.text = element_blank(), axis.title = element_blank(),
    panel.background = element_rect(fill = "white"),
    legend.key.height = unit(1, "cm")
  )
  rownames(mat) <- NULL
  colnames(mat) <- NULL
  include_breaks <- unique(include_breaks)
  df <- reshape2::melt(mat) %>%
    dplyr::transmute(
      x = Var1,
      y = 1 + max(Var2) - Var2, value = value
    )
  if (!is.null(ranges)) {
    nr <- length(ranges) - 1
    if (is.null(colours)) {
      if (log_trans) {
        min_log_sep <- log(nr) - log(nr - 1)
        nums <- round((1 + log(seq_along(ranges))) / min_log_sep) %>% {
          .[-length(.)]
        }
        colours <- viridis::viridis(max(nums))[nums]
      } else {
        colours <- viridis::viridis(nr)
      }
    } else if (length(colours) != nr) {
      custom_stop(
        "The number of colours must match the number of ranges.",
        "You have {length(colours)} colours and {nr} ranges."
      )
    }
    ranges <- ranges %>% {
      cbind(.[-length(.)], .[-1])
    } # adjacent pairs
    ranges_typeset <- apply(ranges, 1, function(x) paste(round(
        x,
        2
      ), collapse = "-"))
    colours_ranges <- factor(df$value %>% vapply(
      which_interval, integer(1),
      ranges
    ), levels = seq_len(nrow(ranges)))
    df <- dplyr::mutate(df, colour = colours_ranges)
    if (!is.null(range_names)) {
      if (length(range_names) != nrow(ranges)) {
        custom_stop(
          "
           The number of `range_names` must be equal to the number of ranges
           (specified directly via ranges or indirectly via colours).
          ", "
           You have {length(range_names)}
          "
        )
      }
    } else {
      range_names <- levels(colours_ranges)
    }
    ggplot(df, aes(x, y, fill = colour)) + scale_fill_manual(scale_name,
      values = magrittr::set_names(colours, seq_along(colours)),
      na.value = na_colour,
      labels = magrittr::set_names(range_names, seq_along(colours))
    ) +
      geom_raster() + plain_theme + coord_fixed()
  } else {
    if (is.null(limits)) {
      if (is.null(include_breaks)) {
        also <- numeric(0)
      } else {
        also <- include_breaks
      }
      limits <- range(c(df$value, also), na.rm = TRUE)
    } else {
      if (!is.null(include_breaks)) {
        limits <- range(c(limits, include_breaks))
      }
    }
    if (is.null(colours)) colours <- viridis::viridis(99)
    if (clip) {
      clip_low <- TRUE
      clip_high <- TRUE
    }
    if (clip_low) df$value[df$value < limits[1]] <- limits[1]
    if (clip_high) df$value[df$value > limits[2]] <- limits[2]
    if (is.null(breaks)) {
      if (log_trans) {
        if (min(limits) <= 0) {
          custom_stop(
            "The `limits` range must be positive-valued.",
            "
             Your limits has a lower bound of {min(limits)},
             which is negative.
            ",
            "Therefore, a log transformation is not possible."
          )
        }
        if (is.null(include_breaks)) {
          breaks <- scale_breaks(
            include_breaks = include_breaks,
            log = log_trans
          )
        } else {
          breaks <- scale_breaks(
            include_breaks = include_breaks,
            log = log_trans
          )
        }
      } else {
        if (is.null(include_breaks)) {
          breaks <- waiver()
        } else {
          breaks <- scale_breaks(
            include_breaks = include_breaks,
            log = log_trans
          )
        }
      }
    }
    ggplot(df, aes(x, y, fill = value)) + scale_fill_gradientn(scale_name,
      limits = limits, colours = colours, na.value = na_colour,
      trans = ifelse(log_trans, "log", "identity"), breaks = breaks
    ) +
      geom_raster() + plain_theme + coord_fixed()
  }
}

#' Which interval are numbers in?
#'
#' @param numbers A numeric vector.
#' @param interval_mat A two-column matrix where the rows are
#' increasing, non-intersecting, half-open (open at the right)
#' intervals on the real line.
#'
#' @return The interval numbers that the `numbers` are in.
#'
#' @noRd
which_interval <- function(numbers, interval_mat) {
  checkmate::assert_matrix(interval_mat, ncols = 2)
  checkmate::assert_numeric(numbers)
  if (!(all(diff(as.vector(t(interval_mat))) >= 0) &&
    all(interval_mat[, 1] < interval_mat[, 2]))) {
    if (!(all(interval_mat[, 1] < interval_mat[, 2]))) {
      bad_index <- match(T, interval_mat[, 1] >= interval_mat[, 2])
      problem <- "Your interval number {bad_index} is
      c({glue::glue_collapse(interval_mat[bad_index, ], sep = ', ')}),
      which is not increasing
      because the first element is not less than the second."
    } else {
      for (bad_index in seq_len(nrow(interval_mat))) {
        if (interval_mat[bad_index, 2] > interval_mat[bad_index + 1, 1]) break
      }
      problem <- "Your interval number {bad_index} is
      c({glue::glue_collapse(interval_mat[bad_index, ], sep = ', ')}) and
      your interval number {bad_index + 1} is
      c({glue::glue_collapse(interval_mat[bad_index + 1, ], sep = ', ')}).
      These intervals intersect."
    }
    custom_stop(
      "
       `interval_mat` must be a two-column matrix where the rows are
       increasing, non-intersecting, half-open intervals on the real line.
      ", problem
    )
  }
  which_interval_(numbers, interval_mat)
}

#' Get the best *spread* of numbers in an interval.
#'
#' Say we have an interval \eqn{[a, b]}  and we want to evenly spread \eqn{n}
#' numbers in this interval, however \eqn{m} of these \eqn{n} numbers have been
#' chosen beforehand, so we have to fit the the other \eqn{n - m} in around them
#' the best we can. Our goal is to maximize the minimum distance between
#' adjacent elements.
#'
#' The idea is to, for an interval of size \eqn{s}, put in \eqn{floor(s / (n -
#' m))} numbers into each of these intervals. Then, if there any numbers left
#' over, put exactly one into each of the intervals with the biggest \eqn{(s /
#' (n - m)) - `floor`(s / (n - m))} (starting with the biggest and working
#' one's way down) until there are no numbers left to assign. The end intervals
#' need special treatment since (assuming the ends are not in the specific
#' numbers), the end intervals are open on one side (one boundary has no point
#' on it), whereas all the middle intervals are not.
#'
#' @param interval A length 2 numeric vector. The real interval over which one
#'   wants to spread the numbers.
#' @param specific A numeric vector. The specific numbers that one wants to be
#'   part of our spread.
#' @param n A number. The total number of numbers that you want to be in the
#'   spread (including the number(s) in `specific`).
#' @param log Measure the difference between adjacent elements as the difference
#'   in their log instead?
#'
#' @return A numeric vector. The chosen \eqn{n} numbers.
#'
#' @examples
#' spread_specific(c(0, 10), 1, 3)
#'
#' @noRd
spread_specific <- function(interval, specific, n, log = FALSE) {
  checkmate::assert_numeric(interval, len = 2)
  interval %<>% sort()
  checkmate::assert_numeric(specific, min.len = 1)
  checkmate::assert_int(n)
  if (log) {
    if (any(interval <= 0)) {
      bad_index <- match(T, interval <= 0)
      custom_stop(
        "If log is selected,
         the `interval` must be on the positive real line.",
        "Index {bad_index} of your `interval` is
                   {interval[bad_index]}, which is less than zero."
      )
    }
  }
  if (any(specific <= interval[1]) | any(specific >= interval[2])) {
    bad_index <- match(
      T, any(specific <= interval[1]) | any(specific >= interval[2])
    )
    custom_stop(
      "All members of `specific` must fall in `interval`.",
      "
      Element {bad_index} of your `specific` is {specific[bad_index]},
      which does not fall in your `interval`, which is
      c({glue::glue_collapse(interval, sep = ', ')}).
      "
    )
  }
  specific %<>% unique() %>% sort()
  lspec <- length(specific)
  interval_pops_init <- c(1, rep(2, lspec - 1), 1)
  intervals <- unique(c(interval[1], specific, interval[2]))
  if (log) {
    interval_lengths <- diff(log(intervals))
  } else {
    interval_lengths <- diff(intervals)
  }
  assertthat::assert_that(
    length(interval_lengths) == length(interval_pops_init)
  )
  interval_pops_final <- spread_specific_helper(
    interval_lengths,
    interval_pops_init, n - lspec
  )
  interval_pops_add <- interval_pops_final - interval_pops_init
  intervals_list <- cbind(dplyr::lag(intervals), intervals)[-1, ] %>%
    BBmisc::convertRowsToList()
  to_be_added_to <- interval_pops_add > 0
  intervals_list_to_add <- intervals_list[to_be_added_to]
  interval_pops_final_to_add <- interval_pops_final[to_be_added_to]
  if (log) {
    for (i in seq_along(intervals_list_to_add)) {
      intervals_list_to_add[[i]] <- exp(seq(log(intervals_list_to_add[[i]][1]),
        log(intervals_list_to_add[[i]][2]),
        length.out = interval_pops_final_to_add[i]
      ))
    }
  } else {
    for (i in seq_along(intervals_list_to_add)) {
      intervals_list_to_add[[i]] <- seq(intervals_list_to_add[[i]][1],
        intervals_list_to_add[[i]][2],
        length.out = interval_pops_final_to_add[i]
      )
    }
  }
  out <- sort(unique(c(specific, unlist(intervals_list_to_add))))
  diff_out <- diff(out)
  below_tol <- diff_out < 1.5e-8
  out[!below_tol]
}

scale_breaks <- function(include_breaks = NULL, log = FALSE) {
  if (is.null(include_breaks) && (!log)) return(waiver())
  function(x) {
    if (is.null(include_breaks)) {
      if (log) {
        untruncated <- exp(seq(log(min(x)), log(max(x)), length.out = 5))
      }
    } else {
      untruncated <- spread_specific(x, include_breaks, 5, log = log)
    }
    min_sep <- min(diff(untruncated))
    roundn <- log10(min_sep) %>% {
      # get a sensible amount of digits for breaks
      ifelse(. >= 1, 0, -floor(.))
    }
    brks <- ifelse(untruncated %in% include_breaks,
      untruncated, round(untruncated, roundn)
    )
    # if (brks[1] < min(x)) brks[1] <- brks[1] + 10 ^ -roundn
    # lbrks <- length(brks)
    # if (brks[lbrks] > max(x)) brks[lbrks] <- brks[lbrks] - 10 ^ -roundn
    brks
  }
}

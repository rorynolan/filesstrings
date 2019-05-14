context("Graphics")

test_that("matrix_raster_plot is ggplot", {
  img <- ijtiff::read_tif(system.file("extdata", "50.tif", package = "nandb"))
  brightness <- brightness(img, def = "B")
  expect_is(matrix_raster_plot(brightness, scale_name = "brightness"), "ggplot")
  expect_is(matrix_raster_plot(brightness,
    scale_name = "brightness",
    log_trans = TRUE
  ), "ggplot")
  expect_is(matrix_raster_plot(brightness,
    scale_name = "brightness",
    log_trans = TRUE, breaks = 1:3
  ), "ggplot")
  expect_is(matrix_raster_plot(brightness,
    scale_name = "brightness",
    ranges = seq(0.5, 3, length.out = 6), range_names = paste0(
      1:5,
      "mer"
    )
  ), "ggplot")
  expect_is(matrix_raster_plot(brightness,
    scale_name = "brightness",
    ranges = seq(0.5, 3, length.out = 6),
    range_names = paste0(1:5, "mer"), log_trans = TRUE
  ), "ggplot")
  expect_error(matrix_raster_plot(brightness,
    scale_name = "brightness",
    ranges = seq(0.5, 3, length.out = 6),
    range_names = paste0(1:59, "mer"), log_trans = TRUE
  ))
  expect_error(matrix_raster_plot(brightness,
    scale_name = "brightness",
    ranges = seq(0.5, 3, length.out = 6), colours = viridis::viridis(999),
    range_names = paste0(1:59, "mer"), log_trans = TRUE
  ))
  expect_is(matrix_raster_plot(brightness,
    scale_name = "brightness",
    ranges = seq(0.5, 3, length.out = 6), include_breaks = 1.25,
    range_names = NULL, log_trans = TRUE
  ), "ggplot")
  expect_is(matrix_raster_plot(brightness,
    scale_name = "brightness",
    ranges = seq(0.5, 3, length.out = 6), include_breaks = 1.25,
    range_names = NULL, log_trans = FALSE
  ), "ggplot")
  expect_is(matrix_raster_plot(brightness,
    scale_name = "brightness",
    limits = c(1, 1.25), clip = TRUE
  ), "ggplot")
  expect_is(matrix_raster_plot(brightness,
    scale_name = "brightness",
    limits = c(1, 1.25), clip = TRUE,
    include_breaks = 1.2
  ), "ggplot")
  expect_is(matrix_raster_plot(brightness,
    scale_name = "brightness",
    include_breaks = 1.25
  ), "ggplot")
  expect_is(matrix_raster_plot(brightness,
    scale_name = "brightness",
    include_breaks = 1.25, log_trans = TRUE
  ), "ggplot")
  expect_error(matrix_raster_plot(brightness,
    scale_name = "brightness",
    limits = c(-1, 1), log_trans = TRUE
  ))
  expect_error(
    matrix_raster_plot(img),
    paste0(
      "`mat` must be a matrix.+",
      ". Your `mat` has dimension c.50, 50, 1, 50. and is.+",
      "therefore not a matrix."
    )
  )
  expect_is(matrix_raster_plot(brightness), "ggplot")
})

test_that("`which_interval()` errors correctly", {
  set.seed(1)
  numbers <- seq(0.25, 0.75, length.out = 6)
  interval_mat <- dplyr::tribble(
    ~left, ~right,
    0, 0.1,
    0.1, 0.5,
    0.5, 0.75,
    0.75, 1
  ) %>%
    data.matrix()
  expect_equal(which_interval(numbers, interval_mat), rep(c(2, 3), each = 3))
  interval_mat[2, 2] <- 0.7
  expect_error(
    which_interval(numbers, interval_mat),
    paste0(
      "`interval_mat` must be a two-column matrix where.+",
      "rows are.+increasing, non-intersecting, half-open.+",
      "intervals on the real.+line.+",
      ". Your interval number 2 is c\\(0.1, 0.7\\) and your.+",
      "interval.+number 3 is c\\(0.5, 0.75\\).+",
      "These intervals intersect."
    )
  )
  interval_mat[2, 2] <- 0.07
  expect_error(
    which_interval(numbers, interval_mat),
    paste0(
      "`interval_mat` must be a two-column matrix where.+",
      "rows are.+increasing, non-intersecting, half-open.+",
      "intervals on the real.+line.+",
      ". Your interval number 2 is c\\(0.1, 0.07\\),.+",
      "which is not.+increasing because the first element.+",
      "is not less than.+the second."
    )
  )
})

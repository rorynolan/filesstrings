context("Utilities")

test_that("translate_parallel() works", {
  expect_equal(translate_parallel(FALSE), 1)
  max_cores <- parallel::detectCores()
  expect_equal(translate_parallel(2), min(2, max_cores))
  expect_equal(translate_parallel(TRUE), max_cores)
  expect_lt(
    translate_parallel(.Machine$integer.max),
    .Machine$integer.max
  )
})

test_that("apply_on_pillars works(", {
  a <- array(runif(3^3), dim = rep(3, 3))
  expect_equal(apply_on_pillars(a, identity), a)
})

test_that("sum_pillars() and mean_pillars() work", {
  arr3d <- system.file("extdata", "bleached.tif", package = "detrendr") %>%
    ijtiff::read_tif(msg = FALSE)
  expect_equal(mean_pillars(arr3d), sum_pillars(arr3d) / dim(arr3d)[4])
})

test_that("sum_frames() and mean_frames() work", {
  arr3d <- system.file("extdata", "bleached.tif", package = "detrendr") %>%
    ijtiff::read_tif(msg = FALSE) %>%
    {
      .[, , 1, ]
    }
  expect_equal(mean_frames(arr3d), sum_frames(arr3d) / prod(dim(arr3d)[1:2]))
  arr3d[1, 1, ] <- NA
  expect_equal(
    mean_frames(arr3d, na_rm = TRUE),
    sum_frames(arr3d, na_rm = TRUE) / (prod(dim(arr3d)[1:2]) - 1)
  )
})

test_that("brightness_cols_given_mean() works", {
  mat <- system.file("extdata", "bleached.tif", package = "detrendr") %>%
    ijtiff::read_tif(msg = FALSE) %>%
    {
      .[, , 1, 1]
    }
  col_means <- mean_cols(mat)
  expect_equal(
    brightness_cols_given_mean(mat, col_means),
    brightness_cols(mat)
  )
})

test_that("var_cols_given_mean() works", {
  skip_if_not_installed("matrixStats")
  mat <- system.file("extdata", "bleached.tif", package = "detrendr") %>%
    ijtiff::read_tif(msg = FALSE) %>%
    {
      .[, , 1, 1]
    }
  col_means <- mean_cols(mat)
  expect_equal(
    var_cols_given_mean(mat, col_means),
    matrixStats::colVars(mat)
  )
})

test_that("var_rows_given_mean() works", {
  skip_if_not_installed("matrixStats")
  mat <- system.file("extdata", "bleached.tif", package = "detrendr") %>%
    ijtiff::read_tif(msg = FALSE) %>%
    {
      .[, , 1, 1]
    }
  row_means <- mean_rows(mat)
  expect_equal(
    var_rows_given_mean(mat, row_means),
    matrixStats::rowVars(mat)
  )
})

test_that("boxcar smoothing edge case works", {
  mm <- matrix(1:4, nrow = 2)
  expect_equal(boxcar_smooth_rows(mm, 9), matrix(c(2, 3, 2, 3), nrow = 2))
  expect_equal(weighted_smooth(1:5, rep(1, 101)), rep(3, 5))
})

test_that("rboxes derivatives error correctly", {
  arr3d <- system.file("extdata", "bleached.tif", package = "detrendr") %>%
    ijtiff::read_tif(msg = FALSE) %>%
    {
      .[, , 1, ]
    }
  if (get_os() == "mac") {
    expect_error(px_take_arr3d(arr3d, 1:2, 3))
  } else {
    expect_error(
      px_take_arr3d(arr3d, 1:2, 3),
      paste(
        "The length of `frames_losing` must be the same as",
        "the number of frames in `arr3d`."
      )
    )
  }
  mat <- arr3d[, , 1]
  if (get_os() == "mac") {
    expect_error(px_take_mat(mat, mat, 1:2, 3))
  } else {
    expect_error(
      px_take_mat(mat, mat, 1:2, 3),
      paste(
        "The length of `frames_losing` must be the same as",
        "the number of frames in `arr3d`."
      )
    )
  }
})

test_that("`myarray2vec()` works", {
  skip_if_not_installed("arrayhelpers")
  mat <- matrix(c(9, 13, 2, 6,
                  7, 5, 15, 12,
                  1, 10, 3, 8,
                  4, 11, 16, 14),
                nrow = 4, byrow = TRUE)
  d <- rep(99, 4)
  expect_equal(myarray2vec(mat, d), arrayhelpers::array2vec(mat, d))
  expect_error(
    myarray2vec(mat, rep(2, 2)),
    paste0(
      "Number of columns in `iarr` and number of dimensions\\s?",
      "must be.+the same.+There are 4 columns in `iarr` and\\s?",
      "you have specified 2.+dimensions."
    )
  )
  set.seed(2)
  expect_error(
    myarray2vec(matrix(c(1, 3, 1, 4,
                         4, 1, 1, 2,
                         3, 2, 4, 5,
                         1, 5, 5, 3),
                       nrow = 4, byrow = TRUE),
      dim = rep(4, 4)
    ),
    paste0(
      "You are requesting an array index outside the\\s?",
      "dimension of.+your array.+Row 3 of `iarr` is `c\\(3, 2,\\s?",
      "4, 5\\)` and `dim` is `c\\(4, 4,.+4, 4\\)`, so element 4\\s?",
      "of that row, 5, is too big as it is.+bigger than\\s?",
      "element 4 of `dim`, 4."
    )
  )
  vec <- sample.int(max(d), length(d))
  expect_equal(myarray2vec(vec, d), myarray2vec(matrix(vec, nrow = 1), d))
})

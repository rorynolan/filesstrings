test_that("brightness_pillars works", {
  d <- 2:4
  aaa <- array(seq_len(prod(d)), dim = d)
  expect_equal(
    ijtiff::ijtiff_img(brightness_pillars(aaa)),
    ijtiff::ijtiff_img(apply_on_pillars(aaa, brightness_vec))
  )
})

test_that("pillar-stats works", {
  d <- 2:4
  aaa <- array(seq_len(prod(d)), dim = d)
  expect_equal(
    ijtiff::ijtiff_img(mean_pillars(aaa)),
    ijtiff::ijtiff_img(apply_on_pillars(aaa, mean))
  )
  expect_equal(
    ijtiff::ijtiff_img(median_pillars(aaa)),
    ijtiff::ijtiff_img(apply_on_pillars(aaa, median))
  )
  expect_equal(
    ijtiff::ijtiff_img(var_pillars(aaa)),
    ijtiff::ijtiff_img(apply_on_pillars(aaa, var))
  )
  d <- 2:5
  aaaa <- array(sample.int(prod(d)), dim = d)
  skip_if_not_installed("abind")
  ans <- purrr::map(seq_len(dim(aaaa)[3]), ~ mean_pillars(aaaa[, , ., ])) %>%
    purrr::reduce(~ abind::abind(.x, .y, along = 3)) %>%
    structure(dimnames = NULL)
  expect_equal(
    ijtiff::ijtiff_img(mean_pillars(aaaa)),
    ijtiff::ijtiff_img(ans)
  )
  ans <- purrr::map(seq_len(dim(aaaa)[3]), ~ median_pillars(aaaa[, , ., ])) %>%
    purrr::reduce(~ abind::abind(.x, .y, along = 3)) %>%
    structure(dimnames = NULL)
  expect_equal(
    ijtiff::ijtiff_img(median_pillars(aaaa)),
    ijtiff::ijtiff_img(ans)
  )
  ans <- purrr::map(seq_len(dim(aaaa)[3]), ~ var_pillars(aaaa[, , ., ])) %>%
    purrr::reduce(~ abind::abind(.x, .y, along = 3)) %>%
    structure(dimnames = NULL)
  expect_equal(
    ijtiff::ijtiff_img(var_pillars(aaaa)),
    ijtiff::ijtiff_img(ans)
  )
  ans <- purrr::map(
    seq_len(dim(aaaa)[3]),
    ~ brightness_pillars(aaaa[, , ., ])
  ) %>%
    purrr::reduce(~ abind::abind(.x, .y, along = 3)) %>%
    structure(dimnames = NULL)
  expect_equal(
    ijtiff::ijtiff_img(brightness_pillars(aaaa)),
    ijtiff::ijtiff_img(ans)
  )
})

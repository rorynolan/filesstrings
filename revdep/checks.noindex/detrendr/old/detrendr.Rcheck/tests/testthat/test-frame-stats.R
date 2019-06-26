test_that("frame stats edge cases work correctly", {
  expect_equal(
    sum_frames_na_omit_(array(3L, dim = rep(3, 3))),
    sum_frames_na_omit_(array(3, dim = rep(3, 3)))
  )
  expect_equal(
    mean_frames_na_omit_(array(3L, dim = rep(3, 3))),
    mean_frames_na_omit_(array(3, dim = rep(3, 3)))
  )
  expect_equal(
    sum_frames_na_omit_(array(NA_integer_, dim = rep(3, 3))),
    rep(NA_real_, 3)
  )
  expect_equal(
    sum_frames_na_omit_(array(NA_real_, dim = rep(3, 3))),
    rep(NA_real_, 3)
  )
  expect_equal(
    mean_frames_na_omit_(array(NA_integer_, dim = rep(3, 3))),
    rep(NA_real_, 3)
  )
  expect_equal(
    mean_frames_na_omit_(array(NA_real_, dim = rep(3, 3))),
    rep(NA_real_, 3)
  )
})

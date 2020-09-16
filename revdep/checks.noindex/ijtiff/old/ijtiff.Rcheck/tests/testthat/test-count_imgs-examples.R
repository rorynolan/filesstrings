test_that("`count_frames()` works", {
  expect_equal(
    count_frames(system.file("img", "Rlogo.tif", package = "ijtiff")),
    structure(1, n_dirs = 1)
  )
  expect_equal(
    frames_count(test_path("testthat-figs", "2ch_ij.tif")),
    structure(5, n_dirs = 10)
  )
})

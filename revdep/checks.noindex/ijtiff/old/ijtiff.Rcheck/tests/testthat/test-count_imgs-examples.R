test_that("`count_imgs()` works", {
  expect_equal(
    count_frames(system.file("img", "Rlogo.tif", package = "ijtiff")),
    structure(1, n_dirs = 1)
  )
  expect_equal(
    count_frames(system.file("img", "2ch_ij.tif", package = "ijtiff")),
    structure(5, n_dirs = 10)
  )
})

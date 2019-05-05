context("`count_imgs()`")

test_that("`count_imgs()` works", {
  expect_equal(
    count_imgs(system.file("img", "Rlogo.tif", package = "ijtiff")),
    1
  )
  expect_equal(
    count_imgs(system.file("img", "2ch_ij.tif", package = "ijtiff")),
    10
  )
})

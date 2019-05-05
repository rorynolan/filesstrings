context("`as_EBImage()`")

test_that("`as_EBImage()` works", {
  skip_if_not_installed("EBImage")
  img <- read_tif(system.file("img", "Rlogo.tif", package = "ijtiff"))
  expect_equal(dim(img), c(76, 100, 4, 1))
  ebimg <- as_EBImage(img)
  expect_equal(dim(ebimg), c(100, 76, 4, 1))
  expect_is(ebimg, "Image")
  img <- read_tif(system.file("img", "2ch_ij.tif", package = "ijtiff"))
  expect_equal(dim(img), c(15, 6, 2, 5))
  ebimg <- as_EBImage(img)
  expect_equal(dim(ebimg), c(6, 15, 2, 5))
  arr <- array(2^9, dim = rep(2, 2))
  expect_equal(as_EBImage(arr), as_EBImage(ijtiff_img(arr)))
  expect_error(
    as_EBImage(arr, force = FALSE),
    paste0(
      "This function expects the input `img` to be of class.+",
      ".ijtiff_img., however the `img` you have supplied is not.+",
      ". To force your array through this function, use `force =.+",
      "TRUE`, but take care to check that the result is what.+",
      "you.d like it to be."
    )
  )
  expect_equal(as_EBImage(as_EBImage(arr)), as_EBImage(arr))
  expect_equal(EBImage::colorMode(as_EBImage(arr)), 0)
  arr <- array(2^22, dim = rep(4, 4))
  expect_equal(EBImage::colorMode(as_EBImage(arr)), 2)
  expect_true(mean(EBImage::imageData(as_EBImage(arr))) <
    mean(EBImage::imageData(as_EBImage(array(2^33, dim = rep(4, 4))))))
  expect_equal(ebimg_install_msg(), paste0(
    "  * To install `EBImage`:", "\n",
    "    - Install `BiocManager` with `install.packages(\"BiocManager\")`.\n",
    "    - Then run `BiocManager::install(\"EBImage\")`."
  ))
})

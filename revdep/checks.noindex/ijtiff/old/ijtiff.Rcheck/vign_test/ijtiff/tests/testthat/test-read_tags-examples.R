context("`read_tags()`")

test_that("`read_tags()` works", {
  path <- system.file("img", "Rlogo.tif", package = "ijtiff")
  tags <- read_tags(path)
  ans <- list(img1 = list(
    width = 100,
    length = 76,
    bits_per_sample = 8,
    samples_per_pixel = 4,
    sample_format = "uint",
    planar_config = "contiguous",
    rows_per_strip = 76,
    compression = "LZW",
    x_resolution = 299.99,
    y_resolution = 299.99,
    resolution_unit = "inch",
    orientation = "top_left",
    color_space = "RGB"
  ))
  expect_equal(tags, ans, tolerance = 0.001)
  path <- system.file("img", "2ch_ij.tif", package = "ijtiff")
  expect_equal(read_tags(path)[c(2, 4)], read_tags(path, all = c(2, 4)))
  expect_error(
    read_tags(path, all = c(11, 12)),
    paste0(
      "Cannot access image 12.+",
      ". You have tried to access information from image 12.+",
      "but.+there are only 10 images in total."
    )
  )
  expect_equal(read_tags(path, all = 1), read_tags(path, all = FALSE))
})

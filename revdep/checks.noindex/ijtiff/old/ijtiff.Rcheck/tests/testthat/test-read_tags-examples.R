test_that("`read_tags()` works", {
  path <- system.file("img", "Rlogo.tif", package = "ijtiff")
  tags <- read_tags(path)
  ans <- list(frame1 = list(
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
  expect_equal(
    read_tags(path, "all")[c(2, 4)],
    read_tags(path, frames = c(2, 4))
  )
  expect_error(
    read_tags(path, frames = c(11, 12)),
    paste(
      " You have requested frame number 12 but there are",
      "only 5 frames in total. "
    )
  )
  expect_true(filesstrings::all_equal(read_tags(path, "all")))
  expect_equal(
    read_tags(path),
    list(
      frame1 = list(
        width = 6L, length = 15L, bits_per_sample = 8L,
        samples_per_pixel = 1L, sample_format = "uint",
        planar_config = "contiguous",
        rows_per_strip = 15L, compression = "none",
        description = paste0(
          "ImageJ=1.51s\nimages=10\n",
          "channels=2\nframes=5\n",
          "hyperstack=true\nmode=composite\n",
          "loop=false\n"
        ),
        color_space = "black is zero"
      )
    )
  )
  for (i in seq_along(read_tags(path, "all"))) {
    expect_equal(read_tags(path)[[1]], read_tags(path, i)[[1]])
  }
})

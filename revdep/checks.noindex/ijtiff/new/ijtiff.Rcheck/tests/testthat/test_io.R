test_that("Package 2-channel example I/O works", {
  set.seed(1)
  context("Package 2-channel example I/O")
  img <- read_tif(system.file("img", "2ch_ij.tif", package = "ijtiff"))
  expect_equal(dim(img), c(15, 6, 2, 5))
  img <- read_tif(system.file("img", "Rlogo-banana-red_green.tif",
    package = "ijtiff"
  ))
  expect_equal(dim(img), c(155, 200, 2, 3))
  img <- read_tif(system.file("img", "Rlogo-banana-1-2.tif",
    package = "ijtiff"
  ))
  expect_equal(dim(img), c(155, 200, 3, 2))
  img <- read_tif(system.file("img", "Rlogo-banana-red_green_blue.tif",
    package = "ijtiff"
  ))
  expect_equal(dim(img), c(155, 200, 3, 2))
  img <- read_tif(system.file("img", "Rlogo-banana-red.tif",
    package = "ijtiff"
  ))
  expect_equal(dim(img), c(155, 200, 1, 2))
  context("8-bit unsigned integer TIFF I/O")
  v2345 <- 2:5
  a2345 <- array(sample.int(prod(v2345)), dim = v2345)
  tmptif <- tempfile(fileext = ".tif") %>%
    stringr::str_replace_all(stringr::coll("\\"), "/")
  write_tif(a2345, tmptif)
  in_tif <- read_tif(tmptif)
  expect_equal(dim(in_tif), v2345)
  expect_equal(as.vector(in_tif), as.vector(a2345), check.attributes = FALSE)
})

test_that("Package RGB I/O works", {
  context("Package RGB I/O")
  img <- read_tif(system.file("img", "Rlogo.tif", package = "ijtiff"))
  expect_equal(dim(img), c(76, 100, 4, 1))
})

test_that("8-bit unsigned integer TIFF I/O works", {
  set.seed(2)
  context("8-bit unsigned integer TIFF I/O")
  v2345 <- 2:5
  a2345 <- array(sample.int(prod(v2345)), dim = v2345)
  tmptif <- tempfile(fileext = ".tif") %>%
    stringr::str_replace_all(stringr::coll("\\"), "/")
  write_tif(a2345, tmptif)
  in_tif <- read_tif(tmptif)
  expect_equal(dim(in_tif), v2345)
  expect_equal(as.vector(in_tif), as.vector(a2345), check.attributes = FALSE)
})

test_that("16-bit unsigned integer TIFF I/O works", {
  set.seed(3)
  context("16-bit unsigned integer TIFF I/O")
  v6789 <- 6:9
  a6789 <- array(sample.int(prod(v6789)), dim = v6789)
  tmptif <- tempfile(fileext = ".tif") %>%
    stringr::str_replace_all(stringr::coll("\\"), "/")
  write_tif(a6789, tmptif)
  in_tif <- read_tif(tmptif)
  expect_equal(dim(in_tif), v6789)
  expect_equal(as.vector(in_tif), as.vector(a6789), check.attributes = FALSE)
})

test_that("32-bit unsigned integer TIFF I/O works", {
  set.seed(4)
  context("32-bit unsigned integer TIFF I/O")
  v1m <- c(20, 50, 10, 100)
  a1m <- array(sample.int(2^32 - 1, prod(v1m)), dim = v1m)
  tmptif <- tempfile(fileext = ".tif") %>%
    stringr::str_replace_all(stringr::coll("\\"), "/")
  write_tif(a1m, tmptif)
  in_tif <- read_tif(tmptif)
  expect_equal(dim(in_tif), v1m)
  expect_equal(as.vector(in_tif), as.vector(a1m), check.attributes = FALSE)
})

test_that("Float (real-numbered) TIFF I/O works", {
  set.seed(5)
  cwd <- getwd()
  on.exit(setwd(cwd))
  setwd(tempdir())
  context("8-bit unsigned integer TIFF I/O")
  v2345 <- 2:5
  a2345 <- array(sample.int(prod(v2345)), dim = v2345) + 0.5
  tmptif <- tempfile(fileext = ".tif") %>%
    stringr::str_replace_all(stringr::coll("\\"), "/")
  write_tif(a2345, paste0(tmptif, "f"))
  in_tif <- read_tif(tmptif)
  expect_equal(dim(in_tif), v2345)
  expect_equal(as.vector(in_tif), as.vector(a2345), check.attributes = FALSE)
  a2345[9] <- NA
  expect_error(
    write_tif(a2345, tmptif),
    "To enable overwriting, use `overwrite = TRUE`"
  )
  write_tif(a2345, tmptif, overwrite = TRUE)
  in_tif <- read_tif(tmptif)
  expect_equal(dim(in_tif), v2345)
  expect_equal(as.vector(in_tif), as.vector(a2345), check.attributes = FALSE)
})

test_that("Negative-numbered TIFF I/O works", {
  context("Negative-numbered TIFF I/O")
  v2345 <- 2:5
  a2345 <- array(sample.int(prod(v2345)), dim = v2345)
  a2345[1] <- -1
  tmptif <- tempfile(fileext = ".tif") %>%
    stringr::str_replace_all(stringr::coll("\\"), "/")
  write_tif(a2345, tmptif)
  in_tif <- read_tif(tmptif)
  expect_equal(dim(in_tif), v2345)
  expect_equal(as.vector(in_tif), as.vector(a2345), check.attributes = FALSE)
  expect_equal(attr(in_tif, "sample_format"), "float")
})

test_that("List returning works", {
  skip_if_not_installed("tiff")
  context("List returning")
  img1 <- matrix(0.5, nrow = 2, ncol = 2)
  img2 <- matrix(0.7, nrow = 3, ncol = 7)
  weird_list_img <- list(img1, img2)
  tmptif <- tempfile(fileext = ".tif") %>%
    stringr::str_replace_all(stringr::coll("\\"), "/")
  expect_equal(tiff::writeTIFF(weird_list_img, tmptif), 2)
  expect_error(read_tif(tmptif), "tried to return a list")
  expect_warning(
    read_tif(tmptif, list_safety = "warn"),
    "returning a list"
  )
  in_weird <- read_tif(tmptif, list_safety = "n")
  expect_equal(in_weird,
    purrr::map(weird_list_img, ~floor(. * (2^8 - 1))),
    check.attributes = FALSE
  ) # writing causes truncation
})

test_that("TIFFErrorHandler_ works", {
  context("TIFFErrorHandler_")
  tmptxt <- tempfile(fileext = ".txt") %>%
    stringr::str_replace_all(stringr::coll("\\"), "/")
  writeLines(c("a", "b"), tmptxt)
  expect_error(suppressWarnings(read_tif(tmptxt)), "Cannot read TIFF header")
})

test_that("write_tif() errors correctly", {
  context("write_tif() exceptions")
  aaaa <- array(0, dim = rep(4, 4))
  expect_error(
    write_tif(aaaa, "a", bits_per_sample = "abc"),
    paste0(
      "If `bits_per_sample` is a string, then 'auto' is the only.?",
      "allowable value\\..?",
      "    \\* You have used 'abc'\\..?"
    )
  )
  expect_error(
    write_tif(aaaa, "a", bits_per_sample = 12),
    paste0(
      "If specifying `bits_per_sample`, it must be one of 8, 16 or.?",
      "32\\..?",
      "    \\* You have used `bits_per_sample = 12`\\..?"
    )
  )
  aaaa[1] <- -2 * float_max()
  expect_error(
    write_tif(aaaa, "a"),
    paste0(
      "The lowest allowable negative value in `img` is.?",
      "-3\\.40282346638529e\\+38\\..?",
      "    \\* The lowest value in your `img` is -6\\.80564693277058e\\+38\\..?",
      "    \\* The `write_txt_img\\(\\)` function allows you to write.?",
      "      images without restriction on the values therein\\..?"
    )
  )
  aaaa[1] <- -1
  aaaa[2] <- 2 * float_max()
  expect_error(
    write_tif(aaaa, "a"),
    paste0(
      "If `img` has negative values \\(which the input `img` does\\),.?",
      "then the maximum allowed positive value is.?",
      "3\\.40282346638529e\\+38\\..?",
      "    \\* The largest value in your `img` is 6.+e\\+38\\..?",
      "    \\* The `write_txt_img\\(\\)` function allows you to write.?",
      "      images without restriction on the values therein\\..?"
    )
  )
  aaaa[2] <- 1
  aaaa[1] <- 0.5
  expect_error(
    write_tif(aaaa, "a", bits_per_sample = 16),
    paste0(
      "Your image needs to be written as floating point numbers \\(not.?",
      "integers\\)\\. For this, it is necessary to have 32 bits per.?",
      "sample\\..?",
      "    \\* You have selected 16 bits per sample\\..?"
    )
  )
  aaaa[1] <- 2^33
  expect_error(
    write_tif(aaaa, "a", bits_per_sample = 16),
    paste0(
      "The maximum value in 'img' is 8589934592 which is greater than.?",
      "2 \\^ 32 - 1 and therefore too high to be written to a TIFF.?",
      "file\\..?",
      "    \\* The `write_txt_img\\(\\)` function allows you to write.?",
      "      images without restriction on the values therein\\..?"
    )
  )
  aaaa[1] <- 2^20
  expect_error(
    write_tif(aaaa, "a", bits_per_sample = 16),
    paste0(
      "You are trying to write a 16-bit image, however the maximum.?",
      "element in `img` is 1048576, which is too big\\..?",
      "    \\* The largest allowable value in a 16-bit image is 65535\\..?",
      "    \\* To write your `img` to a TIFF file, you need at least 32.?",
      "      bits per sample\\..?"
    )
  )
  expect_error(
    read_tif(system.file("img", "bad_ij1.tif", package = "ijtiff")),
    paste0("The ImageJ-written image you're trying to read says in its ",
           "TIFFTAG_DESCRIPTION that it has 13 images of 5 slices of 2 ",
           "channels. However, with 5 slices of 2 channels, one would expect ",
           "there to be 5x2=10 images. This discrepancy means that the ",
           "'ijtiff' package can't read your image correctly. ",
           "One possible source of this kind of error is that your image is ",
           "temporal and volumetric. 'ijtiff' can handle either time-based or ",
           "volumetric stacks, but not both.")
  )
  expect_error(
    read_tif(system.file("img", "bad_ij2.tif", package = "ijtiff")),
    paste0("The ImageJ-written image you're trying to read says it has ",
           "8 frames AND 5 slices. To be read by the 'ijtiff' package, ",
           "the number of slices OR the number of frames should be specified ",
           "in the description tiff tag ",
           "\\(and they're interpreted as the same thing\\), but not both.")
  )
})

context("Text I/O")
test_that("text-image-io works", {
  mm <- matrix(1:60, nrow = 4)
  dim(mm) %<>% c(1, 1)
  tmpfl <- tempfile() %>%
    stringr::str_replace_all(stringr::coll("\\"), "/")
  write_txt_img(mm, tmpfl)
  tmpfl_txt <- filesstrings::give_ext(tmpfl, "txt")
  expect_true(file.exists(tmpfl_txt))
  expect_equal(as.vector(mm), unlist(read_txt_img(tmpfl_txt)),
    check.attributes = FALSE
  )
  file.remove(tmpfl_txt)
  skip_if_not_installed("abind")
  mmm <- abind::abind(mm, mm, along = 3)
  write_txt_img(mmm, tmpfl, rds = TRUE)
  expect_equal(readRDS(filesstrings::give_ext(tmpfl, "rds")), ijtiff_img(mmm))
  tmpfl_txts <- paste0(tmpfl, "_ch", 1:2, ".txt")
  expect_equal(dir(filesstrings::str_before_last(tmpfl, "/"),
    pattern = paste0(
      filesstrings::str_after_last(tmpfl, "/"),
      ".+txt$"
    )
  ),
  filesstrings::str_after_last(tmpfl_txts, "/"),
  check.names = FALSE, check.attributes = FALSE
  )
  expect_equal(unlist(lapply(tmpfl_txts, read_txt_img)), as.vector(mmm),
    check.attributes = FALSE
  )
  file.remove(tmpfl_txts)
  mmmm <- abind::abind(mmm, mmm, along = 4)
  write_txt_img(mmmm, tmpfl)
  tmpfl_txts <- paste0(tmpfl, c(
    "_ch1_frame1",
    "_ch1_frame2",
    "_ch2_frame1",
    "_ch2_frame2"
  ), ".txt")
  expect_equal(dir(filesstrings::str_before_last(tmpfl, "/"),
    pattern = paste0(
      filesstrings::str_after_last(tmpfl, "/"),
      ".+txt$"
    )
  ),
  filesstrings::str_after_last(tmpfl_txts, "/"),
  check.names = FALSE, check.attributes = FALSE
  )
  expect_equal(unlist(lapply(tmpfl_txts, read_txt_img)), as.vector(mmmm),
    check.attributes = FALSE
  )
  bad_txt_img <- dplyr::tribble(
    ~col1, ~col2,
    1, 5,
    8, "y"
  )
  tmpfl <- tempfile(fileext = ".txt")
  readr::write_tsv(bad_txt_img, tmpfl, col_names = FALSE)
  expect_error(
    read_txt_img(tmpfl),
    paste0(
      "`path` must be the path to a text file which is.+",
      "an array of.+numbers.",
      "* Column 2 of the text file at your `path`.+",
      "is not numeric."
    )
  )
})

test_that("Package 2-channel example I/O works", {
  set.seed(1)
  img0 <- read_tif(test_path("testthat-figs", "2ch_ij.tif"))
  expect_equal(dim(img0), c(15, 6, 2, 5))
  img1 <- read_tif(system.file("img", "Rlogo-banana-red_green.tif",
    package = "ijtiff"
  ))
  expect_equal(dim(img1), c(155, 200, 2, 2))
  img2 <- read_tif(test_path("testthat-figs", "Rlogo-banana-1-2.tif"))
  expect_equal(dim(img2), c(155, 200, 3, 2))
  img3 <- read_tif(
    test_path("testthat-figs", "Rlogo-banana-red_green_blue.tif")
  )
  expect_equal(dim(img3), c(155, 200, 3, 2))
  img4 <- read_tif(test_path("testthat-figs", "Rlogo-banana-red.tif"))
  expect_equal(dim(img4), c(155, 200, 1, 2))
  expect_equal(img3[, , 1, 1], img4[, , 1, 1])
  v22 <- c(2, 2, 1, 1)
  a22 <- array(seq_len(prod(v22)), dim = v22)
  tmptif <- tempfile(fileext = ".tif") %>%
    stringr::str_replace_all(stringr::coll("\\"), "/")
  write_tif(a22, tmptif)
  in_tif <- read_tif(tmptif)
  expect_equal(dim(in_tif), v22)
  expect_equal(as.vector(in_tif), as.vector(a22), check.attributes = FALSE)
  v2345 <- 2:5
  a2345 <- array(seq_len(prod(v2345)), dim = v2345)
  write_tif(a2345, tmptif, overwrite = TRUE)
  in_tif <- read_tif(tmptif)
  expect_equal(dim(in_tif), v2345)
  expect_equal(as.vector(in_tif), as.vector(a2345), check.attributes = FALSE)
  expect_equal(
    as.vector(read_tif(tmptif, frames = c(3, 5))),
    as.vector(a2345[, , , c(3, 5)])
  )
  v22 <- c(2, 2, 1, 1)
  a22 <- array(sample.int(prod(v22)), dim = v22)
  tmptif <- tempfile(fileext = ".tif") %>%
    stringr::str_replace_all(stringr::coll("\\"), "/")
  write_tif(a22, tmptif)
  in_tif <- read_tif(tmptif)
  expect_equal(dim(in_tif), v22)
  expect_equal(as.vector(in_tif), as.vector(a22), check.attributes = FALSE)
  v2345 <- 2:5
  a2345 <- array(sample.int(prod(v2345)), dim = v2345)
  write_tif(a2345, tmptif, overwrite = TRUE)
  in_tif <- read_tif(tmptif)
  expect_equal(dim(in_tif), v2345)
  expect_equal(as.vector(in_tif), as.vector(a2345), check.attributes = FALSE)
})

test_that("Package RGB I/O works", {
  img <- read_tif(system.file("img", "Rlogo.tif", package = "ijtiff"))
  expect_equal(dim(img), c(76, 100, 4, 1))
})

test_that("8-bit unsigned integer TIFF I/O works", {
  set.seed(2)
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
  v6789 <- 6:9
  a6789 <- array(sample.int(prod(v6789)), dim = v6789)
  tmptif <- tempfile(fileext = ".tif") %>%
    stringr::str_replace_all(stringr::coll("\\"), "/")
  tif_write(a6789, tmptif)
  in_tif <- read_tif(tmptif)
  expect_equal(dim(in_tif), v6789)
  expect_equal(as.vector(in_tif), as.vector(a6789), check.attributes = FALSE)
})

test_that("32-bit unsigned integer TIFF I/O works", {
  set.seed(4)
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
    purrr::map(weird_list_img, ~ floor(. * (2^8 - 1))),
    check.attributes = FALSE
  ) # writing causes truncation
})

test_that("TIFFErrorHandler_ works", {
  tmptxt <- tempfile(fileext = ".txt") %>%
    stringr::str_replace_all(stringr::coll("\\"), "/")
  writeLines(c("a", "b"), tmptxt)
  expect_error(suppressWarnings(tif_read(tmptxt)), "Cannot read TIFF header")
})

test_that("write_tif() errors correctly", {
  aaaa <- array(0, dim = rep(4, 4))
  expect_error(tif_write(aaaa, "path/"), "path.+cannot end with.+/")
  expect_error(
    write_tif(aaaa, "a", bits_per_sample = "abc"),
    paste0(
      " If `bits_per_sample` is a string, then 'auto' is ",
      "the only allowable value. \n    * You have used 'ab",
      "c'."
    ),
    fixed = TRUE
  )
  expect_error(
    write_tif(aaaa, "a", bits_per_sample = 12),
    paste0(
      "If specifying `bits_per_sample`, it must be one of 8, 16 or.?",
      "32\\..?",
      "    \\* You have used `bits_per_sample = 12`\\..?"
    )
  )
  aaaa[1] <- -2 * .Call("float_max_C", PACKAGE = "ijtiff")
  expect_error(
    write_tif(aaaa, "a"),
    paste(
      "The lowest allowable negative value in `img` is",
      "-3.40282346638529e+38.\n    * The lowest value in",
      "your `img` is -6.80564693277058e+38.\n    *  The",
      "`write_txt_img()` function allows you to write",
      "images without restriction on the values therein.",
      "Maybe you should try that?"
    ),
    fixed = TRUE
  )
  aaaa[1] <- -1
  aaaa[2] <- 2 * .Call("float_max_C", PACKAGE = "ijtiff")
  expect_error(
    write_tif(aaaa, "a"),
    paste(
      " If `img` has negative values (which the input",
      "`img` does), then the maximum allowed positive",
      "value is 3.40282346638529e+38. \n    * The largest",
      "value in your `img` is 6.80564693277058e+38.\n   ",
      "*  The `write_txt_img()` function allows you to",
      "write images without restriction on the values",
      "therein. Maybe you should try that?"
    ),
    fixed = TRUE
  )
  aaaa[2] <- 1
  aaaa[1] <- 0.5
  expect_error(
    write_tif(aaaa, "a", bits_per_sample = 16),
    paste(
      " Your image needs to be written as floating point",
      "numbers (not integers). For this, it is necessary",
      "to have 32 bits per sample. \n    * You have",
      "selected 16 bits per sample."
    ),
    fixed = TRUE
  )
  aaaa[1] <- 2^33
  expect_error(
    write_tif(aaaa, "a", bits_per_sample = 16),
    paste(
      " The maximum value in 'img' is 8589934592 which",
      "is greater than 2 ^ 32 - 1 and therefore too high",
      "to be written to a TIFF file. \n    * The",
      "`write_txt_img()` function allows you to write",
      "images without restriction on the values therein.",
      "Maybe you should try that?"
    ),
    fixed = TRUE
  )
  aaaa[1] <- 2^20
  expect_error(
    write_tif(aaaa, "a", bits_per_sample = 16),
    paste0(
      " You are trying to write a 16-bit image, however t",
      "he maximum element in `img` is 1048576, which is t",
      "oo big. \n    *  The largest allowable value in a 1",
      "6-bit image is 65535.\n    *  To write your `img` t",
      "o a TIFF file, you need at least 32 bits per sampl",
      "e."
    ),
    fixed = TRUE
  )
  expect_error(
    read_tif(test_path("testthat-figs", "bad_ij1.tif")),
    paste(
      " The ImageJ-written image you're trying to read",
      "says in its TIFFTAG_DESCRIPTION that it has 13",
      "images of 5 slices of 2 channels. However, with 5",
      "slices of 2 channels, one would expect there to",
      "be 5 x 2 = 10 images. \n    * This discrepancy",
      "means that the `ijtiff` package can't read your",
      "image correctly.\n    * One possible source of",
      "this kind of error is that your image is temporal",
      "and volumetric. `ijtiff` can handle either",
      "time-based or volumetric stacks, but not both."
    ),
    fixed = TRUE
  )
  expect_error(
    read_tif(test_path("testthat-figs", "bad_ij2.tif")),
    paste(
      " The ImageJ-written image you're trying to read",
      "says it has 8 frames AND 5 slices. \n    * To be",
      "read by the `ijtiff` package, the number of",
      "slices OR the number of frames should be",
      "specified in the TIFFTAG_DESCRIPTION and they're",
      "interpreted as the same thing. It does not make",
      "sense for them to be different numbers."
    ),
    fixed = TRUE
  )
})

test_that("text-image-io works", {
  mm <- matrix(1:60, nrow = 4)
  dim(mm) %<>% c(1, 1)
  tmpfl <- tempfile() %>%
    stringr::str_replace_all(stringr::coll("\\"), "/")
  txt_img_write(mm, tmpfl)
  tmpfl_txt <- filesstrings::give_ext(tmpfl, "txt")
  expect_true(file.exists(tmpfl_txt))
  expect_equal(as.vector(mm), unlist(txt_img_read(tmpfl_txt)),
    check.attributes = FALSE
  )
  file.remove(tmpfl_txt)
  skip_if_not_installed("abind")
  mmm <- abind::abind(mm, mm, along = 3)
  expect_message(
    write_txt_img(mmm, tmpfl, rds = TRUE),
    "_ch1.txt and .+_ch2.txt"
  )
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
    1, "5",
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

test_that("reading certain frames works", {
  path <- test_path("testthat-figs", "2ch_ij.tif")
  img <- read_tif(path, "A")
  img12 <- read_tif(path, frames = 1:2)
  img34 <- read_tif(path, frames = 3:4)
  img25 <- read_tif(path, frames = c(2, 5))
  expect_equal(
    img[, , , c(1, 2)] %>% {
      list(
        dim(.), as.vector(.),
        attributes(img) %T>% {
          .[["dim"]] <- c(dim(img)[1:3], 2)
        }
      )
    },
    img12 %>% {
      list(dim(.), as.vector(.), attributes(.))
    }
  )
  expect_equal(
    img[, , , c(3, 4)] %>% {
      list(
        dim(.), as.vector(.),
        attributes(img) %T>% {
          .[["dim"]] <- c(dim(img)[1:3], 2)
        }
      )
    },
    img34 %>% {
      list(dim(.), as.vector(.), attributes(.))
    }
  )
  expect_equal(
    img[, , , c(2, 5)] %>% {
      list(
        dim(.), as.vector(.),
        attributes(img) %T>% {
          .[["dim"]] <- c(dim(img)[1:3], 2)
        }
      )
    },
    img25 %>% {
      list(dim(.), as.vector(.), attributes(.))
    }
  )
  expect_error(read_tif(path, frames = 7),
    paste(
      " You have requested frame number 7 but there are",
      "only 5 frames in total. "
    ),
    fixed = TRUE
  )
})

test_that("Reading Mathieu's file works", {
  i2 <- read_tif(test_path("testthat-figs", "image2.tif"))
  expect_equal(dim(i2), c(200, 200, 6, 1))
  expect_equal(dim(attr(i2, "color_map")), c(256, 3))
  expect_equal(colnames(attr(i2, "color_map")), c("red", "green", "blue"))
})

test_that("extract_desired_plane() works", {
  aaa <- array(0, dim = rep(3, 3))
  expect_equal(extract_desired_plane(aaa), aaa[, , 1])
  aaa[] <- 1
  aaa[[1]] <- 2
  expect_error(
    extract_desired_plane(aaa),
    paste0(
      "Cannot extract the desired plane.+",
      "There are 2 unique nonzero planes, so it is impossible.+",
      "to decipher which is the correct one to extract."
    )
  )
})

test_that("tif_tags_reference() works", {
  expect_equal(
    tif_tags_reference(),
    readr::read_csv(system.file("extdata",
      "TIFF_tags.csv",
      package = "ijtiff"
    ))
  )
  expect_s3_class(tif_tags_reference(), "tbl_df")
})

test_that("prep_frames() errors correctly", {
  expect_error(
    prep_frames("xyz"),
    paste0("If.+frames.+is a string.+must be.+all.+You have.+frames.+xyz")
  )
})

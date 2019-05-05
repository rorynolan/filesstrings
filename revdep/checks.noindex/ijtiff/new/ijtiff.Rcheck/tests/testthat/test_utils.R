context("Utils")

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

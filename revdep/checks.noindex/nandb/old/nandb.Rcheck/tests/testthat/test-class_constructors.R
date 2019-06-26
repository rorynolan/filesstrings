context("Class constructors")

test_that("Class construction edge cases function correctly", {
  img <- array(runif(8), dim = rep(2, 3))
  eg <- number_img(img, "N", 4, structure(8, auto = TRUE), NA)
  expect_equal(attr(eg, "thresh"), rep(4, 2))
  expect_equal(
    attr(eg, "swaps"),
    structure(c(8, 8), auto = c(TRUE, TRUE))
  )
  expect_error(brightness_img(img, "a", 4, 9, NA), "def.*must.*one of")
  expect_error(brightness_ts_img(img, "a", 10, 4, 9, NA), "def.*must.*one of")
  x <- list(
    structure(4, a = 0),
    structure(5, b = 0)
  )
  ans <- 4:5
  attributes(ans) <- list(a = c(0, NA), b = c(NA, 0))
  expect_equal(c_list_attr_na(x), ans)
  img <- array(runif(3^3), dim = rep(3, 3))
  expect_error(number_img(
    img, "n", 5,
    structure(4:6, auto = rep(FALSE, 2)),
    NA
  ), "auto.*attribute.*swaps.*same length.*swaps.*itself")
  expect_equal(
    number_img(
      img, "n", 5,
      structure(4:6, auto = FALSE), "med"
    ),
    number_img(
      img, "n", 5,
      structure(4:6, auto = rep(FALSE, 3)), rep("median", 3)
    )
  )
  expect_error(
    number_img(img, "n", 5, structure(4:6, auto = NA), NA),
    "swaps.*att.*not NA"
  )
  expect_error(
    number_img(
      img, "n", 5,
      structure(4:5, auto = rep(FALSE, 2)), NA
    ),
    "thresh.*swaps.*filt.*same.*channels"
  )
  expect_error(
    cc_number_img_common(img, 1, 2, "s"),
    paste0(
      "If swaps is specified, it must have an attribute\\s?",
      "'auto'.+Your `swaps` has no attribute 'auto'"
    )
  )
  expect_error(
    cc_number_img_common(
      img, 1:2,
      structure(1, auto = TRUE), "s"
    ),
    "Assertion on.*swaps.*failed: Must have length 2.* has length 1"
  )
  expect_error(
    cc_number_img_common(
      img, 1:2,
      structure(1, auto = 1:2), "s"
    ),
    paste0(
      "The 'auto' attribute of `swaps` must be the same length\\s?",
      "as.+'swaps' itself.+Your `swaps` has length 1 and its\\s?",
      "'auto' attribute has.+length 2"
    )
  )
  expect_error(
    cc_number_img_common(
      img, 1:2,
      structure(1, auto = NA), "s"
    ),
    paste0(
      "Each element of `swaps` must have an associated\\s?",
      "attribute.+'auto' which must be `TRUE` or `FALSE` and not\\s?",
      "NA.+Element 1 of `swaps` has an 'auto' attribute which\\s?",
      "is.+NA"
    )
  )
  expect_equal(
    cc_number_img_common(
      img, 1:2,
      structure(1:2, auto = rep(TRUE, 2)),
      "smooth"
    ),
    cc_number_img_common(
      img, 1:2,
      structure(1:2, auto = TRUE),
      "smooth"
    )
  )
  expect_error(
    number_img(img, "r", 3, NA, NA),
    paste0(
      "Argument `def` must be one of 'n' or 'N'.+You have used\\s?",
      "`def = 'r'`"
    )
  )
  expect_error(number_img(img, "n", 4, 5, NA), "swaps.*spec.*must.*attr.*auto")
})

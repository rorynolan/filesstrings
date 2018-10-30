context("Class constructors")

test_that("Class construction edge cases function correctly", {
  img <- array(runif(8), dim = rep(2, 3))
  eg <- number_img(img, "N", 4, rlang::set_attrs(8, auto = TRUE), NA)
  expect_equal(attr(eg, "thresh"), rep(4, 2))
  expect_equal(attr(eg, "tau"),
               rlang::set_attrs(c(8, 8), auto = c(T, T)))
  expect_error(number_img(img, "n", 4, 5, NA), "tau.*spec.*must.*attr.*auto")
  expect_error(brightness_img(img, "a", 4, 9, NA), "def.*must.*one of")
  expect_error(brightness_ts_img(img, "a", 10, 4, 9, NA), "def.*must.*one of")
  x <- list(rlang::set_attrs(4, a = 0),
            rlang::set_attrs(5, b = 0))
  ans <- 4:5
  attributes(ans) <- list(a = c(0, NA), b = c(NA, 0))
  expect_equal(c_list_attr_na(x), ans)
  img <- array(runif(3 ^ 3), dim = rep(3, 3))
  expect_error(number_img(img, "n", 5,
                          rlang::set_attrs(4:6, auto = rep(FALSE, 2)),
                          NA), "auto.*attribute.*tau.*same length.*tau.*itself")
  expect_equal(number_img(img, "n", 5,
                          rlang::set_attrs(4:6, auto = FALSE), NA),
               number_img(img, "n", 5,
                          rlang::set_attrs(4:6, auto = rep(FALSE, 3)), NA))
  expect_error(number_img(img, "n", 5, rlang::set_attrs(4:6, auto = NA), NA),
               "tau.*att.*not NA")
  expect_error(number_img(img, "n", 5,
                          rlang::set_attrs(4:5, auto = rep(FALSE, 2)), NA),
               "thresh.*tau.*filt.*same.*channels")
  expect_error(cc_number_img_common(img, 1, 2, "s"),
               "If `tau` is specified, it must have an attribute.*auto")
  expect_error(cc_number_img_common(img, 1:2,
                                    rlang::set_attrs(1, auto = TRUE), "s"),
               "Assertion on.*tau.*failed: Must have length 2.* has length 1")
  expect_error(cc_number_img_common(img, 1:2,
                                    rlang::set_attrs(1, auto = 1:2), "s"),
               "auto.* attr.* of.*tau.*must be.*same length as.*tau.*itself")
  expect_error(cc_number_img_common(img, 1:2,
                                    rlang::set_attrs(1, auto = NA), "s"),
               paste("Each element of.*tau.*must have .* attribute.*auto.*",
                     "which must be `TRUE` or `FALSE` and not `NA`."))
  expect_equal(cc_number_img_common(img, 1:2,
                                    rlang::set_attrs(1:2, auto = rep(TRUE, 2)),
                                    "smooth"),
               cc_number_img_common(img, 1:2,
                                    rlang::set_attrs(1:2, auto = TRUE),
                                    "smooth"))
})

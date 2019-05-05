context("Utils")

test_that("translate_fail works", {
  expect_equal(translate_fail(3, "s"), 2 ^ 8 - 1)
  expect_equal(translate_fail(2 ^ 8 + 3, "s"), 2 ^ 16 - 1)
  expect_equal(translate_fail(2 ^ 30 + 3, "s"), 2 ^ 32 - 1)
  expect_equal(translate_fail(3, "Z"), 0)
  expect_equal(translate_fail(1:10, 77), 77)
  expect_error(translate_fail(1:3, -1),
               paste0(
                 "If `fail` is specified as a number, then that number\\s?",
                 "must be.+greater than zero.+You have specified `fail =\\s?",
                 "-1`."
               ))
})

test_that("`ptem()` works", {
  skip_if_not(identical(Sys.getenv("TRAVIS"), "true"))
  ptem_pkgs <- c("clipr", "styler", "ore")
  for (p in ptem_pkgs) {
    skip_if_not_installed(p)
  }
  clipr::write_clip("err_fun()")
  expect_equal(
    as.character(ptem()),
    "\"An error message to give ptem\\\\(\\\\) full coverage.\""
  )
})

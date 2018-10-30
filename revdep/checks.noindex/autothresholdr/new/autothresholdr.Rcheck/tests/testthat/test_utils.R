context("Utils")

test_that("translate_fail works", {
  expect_equal(translate_fail(3, "s"), 2 ^ 8 - 1)
  expect_equal(translate_fail(2 ^ 8 + 3, "s"), 2 ^ 16 - 1)
  expect_equal(translate_fail(2 ^ 30 + 3, "s"), 2 ^ 32 - 1)
  expect_equal(translate_fail(3, "Z"), 0)
  expect_equal(translate_fail(1:10, 77), 77)
})

test_that("`int_anyNA()` works", {
  expect_equal(int_anyNA(NA_integer_), TRUE)
})

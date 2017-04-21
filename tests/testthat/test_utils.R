test_that("AllEqual works", {
  expect_true(AllEqual(1, rep(1, 3)))
  expect_true(AllEqual(rep(1, 3), 1))
  expect_false(AllEqual(2, 1:3))
  expect_true(AllEqual(1:4, 1:4))
  expect_false(AllEqual(1:4, c(1, 2, 3, 3)))
  expect_true(AllEqual(rep(1, 10)))
  expect_false(AllEqual(c(1, 88)))
  expect_false(AllEqual(character(0), NA))
  expect_false(AllEqual(NA, character(0)))
  expect_false(AllEqual(NULL, NA))
})

test_that("GroupClose works", {
  expect_equal(GroupClose(1:10, 1), list(1:10))
  expect_equal(GroupClose(1:10, 0.5), as.list(1:10))
  expect_equal(GroupClose(c(1, 2, 4, 10, 11, 14, 20, 25, 27), 3),
               list(c(1, 2, 4), c(10, 11, 14), 20, c(25, 27)))
  expect_error(GroupClose(integer(0)))
  expect_error(GroupClose(rep(1, 2)))
  expect_equal(GroupClose(0), list(0))
  expect_equal(GroupClose(c(0, 2)), list(0, 2))
})

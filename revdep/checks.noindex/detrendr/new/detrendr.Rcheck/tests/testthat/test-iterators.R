test_that("iterator edge case works", {
  mm <- matrix(1:4, nrow = 2)
  expect_equal(iter_mat_col_sets(mm, n_sets = 6), iterators::iapply(mm, 2))
  expect_equal(iter_mat_col_sets(mm, n_sets = 1), mm)
})

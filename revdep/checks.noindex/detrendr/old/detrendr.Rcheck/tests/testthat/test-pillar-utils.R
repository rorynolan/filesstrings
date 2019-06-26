test_that("turning pillars into columns and back again works", {
  aaa <- array(1:27, dim = rep(3, 3))
  expect_equal(aaa, pillars_to_cols(aaa) %>% cols_to_pillars(dim(aaa)))
})

context("`linescan_to_stack()`")

test_that("`linescan_to_stack()` works", {
  linescan <- ijtiff_img(array(rep(1:4, each = 4), dim = c(
    4, 4,
    1, 1
  )))
  stack <- linescan_to_stack(linescan)
  expect_equal(
    array(stack, dim = dim(stack)),
    array(rep(1:4, 4), dim = c(1, 4, 1, 4))
  )
  expect_equal(linescan, stack_to_linescan(stack))
  arr <- array(1, dim = rep(4, 4))
  expect_error(
    linescan_to_stack(arr),
    paste0(
      "The fourth dimension of `linescan_img` should be equal to 1.+",
      "\\(or else it's not a linescan image\\).+",
      ". Yours has dim\\(linescan_img\\)\\[4\\] == 4."
    )
  )
  expect_error(
    stack_to_linescan(arr),
    paste0(
      "The first dimension of `img` should be equal to 1 \\(or else.+",
      "it's not a stack that can be converted to a linescan\\).+",
      "\\* Yours has dim\\(img\\)\\[1\\] == 4\\."
    )
  )
})

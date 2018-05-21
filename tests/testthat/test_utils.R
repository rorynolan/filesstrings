context("Utils")

test_that("all_equal works", {
  expect_true(all_equal(1, rep(1, 3)))
  expect_true(all_equal(rep(1, 3), 1))
  expect_false(all_equal(2, 1:3))
  expect_true(all_equal(1:4, 1:4))
  expect_false(all_equal(1:4, c(1, 2, 3, 3)))
  expect_true(all_equal(rep(1, 10)))
  expect_false(all_equal(c(1, 88)))
  expect_false(all_equal(character(0), NA))
  expect_false(all_equal(NA, character(0)))
  expect_false(all_equal(NULL, NA))
  expect_true(all_equal(1:4, matrix(1:4, nrow = 2)))
  expect_true(all_equal(matrix(1:4, nrow = 2), 1:4))
})

test_that("group_close works", {
  expect_equal(group_close(1:10, 1), list(1:10))
  expect_equal(group_close(1:10, 0.5), as.list(1:10))
  expect_equal(group_close(c(1, 2, 4, 10, 11, 14, 20, 25, 27), 3),
               list(c(1, 2, 4), c(10, 11, 14), 20, c(25, 27)))
  expect_error(group_close(integer(0)))
  expect_error(group_close(rep(1, 2)))
  expect_equal(group_close(0), list(0))
  expect_equal(group_close(c(0, 2)), list(0, 2))
})

test_that("match_arg() works", {
  expect_equal(match_arg("ab", c("abcdef", "defgh")), "abcdef")
  expect_error(match_arg("abcdefg", c("Abcdef", "defg")), "not a prefix of any")
  expect_equal(match_arg("ab", c("Abcdef", "defgh"), ignore_case = TRUE),
               "Abcdef")
  expect_equal(match_arg("ab", c("xyz", "Abcdef", "defgh"),
                         ignore_case = TRUE, index = TRUE), 2)
  choices <- c("Apples", "Pears", "Bananas", "Oranges")
  expect_equal(match_arg(NULL, choices), "Apples")
  expect_equal(match_arg("A", choices), "Apples")
  expect_equal(match_arg("B", choices, index = TRUE), 3)
  expect_equal(match_arg(c("b", "a"), choices, several_ok = TRUE,
                         ignore_case = TRUE),
               c("Bananas", "Apples"))
  expect_equal(match_arg(c("b", "a"), choices, ignore_case = TRUE, index = TRUE,
                         several_ok = TRUE),
               c(3, 1))
  choices %<>% c("Avocados", "Apricots")
  expect_error(match_arg("A", choices, ignore_case = FALSE),
               "prefix of two.+Apples.+Avocados.+")
  expect_error(match_arg("a", choices, ignore_case = TRUE),
               "prefix of two.+Apples.+Avocados.+")
  expect_error(match_arg(c("A", "a"), choices),
               "arg.+must have length 1.+use.+several_ok = TRUE")
  choices %<>% c("bananas")
  expect_error(match_arg("p", choices, ignore_case = TRUE),
               "elements 3 and 7.+Bananas.+bananas.+effectively duplicates.")
  choices %<>% c("Pears")
  expect_error(match_arg("p", choices, ignore_case = TRUE),
               "Element 8.+Pears.+is a duplicate")
  expect_equal(match_arg("ab", c("ab", "abc")), "ab")
})

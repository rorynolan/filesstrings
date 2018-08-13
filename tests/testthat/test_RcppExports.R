context("C++ stuff")

test_that("paste_collapse_list_elems works", {
  expect_equal(paste_collapse_list_elems(list(1:3, c("a", 5, "rory")),
                                      collapse = "R"),
               c("1R2R3", "aR5Rrory"))
})

test_that("str_list_remove_empties works", {
  expect_equal(str_list_remove_empties(list(c("a", "", "b"), "gg",
                                            c("", 1, ""))),
               list(c("a", "b"), "gg", "1"))
})

test_that("str_list_nth_elems works", {
  expect_equal(str_list_nth_elems(list(c("a", "b", "c"),
                                         c("d", "f", "a")), 2),
               c("b", "f"))
  expect_equal(num_list_nth_elems(list(1:5, 0:2), 4), c(4, NA))
})

test_that("Random Rcpp stuff works",{
  expect_equal(filesstrings:::interleave_strings(c("a", "b", "v"), "a"),
               NA_character_)
  expect_equal(filesstrings:::interleave_correctly_vec("a", character(0), "a"),
               "a")
  expect_equal(filesstrings:::interleave_correctly_vec("a", "a", character(0)),
               "a")
  expect_equal(filesstrings:::interleave_correctly_vec("ab", "b", "a"),
               c("a", "b"))
  expect_equal(filesstrings:::interleave_correctly("a", list(), list()),
               list(NA_character_))
  expect_equal(filesstrings:::interleave_char_lists(list("a"), list()),
               list(NA_character_))
})


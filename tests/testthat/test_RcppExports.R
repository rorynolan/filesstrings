test_that("PasteCollapseListElems works", {
  expect_equal(PasteCollapseListElems(list(1:3, c("a", 5, "rory")),
                                      collapse = "R"),
               c("1R2R3", "aR5Rrory"))
})

test_that("StrListRemoveEmpties works", {
  expect_equal(StrListRemoveEmpties(list(c("a", "", "b"), "gg", c("", 1, ""))),
               list(c("a", "b"), "gg", "1"))
})

test_that("CharListElemsNthElem works", {
  expect_equal(CharListElemsNthElem(list(c("a", "b", "c"),
                                         c("d", "f", "a")), 2),
               c("b", "f"))
  expect_equal(NumListElemsNthElem(list(1:5, 0:2), 4), c(4, NA))
})

test_that("Random Rcpp stuff works",{
  expect_equal(filesstrings:::InterleaveStrings(c("a", "b", "v"), "a"),
               NA_character_)
  expect_equal(filesstrings:::CorrectInterleave0("a", character(0), "a"), "a")
  expect_equal(filesstrings:::CorrectInterleave0("a", "a", character(0)), "a")
  expect_equal(filesstrings:::CorrectInterleave0("ab", "b", "a"), c("a", "b"))
  expect_equal(filesstrings:::CorrectInterleave("a", list(), list()),
               list(NA_character_))
  expect_equal(filesstrings:::InterleaveStringList(list("a"), list()),
               list(NA_character_))
})

test_that("CanBeNumeric works", {
  expect_true(CanBeNumeric("3"))
  expect_true(CanBeNumeric("5 "))
  expect_equal(CanBeNumeric(c("1a", "abc")), rep(FALSE, 2))
})

test_that("GetCurrencies works", {
  expect_equal(GetCurrencies("35.00 $1.14 abc5 $3.8 77"),
               tibble::tibble(currency = c("", "$", "c", "$", " "),
                              amount = c(35, 1.14, 5, 3.8, 77)))
})

test_that("GetCurrency works", {
  expect_equal(GetCurrency(c("ab3 13", "$1")), c("b", "$"))
})

test_that("DuplicatesToSingles works", {
  expect_equal(DuplicatesToSingles("abc//def", "/"), "abc/def")
  expect_equal(DuplicatesToSingles("abababcabab", "ab"), "abcab")
  expect_equal(DuplicatesToSingles(c("abab", "cdcd"), "cd"), c("abab", "cd"))
  expect_equal(DuplicatesToSingles(c("abab", "cdcd"), c("ab", "cd")),
               c("ab", "cd"))
})

test_that("NiceNums works", {
  strings <- paste0("abc", 1:12)
  expect_equal(NiceNums(strings), paste0("abc", c(paste0(0, 1:9), 10:12)))
  expect_equal(NiceNums(c("01abc9def55", "5abc10def777", "99abc4def4")),
               c("01abc09def055", "05abc10def777", "99abc04def004"))
  expect_equal(NiceNums(c("abc9def55", "abc10def7")),
               c("abc09def55", "abc10def07"))
  expect_equal(NiceNums(c("abc9def55", "abc10def777", "abc4def4")),
               c("abc09def055", "abc10def777", "abc04def004"))
  expect_error(NiceNums(c("abc9def55", "abc10xyz7")))
  expect_error(NiceNums(c("abc9def55", "9abc10def7")))
  expect_error(NiceNums(c("0abc9def55g", "abc10def7g0")))
})

test_that("ExtractNumbers works", {
  expect_equal(ExtractNumbers(c("abc123abc456", "abc1.23abc456")),
               list(c(123, 456), c(1, 23, 456)))
  expect_equal(ExtractNumbers(c("abc1.23abc456", "abc1..23abc456"),
                              decimals = TRUE),
               list(c(1.23, 456), c(1, 23, 456)))
  expect_equal(ExtractNumbers("abc1..23abc456", decimals = TRUE),
               list(c(1, 23, 456)))
  expect_equal(ExtractNumbers("abc1..23abc456", decimals = TRUE,
                              leading.decimals = TRUE), list(c(1, .23, 456)))
  expect_equal(ExtractNumbers("abc1..23abc456", decimals = TRUE,
                              leading.decimals = TRUE,
  leave.as.string = TRUE), list((c("1", ".23", "456"))))
  expect_equal(ExtractNumbers("-123abc456"), list(c(123, 456)))
  expect_equal(ExtractNumbers("-123abc456", negs = TRUE), list(c(-123, 456)))
  expect_equal(ExtractNumbers("--123abc456", negs = TRUE), list(c(-123, 456)))
  expect_equal(ExtractNonNumerics("abc123abc456"), list(rep("abc", 2)))
  expect_equal(ExtractNonNumerics("abc1.23abc456"), list(c("abc", ".", "abc")))
  expect_equal(ExtractNonNumerics("abc1.23abc456", decimals = TRUE),
               list(c("abc", "abc")))
  expect_equal(ExtractNonNumerics("abc1..23abc456", decimals = TRUE),
               list(c("abc", "..", "abc")))
  expect_equal(ExtractNonNumerics("abc1..23abc456", decimals = TRUE,
  leading.decimals = TRUE), list(c("abc", ".", "abc")))
  expect_equal(ExtractNonNumerics(c("-123abc456", "ab1c")),
               list(c("-", "abc"), c("ab", "c")))
  expect_equal(ExtractNonNumerics("-123abc456", negs = TRUE), list("abc"))
  expect_equal(ExtractNonNumerics("--123abc456", negs = TRUE),
               list(c("-", "abc")))
  expect_equal(ExtractNumbers("abc1.2.3", decimals = TRUE), list(NA_real_))
  expect_equal(ExtractNumbers("ab.1.2", decimals = TRUE,
                              leading.decimals = TRUE), list(NA_real_))
  expect_equal(NthNumber("abc1.23abc456", 2), 23)
  expect_equal(NthNumber("abc1.23abc456", 2, leave.as.string = TRUE), "23")
  expect_equal(NthNumber("abc1.23abc456", 2, decimals = TRUE), 456)
  expect_equal(NthNumber("-123abc456", -2, negs = TRUE), -123)
  expect_equal(ExtractNonNumerics("--123abc456", negs = TRUE),
               list(c("-", "abc")))
  expect_equal(NthNonNumeric("--123abc456", 1), "--")
  expect_equal(NthNonNumeric("--123abc456", -2), "--")
  expect_error(ExtractNumbers("a.23", leading.decimals = T))
  expect_error(ExtractNonNumerics("a.23", leading.decimals = T))
})

test_that("StrSplitByNums works", {
  expect_equal(StrSplitByNums(c("abc123def456.789gh", "a1b2c344")),
               list(c("abc", "123", "def", "456", ".", "789", "gh"),
                    c("a", 1, "b", 2, "c", 344)))
  expect_equal(StrSplitByNums("abc123def456.789gh", decimals = TRUE),
               list(c("abc", "123", "def", "456.789", "gh")))
  expect_equal(StrSplitByNums("22"), list("22"))
})

test_that("StrElem works", {
  expect_equal(StrElem("abcd", 3), "c")
  expect_equal(StrElem("abcd", -2), "c")
})

test_that("StrElemsPasted works", {
  expect_equal(StrElemsPasted("abcdef", c(2, 5:6)), "bef")
})

test_that("StringToVec works", {
  expect_equal(StringToVec("abcdef"), c("a", "b", "c", "d", "e", "f"))
})

test_that("StringsWithPatterns works", {
  expect_equal(StringsWithPatterns(c("abc", "bcd", "cde"), c("b", "c")),
               c("abc", "bcd"))
  expect_equal(StringsWithPatterns(c("abc", "bcd", "cde"), c("b", "c"),
                                   any = TRUE), c("abc", "bcd", "cde"))
  expect_equal(StringsWithPatterns(toupper(c("abc", "bcd", "cde")),
                                   c("b", "c"), any = TRUE),
               character(0))
  expect_equal(StringsWithPatterns(toupper(c("abc", "bcd", "cde")),
                                   c("b", "c"), any = TRUE,
  ignore.case = TRUE), toupper(c("abc", "bcd", "cde")))
})

test_that("StrReverse works", {
  expect_equal(StrReverse("abcdef"),
               paste(rev(c("a", "b", "c", "d", "e", "f")), collapse = ""))
})

test_that("StrAfterNth works", {
  string <- "ab..cd..de..fg..h"
  expect_equal(StrAfterNth(string, "\\.\\.", 3), "fg..h",
               check.attributes = FALSE)
  expect_equal(StrBeforeNth(string, "e", 1), "ab..cd..d",
               check.attributes = FALSE)
  expect_equal(StrBeforeNth(string, "\\.", -3), "ab..cd..de.",
               check.attributes = FALSE)
  expect_equal(StrBeforeNth(string, ".", -3), "ab..cd..de..fg",
               check.attributes = FALSE)
  expect_equal(StrBeforeNth(rep(string, 2), fixed("."), -3),
               rep("ab..cd..de.", 2), check.attributes = FALSE)
})

test_that("BeforeLastDot works", {
  expect_equal(BeforeLastDot(c("spreadsheet1.csv", "doc2.doc")),
               c("spreadsheet1", "doc2"), check.attributes = FALSE)
})

test_that("ExtendCharVec works", {
  expect_equal(ExtendCharVec(1:5, extend.by = 2), c(1:5, "", ""))
  expect_equal(ExtendCharVec(c("a", "b"), length.out = 10),
               c("a", "b", rep("", 8)))
  expect_error(ExtendCharVec("0"))
  expect_error(ExtendCharVec(c("0", 3)))
  expect_error(ExtendCharVec(c("0", "1"), length.out = 1))
})

test_that("PasteDifferentLengths works", {
  expect_equal(PasteDifferentLengths(list(1:3, 1:4)), c("11", 22, 33, 4))
  expect_equal(PasteDifferentLengths(list(1:3, 1:4), sep = "S"),
               c("1S1", "2S2", "3S3", 4))
  writeLines(as.character(1:3), "PasteDifferentLengths1.txt")
  writeLines(as.character(1:4), "PasteDifferentLengths2.txt")
  expect_equal(PasteDifferentLengths(list.files(pattern = "PasteDifferentLengths"),
  sep = "S"), c("1S1", "2S2", "3S3", 4))
  # clean up working directory
  file.remove(list.files(pattern = "PasteDifferentLengths"))
})

test_that("PutInPos works", {
  expect_equal(PutInPos(1:3, c(1, 8, 9)), c(1, rep("", 6), 2, 3))
  expect_equal(PutInPos(c("Apple", "Orange", "County"), c(5, 7, 8)),
               c(rep("", 4), "Apple", "", "Orange", "County"))
  expect_equal(PutInPos(1:2, 5), c(rep("", 4), 1:2))
})

test_that("TrimAnything works", {
  expect_equal(TrimAnything("..abcd.", ".", "left"), "abcd.")
  expect_equal(TrimAnything("-ghi--", "-"), "ghi")
  expect_equal(TrimAnything("-ghi--", "--"), "-ghi")
})

test_that("CountMatches works", {
  expect_equal(CountMatches("abacad", "a"), 3)
  expect_equal(CountMatches("2.1.0.13", "."), 8)
  expect_equal(CountMatches("2.1.0.13", stringr::coll(".")), 3)
  expect_error(CountMatches("2", c("0", 1)))
})

test_that("LocateBraces works", {
  expect_equal(LocateBraces(c("a{](kkj)})", "ab(]c{}")),
               list(tibble::tibble(position = as.integer(c(2, 3, 4, 8, 9, 10)),
                                   brace = c("{", "]", "(", ")", "}", ")")),
                    tibble::tibble(position = as.integer(c(3, 4, 6, 7)),
                                   brace = c("(", "]", "{", "}"))))
})

test_that("RemoveQuoted works", {
  string <- "\"abc\"67a\'dk\'f"
  expect_equal(RemoveQuoted(string), "67af")
})

test_that("MakeExtName works", {
  expect_equal(MakeExtName("abc.csv", "csv"), "abc.csv")
  expect_equal(MakeExtName("abc", "csv"), "abc.csv")
  expect_equal(MakeExtName("abc.csv", "pdf"), "abc.csv.pdf")
  expect_equal(MakeExtName("abc.csv", "pdf", replace = TRUE), "abc.pdf")
})

test_that("SplitCamelcase works", {
  expect_equal(SplitCamelcase(c("RoryNolan", "NaomiFlagg",
                                "DepartmentOfSillyHats")),
               list(c("Rory", "Nolan"), c("Naomi", "Flagg"),
                    c("Department", "Of", "Silly", "Hats")))
})

test_that("StrNthInstanceIndices errors in the right way", {
  expect_error(filesstrings:::StrNthInstanceIndices("aba", "a", 9))
})

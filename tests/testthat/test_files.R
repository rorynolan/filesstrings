test_that("NiceFileNums works", {
  setwd(tempdir())
  expect_true(dir.create("NiceFileNums_test"))
  setwd("NiceFileNums_test")
  files <- c("1litres_1.txt", "1litres_30.txt", "3litres_5.txt")
  expect_equal(file.create(files), rep(TRUE, 3))
  expect_equal(NiceFileNums(), rep(TRUE, 3))
  expect_equal(NiceFileNums(pattern = "\\.txt$"), rep(TRUE, 3))
  setwd("..")
  expect_true(RemoveDirs("NiceFileNums_test"))
})

test_that("RemoveFileNameSpaces works", {
  setwd(tempdir())
  expect_true(dir.create("RemoveFileNameSpaces_test"))
  setwd("RemoveFileNameSpaces_test")
  files <- c("1litres 1.txt", "1litres 30.txt", "3litres 5.txt")
  expect_equal(file.create(files), rep(TRUE, 3))
  expect_equal(RemoveFileNameSpaces(), rep(TRUE, 3))
  expect_equal(list.files(), c("1litres1.txt", "1litres30.txt", "3litres5.txt"))
  setwd("..")
  expect_true(RemoveDirs("RemoveFileNameSpaces_test"))
})

test_that("RenameWithNums works", {
  setwd(tempdir())
  expect_true(dir.create("RenameWithNums_test"))
  setwd("RenameWithNums_test")
  files <- c("1litres 1.txt", "1litres 30.txt", "3litres 5.txt")
  expect_equal(file.create(files), rep(TRUE, 3))
  expect_equal(RenameWithNums(), rep(TRUE, 3))
  expect_equal(list.files(), paste0(1:3, ".txt"))
  expect_error(RenameWithNums())
  file.create("xyz.csv")
  expect_error(RenameWithNums())
  setwd("..")
  expect_true(RemoveDirs("RenameWithNums_test"))
})

test_that("CreateDirsIfNotThere works", {
  setwd(tempdir())
  expect_equal(CreateDirsIfNotThere(c("mydir", "yourdir")), rep(TRUE, 2),
               check.names = FALSE)
  expect_equal(CreateDirsIfNotThere(c("mydir", "yourdir")), rep(FALSE, 2),
               check.names = FALSE)
  expect_equal(RemoveDirs(c("mydir", "yourdir")), rep(TRUE, 2),
               check.names = FALSE)
})

test_that("UnitDirs works", {
  setwd(tempdir())
  expect_equal(dir.create("UnitDirs_test"), TRUE)
  setwd("UnitDirs_test")
  files <- c("1litres_1.txt", "1litres_3.txt", "3litres.txt", "5litres_1.txt")
  expect_equal(file.create(files), rep(TRUE, length(files)))
  expect_true(UnitDirs("litres", "\\.txt"))
  file.create("10ml.txt")
  setwd("..")
  expect_error(UnitDirs("litres"))
  expect_true(RemoveDirs("UnitDirs_test"))
})

test_that("MoveFiles errors correctly", {
  setwd(tempdir())
  dir.create("tmpdir0")
  file.create("tmpfile0.R")
  expect_error(MoveFiles("tmpfile0.R", c("tmpdir0", "tmpdir0")))
})

test_that("MergeTables works", {
  setwd(tempdir())
  expect_true(dir.create("MergeTables_test"))
  setwd("MergeTables_test")
  tab1 <- tibble::tibble(x = 1, y = 2)
  tab2 <- tibble::tibble(x = 1, y = 29)
  tab3 <- tibble::tibble(x = 1, z = 29)
  tab4 <- tibble::tibble(x = 1, y = 29, z = 0)
  mapply(readr::write_csv, list(tab1, tab2, tab3, tab4),
         paste0(c("tab1", "tab2", "tab3", "tab4"), ".csv"))
  expect_equal(MergeTables(c("tab1.csv", "tab2.csv"), ",", "merged.csv"),
               tibble::tibble(x = c(1, 1), y = c(2, 29)))
  expect_equal(readr::read_csv("merged.csv"),
               tibble::tibble(x = c(1, 1), y = c(2, 29)))
  expect_error(MergeTables(c("tab1.csv", "tab3.csv"), ",", "merged.csv"))
  expect_error(MergeTables(c("tab1.csv", "tab4.csv"), ",", "merged.csv"))
  setwd("..")
  expect_true(RemoveDirs("MergeTables_test"))
})

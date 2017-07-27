test_that("nice_file_nums works", {
  setwd(tempdir())
  expect_true(dir.create("nice_file_nums_test"))
  setwd("nice_file_nums_test")
  files <- c("1litres_1.txt", "1litres_30.txt", "3litres_5.txt")
  expect_equal(file.create(files), rep(TRUE, 3))
  expect_equal(nice_file_nums(), rep(TRUE, 3))
  expect_equal(nice_file_nums(pattern = "\\.txt$"), rep(TRUE, 3))
  setwd("..")
  expect_true(dir.remove("nice_file_nums_test"))
})

test_that("remove_filename_spaces works", {
  setwd(tempdir())
  expect_true(dir.create("remove_filename_spaces_test"))
  setwd("remove_filename_spaces_test")
  files <- c("1litres 1.txt", "1litres 30.txt", "3litres 5.txt")
  expect_equal(file.create(files), rep(TRUE, 3))
  expect_equal(remove_filename_spaces(), rep(TRUE, 3))
  expect_equal(list.files(), c("1litres1.txt", "1litres30.txt", "3litres5.txt"))
  setwd("..")
  expect_true(dir.remove("remove_filename_spaces_test"))
})

test_that("rename_with_nums works", {
  setwd(tempdir())
  expect_true(dir.create("rename_with_nums_test"))
  setwd("rename_with_nums_test")
  files <- c("1litres 1.txt", "1litres 30.txt", "3litres 5.txt")
  expect_equal(file.create(files), rep(TRUE, 3))
  expect_equal(rename_with_nums(), rep(TRUE, 3))
  expect_equal(list.files(), paste0(1:3, ".txt"))
  expect_error(rename_with_nums())
  file.create("xyz.csv")
  expect_error(rename_with_nums())
  setwd("..")
  expect_true(dir.remove("rename_with_nums_test"))
})

test_that("create_dirs works", {
  setwd(tempdir())
  expect_equal(create_dirs(c("mydir", "yourdir")), rep(TRUE, 2),
               check.names = FALSE)
  expect_equal(create_dirs(c("mydir", "yourdir")), rep(FALSE, 2),
               check.names = FALSE)
  expect_equal(dir.remove(c("mydir", "yourdir")), rep(TRUE, 2),
               check.names = FALSE)
})

test_that("unitize_dirs works", {
  setwd(tempdir())
  expect_equal(dir.create("unitize_dirs_test"), TRUE)
  setwd("unitize_dirs_test")
  files <- c("1litres_1.txt", "1litres_3.txt", "3litres.txt", "5litres_1.txt")
  expect_equal(file.create(files), rep(TRUE, length(files)))
  expect_true(unitize_dirs("litres", "\\.txt"))
  file.create("10ml.txt")
  setwd("..")
  expect_error(unitize_dirs("litres"))
  expect_true(dir.remove("unitize_dirs_test"))
})

test_that("file.move errors correctly", {
  setwd(tempdir())
  dir.create("tmpdir0")
  file.create("tmpfile0.R")
  expect_error(file.move("tmpfile0.R", c("tmpdir0", "tmpdir0")))
})

test_that("merge_tables_on_disk works", {
  setwd(tempdir())
  expect_true(dir.create("merge_tables_on_disk_test"))
  setwd("merge_tables_on_disk_test")
  tab1 <- tibble::tibble(x = 1.5, y = 2.5)
  tab2 <- tibble::tibble(x = 1.5, y = 29.5)
  tab3 <- tibble::tibble(x = 1.5, z = 29.5)
  tab4 <- tibble::tibble(x = 1.5, y = 29.5, z = 0.5)
  mapply(readr::write_csv, list(tab1, tab2, tab3, tab4),
         paste0(c("tab1", "tab2", "tab3", "tab4"), ".csv"))
  expect_equal(merge_tables_on_disk(c("tab1.csv", "tab2.csv"), ",", "merged.csv"),
               tibble::tibble(x = c(1.5, 1.5), y = c(2.5, 29.5)),
               check.attributes = FALSE)
  expect_equal(readr::read_csv("merged.csv"),
               tibble::tibble(x = c(1.5, 1.5), y = c(2.5, 29.5)),
               check.attributes = FALSE)
  expect_error(merge_tables_on_disk(c("tab1.csv", "tab3.csv"), ",", "merged.csv"))
  expect_error(merge_tables_on_disk(c("tab1.csv", "tab4.csv"), ",", "merged.csv"))
  setwd("..")
  expect_true(dir.remove("merge_tables_on_disk_test"))
})


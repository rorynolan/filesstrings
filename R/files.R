#' Create directories if they don't already exist
#'
#' Given the names of (potential) directories, create the ones that do not
#' already exist.
#' @param ... The names of the directories, specified via relative or absolute
#'   paths. Duplicates are ignored.
#' @return Invisibly, a vector with a `TRUE` for each time a directory was
#'   actually created and a `FALSE` otherwise. Theis vector is named with the
#'   paths of the directories that were passed to the function.
#' @examples
#' setwd(tempdir())
#' create_dirs(c("mydir", "yourdir"))
#' remove_dirs(c("mydir", "yourdir"))
#' @export
create_dirs <- function(...) {
  dirs <- unique(unlist(...))
  created <- purrr::map_lgl(dirs, function(dir) {
                                    if (!dir.exists(dir)) {
                                      dir.create(dir)
                                      TRUE
                                    } else {
                                      FALSE
                                    }
                                  })
  names(created) <- dirs
  msg <- sum(created) %>% {
    ifelse(., paste(., ifelse(. == 1, "directory", "directories"),
                    "created. "),
           "")
  }
  n_not_created <- sum(!created)
  if (n_not_created) {
    msg %<>% paste0(n_not_created,
                    ifelse(n_not_created == 1, " directory", " directories"),
                    " not created because ",
                    ifelse(n_not_created == 1,
                           "it already exists.",
                           "they already exist."))
  }
  message(msg)
  invisible(created)
}

#' Remove directories
#'
#' Delete directories and all of their contents.
#' @param ... The names of the directories, specified via relative or absolute
#'   paths.
#' @return Invisibly, a logical vector with `TRUE` for each success and
#'   `FALSE` for failures.
#' @examples
#' setwd(tempdir())
#' sapply(c("mydir1", "mydir2"), dir.create)
#' remove_dirs(c("mydir1", "mydir2"))
#' @export
remove_dirs <- function(...) {
  dirs <- unlist(...)
  outcome <- !as.logical(purrr::map_int(dirs, unlink, recursive = TRUE))
  outcome %>% {
    paste(sum(.), ifelse(sum(.) == 1, "directory", "directories"),
          "deleted.", sum(!.), "failed to delete.")
  } %>% message()
  invisible(outcome)
}

#' @rdname remove_dirs
#' @export
dir.remove <- remove_dirs

#' Merge tables on disk
#'
#' Merge tables saved on disk as delimited files. They need to have the same
#' number of columns and the same column names (if they have column names).
#'
#' @param files The paths to the files to merge.
#' @param delim Delimeter used to separate values.
#' @param out_name The path to the output file containing the merged tables.
#' @param header Do the tables to be merged have headers?
#' @param ... Additional arguments passed to [readr::read_delim].
#' @examples
#' setwd(tempdir())
#' dir.create("MergeTablesOnDisk_test")
#' setwd("MergeTablesOnDisk_test")
#' tab1 <- tibble::tibble(x = 1, y = 2)
#' tab2 <- tibble::tibble(x = 1, y = 29)
#' mapply(readr::write_csv, list(tab1, tab2), paste0(c("tab1", "tab2"), ".csv"))
#' merge_tables_on_disk(c("tab1.csv", "tab2.csv"), ",", "merged.csv")
#' readr::read_csv("merged.csv")
#' setwd("..")
#' dir.remove("MergeTablesOnDisk_test")
#' @export
merge_tables_on_disk <- function(files, delim,
                                 out_name, header = TRUE, ...) {
  tables <- lapply(files, readr::read_delim, delim, col_names = header, ...)
  ncs <- vapply(tables, ncol, integer(1))
  if (!all_equal(ncs)) stop("The tables have different numbers of columns.")
  if (header) {
    namess <- vapply(tables, names, character(ncs[1]))
    if (!all(apply(namess, 1, all_equal))) {
      stop("Tables have different colnames.")
    }
  }
  merged <- Reduce(rbind, tables)
  readr::write_delim(merged, out_name, delim = delim, col_names = header)
}

move_file <- function(file, destination) {
  # This also works for directories
  file <- normalizePath(file)  # get full path
  file.name.base <- basename(file)
  destination <- normalizePath(destination)  # remove risk of tilde use
  new.name <- paste0(destination, "/", file.name.base)
  file.rename(file, new.name)
}

#' Move files around.
#'
#' Move specified files into specified directories
#'
#' If there are \eqn{n} files, there must be either \eqn{1} or \eqn{n}
#' directories. If there is one directory, then all \eqn{n} files are moved
#' there. If there are \eqn{n} directories, then each file is put into its
#' respective directory. This function also works to move directories.
#'
#' If you try to move files to a directory that doesn't exist, the directory is
#' first created and then the files are put inside.
#'
#' @param files A character vector of files to move (relative or absolute
#'   paths).
#' @param destinations A character vector of the destination directories into
#'   which to move the files.
#' @return Invisibly, a logical vector with a `TRUE` for each time the operation
#'   succeeded and a `FALSE` for every fail.
#' @examples
#' setwd(tempdir())
#' dir.create("dir")
#' files <- c("1litres_1.txt", "1litres_30.txt", "3litres_5.txt")
#' file.create(files)
#' file.move(files, "dir")
#' @export
move_files <- function(files, destinations) {
  if (! length(destinations) %in% (c(1, length(files)))) {
    stop("The number of destinations must be equal to 1 or equal to the ",
         "number of files to be moved")
  }
  n_created_dirs <- sum(suppressMessages(create_dirs(destinations)))
  if (n_created_dirs > 0) {
    message(n_created_dirs, " ", "director",
            ifelse(n_created_dirs == 1, "y", "ies"),
            " created.")
  }
  if(length(destinations) == length(files)) {
    outcome <- mapply(move_file, files, destinations)
  } else {
    outcome <- vapply(files, move_file, logical(1), destinations)
  }
  message(sum(outcome), ifelse(sum(outcome) == 1, " file", " files"),
          " moved. ", sum(!outcome), " failed.")
  invisible(outcome)
}

#' @rdname move_files
#' @export
file.move <- move_files

#' Make file numbers comply with alphabetical order
#'
#' If files are numbered, their numbers may not \emph{comply} with alphabetical
#' order, i.e. "file2.ext" comes after "file10.ext" in alphabetical order. This
#' function renames the files in the specified directory such that they comply
#' with alphabetical order, so here "file2.ext" would be renamed to
#' "file02.ext".
#'
#' It works on file names with more than one number in them e.g.
#' "file01part3.ext" (a file with 2 numbers). All the file names that it works
#' on must have the same number of numbers, and the non-number bits must be the
#' same. One can limit the renaming to files matching a certain pattern. This
#' function wraps [nice_nums()], which does the string operations, but
#' not the renaming. To see examples of how this function works, see the
#' examples in that function's documentation.
#'
#' @param dir Path (relative or absolute) to the directory in which to do the
#'   renaming (default is current working directory).
#' @param pattern A regular expression. If specified, files to be renamed are
#'   restricted to ones matching this pattern (in their name).
#' @return A logical vector with a `TRUE` for each successful rename
#'   (should be all `TRUE`s) and a `FALSE` otherwise.
#'
#' @examples
#' setwd(tempdir())
#' dir.create("NiceFileNums_test")
#' setwd("NiceFileNums_test")
#' files <- c("1litres_1.txt", "1litres_30.txt", "3litres_5.txt")
#' file.create(files)
#' nice_file_nums()
#' nice_file_nums(pattern = "\\.txt$")
#' setwd("..")
#' dir.remove("NiceFileNums_test")
#' @export
nice_file_nums <- function(dir = ".", pattern = NA) {
  init.dir <- getwd()
  on.exit(setwd(init.dir))
  setwd(dir)
  if (is.na(pattern)) {
    lf <- list.files()
  } else {
    lf <- list.files(pattern = pattern)
  }
  renamed <- file.rename(lf, nice_nums(lf))
  message(sum(renamed), " files renamed into the desired format. ",
          sum(!renamed), " failed.")
  invisible(renamed)
}

#' Remove spaces in file names
#'
#' Remove spaces in file names in a specified directory, replacing them with
#' whatever you want, default nothing.
#'
#' @param dir The directory in which to perform the operation.
#' @param pattern A regular expression. If specified, only files matching this
#'   pattern wil be treated.
#' @param replacement What do you want to replace the spaces with? This
#'   defaults to nothing, another sensible choice would be an underscore.
#' @return A logical vector indicating which operation succeeded for each of the
#'   files attempted. Using a missing value for a file or path name will always
#'   be regarded as a failure.
#' @examples
#' setwd(tempdir())
#' dir.create("RemoveFileNameSpaces_test")
#' setwd("RemoveFileNameSpaces_test")
#' files <- c("1litres 1.txt", "1litres 30.txt", "3litres 5.txt")
#' file.create(files)
#' remove_filename_spaces()
#' list.files()
#' setwd("..")
#' dir.remove("RemoveFileNameSpaces_test")
#' @export
remove_filename_spaces <- function(dir = ".", pattern = "", replacement = "") {
  init.dir <- getwd()
  on.exit(setwd(init.dir))
  setwd(dir)
  lf <- list.files(pattern = pattern)
  new.names <- str_replace_all(lf, " ", replacement)
  outcome <- file.rename(lf, new.names)
  message(sum(outcome), " files renamed. ", sum(!outcome),
          " failed to rename.")
  invisible(outcome)
}

#' Replace file names with numbers
#'
#' Rename the files in the directory, replacing file names with numbers only.
#'
#' @param dir The directory in which to rename the files (relative or absolute
#'   path). Defaults to current working directory.
#' @param pattern A regular expression. If specified, only files with names
#'   matching this pattern will be treated.
#' @return A logical vector with a `TRUE` for each successful renaming and a
#'   `FALSE` otherwise.
#' @examples
#' setwd(tempdir())
#' dir.create("RenameWithNums_test")
#' setwd("RenameWithNums_test")
#' files <- c("1litres 1.txt", "1litres 30.txt", "3litres 5.txt")
#' file.create(files)
#' rename_with_nums()
#' list.files()
#' setwd("..")
#' dir.remove("RenameWithNums_test")
#' @export
rename_with_nums <- function(dir = ".", pattern = NULL) {
  init.dir <- getwd()
  on.exit(setwd(init.dir))
  setwd(dir)
  lf <- list.files(pattern = pattern)
  ext <- unique(tools::file_ext(lf))
  if (length(ext) != 1) {
    stop("Files matching pattern have different extensions.")
  }
  l <- length(lf)
  if (l == 0) stop("No files found to rename.")
  new_names <- nice_nums(paste0(seq_len(l), ".", ext))
  if (any(new_names %in% lf)) {
    stop("Some of the names are already in the desired format, ",
         "unable to proceed as renaming may result in deletion.")
  }
  file.rename(lf, new_names)
}

#' Put files with the same unit measurements into directories
#'
#' Say you have a number of files with "5min" in their names, number with
#' "10min" in the names, a number with "15min" in their names and so on, and
#' you'd like to put them into directories named "5min", "10min", "15min" and so
#' on. This function does this, but not just for the unit "min", for any unit.
#'
#' This function takes the number to be the last number (as defined in
#' [nth_number()]) before the first occurrence of the unit name. There
#' is the option to only treat files matching a certain pattern.
#'
#' @param unit The unit upon which to base the categorising.
#' @param pattern If set, only files with names matching this pattern will be
#'   treated.
#' @param dir In which directory do you want to perform this action (defaults
#'   to current)?
#' @return Invisibly `TRUE` if the operation is successful, if not there will be an
#'   error.
#' @examples
#' setwd(tempdir())
#' dir.create("UnitDirs_test")
#' setwd("UnitDirs_test")
#' files <- c("1litres_1.txt", "1litres_3.txt", "3litres.txt", "5litres_1.txt")
#' file.create(files)
#' unitize_dirs("litres", "\\.txt")
#' setwd("..")
#' dir.remove("UnitDirs_test")
#' @export
unitize_dirs <- function(unit, pattern = NULL, dir = ".") {
  lf <- list.files(pattern = pattern)
  if (!all(str_detect(lf, unit))) {
    stop(paste0("The file names must all contain the word", unit ,"."))
  }
  up_to_first_units <- str_before_nth(lf, unit, 1)
  nums <- vapply(up_to_first_units, nth_number, numeric(1), -1, decimals = TRUE)
  un <- unique(nums)
  for (i in un) {
    files <- lf[nums == i]
    move_files(files, paste0(i, unit))
  }
  invisible(TRUE)
}

#' Create directories if they don't already exist
#'
#' Given the names of (potential) directories, create the ones that do not
#' already exist.
#' @param ... The names of the directories, specified via relative or absolute
#'   paths. Duplicates are ignored.
#' @return Invisibly, a vector with a `TRUE` for each time a directory was
#'   actually created and a `FALSE` otherwise. This vector is named with the
#'   paths of the directories that were passed to the function.
#' @examples
#' \dontrun{
#' create_dir(c("mydir", "yourdir"))
#' remove_dir(c("mydir", "yourdir"))}
#' @export
create_dir <- function(...) {
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
#' \dontrun{
#' sapply(c("mydir1", "mydir2"), dir.create)
#' remove_dir(c("mydir1", "mydir2"))}
#' @export
remove_dir <- function(...) {
  dirs <- unlist(...)
  exist <- dir.exists(dirs)
  outcome <- !as.logical(purrr::map_int(dirs, unlink, recursive = TRUE))
  outcome[!exist] <- FALSE
  outcome %>% {
    paste(sum(.), ifelse(sum(.) == 1, "directory", "directories"),
          "deleted.", sum(!.), "failed to delete.")
  } %>% message()
  invisible(outcome)
}

#' @rdname remove_dir
#' @export
dir.remove <- remove_dir

moved_file_new_path <- function(file, destination) {
  # This function does not move any files, it helps `move_files()` which does
  file %<>% normalizePath()  # get full path
  file_name_base <- basename(file)
  destination %<>% {suppressWarnings(normalizePath(.))}  # replace tildes
  paste0(destination, "/", file_name_base)
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
#' @param overwrite Allow overwriting of files? Default no.
#' @return Invisibly, a logical vector with a `TRUE` for each time the operation
#'   succeeded and a `FALSE` for every fail.
#' @examples
#' \dontrun{
#' dir.create("dir")
#' files <- c("1litres_1.txt", "1litres_30.txt", "3litres_5.txt")
#' file.create(files)
#' file.move(files, "dir")}
#' @export
move_files <- function(files, destinations, overwrite = FALSE) {
  checkmate::assert_character(files)
  checkmate::assert_character(destinations)
  checkmate::assert_flag(overwrite)
  if (filesstrings::all_equal(files, character(0))) {
    message("0 files moved. 0 failed.")
    return(invisible(logical(0)))
  }
  checkmate::assert_file_exists(files)
  checkmate::assert_character(destinations, min.chars = 1)
  anydup_files <- anyDuplicated(files)
  if (anydup_files) {
    stop("`files` must not have any duplicated elements.", "\n",
         "    * Element ", anydup_files, " of `files` is a duplicate.")
  }
  if (length(destinations) == 1) destinations %<>% rep(length(files))
  if (length(destinations) != length(files)) {
    stop("The number of destinations must be equal to 1 or equal to the ",
         "number of files to be moved.")
  }
  n_created_dirs <- sum(suppressMessages(create_dir(destinations)))
  if (n_created_dirs > 0) {
    message(n_created_dirs, " ", "director",
            ifelse(n_created_dirs == 1, "y", "ies"),
            " created.")
  }
  n_files <- length(files)
  overwrite_attempt <- FALSE
  out <- rep(FALSE, n_files)
  new_paths <- moved_file_new_path(files, destinations)
  for (i in seq_len(n_files)) {
    if (file.exists(new_paths[i])) {
      overwrite_attempt <- TRUE
      if (overwrite) {
        file.rename(files[i], new_paths[i])
        out[i] <- TRUE
      }
    } else {
      file.rename(files[i], new_paths[i])
      out[i] <- TRUE
    }
  }
  n_succeeded <- sum(out)
  n_failed <- sum(!out)
  message(n_succeeded, ifelse(n_succeeded == 1, " file", " files"),
          " moved. ", n_failed, " failed.")
  if (sum(!out) && !overwrite && overwrite_attempt) {
    message("Some files failed to move because it would have caused files ",
            "to be overwritten. ", "\n",
            "    * To allow overwriting, use `overwrite = TRUE`.")
  }
  invisible(out)
}

#' @rdname move_files
#' @export
file.move <- move_files

#' Make file numbers comply with alphabetical order
#'
#' If files are numbered, their numbers may not *comply* with alphabetical
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
#' \dontrun{
#' dir.create("NiceFileNums_test")
#' setwd("NiceFileNums_test")
#' files <- c("1litres_1.txt", "1litres_30.txt", "3litres_5.txt")
#' file.create(files)
#' nice_file_nums()
#' nice_file_nums(pattern = "\\.txt$")
#' setwd("..")
#' dir.remove("NiceFileNums_test")}
#' @export
nice_file_nums <- function(dir = ".", pattern = NA) {
  init_dir <- getwd()
  on.exit(setwd(init_dir))
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
#'   pattern will be treated.
#' @param replacement What do you want to replace the spaces with? This
#'   defaults to nothing, another sensible choice would be an underscore.
#' @return A logical vector indicating which operation succeeded for each of the
#'   files attempted. Using a missing value for a file or path name will always
#'   be regarded as a failure.
#' @examples
#' \dontrun{
#' dir.create("RemoveFileNameSpaces_test")
#' setwd("RemoveFileNameSpaces_test")
#' files <- c("1litres 1.txt", "1litres 30.txt", "3litres 5.txt")
#' file.create(files)
#' remove_filename_spaces()
#' list.files()
#' setwd("..")
#' dir.remove("RemoveFileNameSpaces_test")}
#' @export
remove_filename_spaces <- function(dir = ".", pattern = "", replacement = "") {
  init_dir <- getwd()
  on.exit(setwd(init_dir))
  setwd(dir)
  lf <- list.files(pattern = pattern)
  new_names <- str_replace_all(lf, " ", replacement)
  n_to_rename <- length(setdiff(lf, new_names))
  outcome <- logical()
  if (n_to_rename) {
    outcome <- file.rename(lf, new_names)
    if (sum(outcome) != n_to_rename) {
      stop("Failed to rename ", sum(!outcome), " file",
           ifelse(sum(!outcome) == 1, "", "s"), ".")
    }
    message(n_to_rename, " file", ifelse(n_to_rename == 1, "", "s"),
            " required renaming and this was done successfully.")
  } else {
    message("No files required renaming.")
  }
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
#' \dontrun{
#' dir.create("RenameWithNums_test")
#' setwd("RenameWithNums_test")
#' files <- c("1litres 1.txt", "1litres 30.txt", "3litres 5.txt")
#' file.create(files)
#' rename_with_nums()
#' list.files()
#' setwd("..")
#' dir.remove("RenameWithNums_test")}
#' @export
rename_with_nums <- function(dir = ".", pattern = NULL) {
  init_dir <- getwd()
  on.exit(setwd(init_dir))
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
#' @param unit The unit upon which to base the categorizing.
#' @param pattern If set, only files with names matching this pattern will be
#'   treated.
#' @param dir In which directory do you want to perform this action (defaults
#'   to current)?
#' @return Invisibly `TRUE` if the operation is successful, if not there will be an
#'   error.
#' @examples
#' \dontrun{
#' dir.create("UnitDirs_test")
#' setwd("UnitDirs_test")
#' files <- c("1litres_1.txt", "1litres_3.txt", "3litres.txt", "5litres_1.txt")
#' file.create(files)
#' unitize_dirs("litres", "\\.txt")
#' setwd("..")
#' dir.remove("UnitDirs_test")}
#' @export
unitize_dirs <- function(unit, pattern = NULL, dir = ".") {
  lf <- list.files(pattern = pattern)
  if (!all(str_detect(lf, unit))) {
    stop(paste0("The file names must all contain the word", unit ,"."))
  }
  up_to_first_units <- str_before_first(lf, unit)
  nums <- purrr::map_dbl(up_to_first_units, last_number, decimals = TRUE)
  un <- unique(nums)
  for (i in un) {
    files <- lf[nums == i]
    move_files(files, paste0(i, unit))
  }
  invisible(TRUE)
}

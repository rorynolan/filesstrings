#' Create a directory if it doesn't already exist.
#'
#' Given the name of a (potential) directory, if that directory does not already
#' exist, create it.
#' @param dir.names The name of the directories, specified via relative or
#'   absolute paths.
#' @return Invisibly, a vector with a `TRUE` for each time a directory was
#'   actually created and a `FALSE` otherwise.
#' @examples
#' CreateDirsIfNotThere(c("mydir", "yourdir"))
#' RemoveDirs(c("mydir", "yourdir"))
#' @export
CreateDirsIfNotThere <- function(dir.names) {
  created <- vapply(dir.names,
                    function(dir.name) {
                      if (!dir.exists(dir.name)) {
                        dir.create(dir.name)
                        TRUE
                      } else {
                        FALSE
                      }
                    }
  , logical(1))
}

#' Remove directories.
#'
#' Delete directories and all of their contents (can just be one directory).
#' @param dirs The names of the directories, specified via relative or absolute
#'   paths.
#' @return Invisibly, a logical vector with `TRUE` for each success and
#'   `FALSE` for failures. See [base::unlink].
#' @examples
#' sapply(c("mydir1", "mydir2"), dir.create)
#' RemoveDirs(c("mydir1", "mydir2"))
#' @export
RemoveDirs <- function(dirs) {
  !vapply(dirs, function(dir) unlink(dir, recursive = TRUE), integer(1)) %>%
    invisible
}

#' Merge Tables.
#'
#' Merge tables saved on disk as delimited files. The merging is done lengthways
#' so they need to have the same number of columns and the same column names (if
#' they have column names).
#'
#' @param file.names The paths to the files to merge.
#' @param delim Delimeter used to separate values.
#' @param out.name The path to the output file containing the merged tables.
#' @param header Do the tables to be merged have headers?
#' @examples
#' dir.create("MergeTables_test")
#' setwd("MergeTables_test")
#' tab1 <- tibble::tibble(x = 1, y = 2)
#' tab2 <- tibble::tibble(x = 1, y = 29)
#' mapply(readr::write_csv, list(tab1, tab2), paste0(c("tab1", "tab2"), ".csv"))
#' MergeTables(c("tab1.csv", "tab2.csv"), ",", "merged.csv")
#' readr::read_csv("merged.csv")
#' setwd("..")
#' RemoveDirs("MergeTables_test")
#' @export
MergeTables <- function(file.names, delim, out.name, header = TRUE) {
  tables <- lapply(file.names, readr::read_delim, delim, col_names = header)
  ncs <- vapply(tables, ncol, integer(1))
  if (!AllEqual(ncs)) stop("The tables have different numbers of columns.")
  if (header) {
    namess <- vapply(tables, names, character(ncs[1]))
    if (!all(apply(namess, 1, AllEqual))) {
      stop("Tables have different colnames.")
    }
  }
  merged <- Reduce(rbind, tables)
  readr::write_delim(merged, out.name, delim = delim, col_names = header)
}

MoveFile <- function(file, destination) {
  # This also works for directories
  file <- normalizePath(file)  # get full path
  file.name.base <- basename(file)
  destination <- normalizePath(destination)  # remove risk of tilde use
  new.name <- paste0(destination, "/", file.name.base)
  file.rename(file, new.name)
}

#' Move files around.
#'
#' Move specified files into specified directories.
#'
#' If there are \eqn{n} files, there must be either \eqn{1} or \eqn{n}
#' directories. If there is one directory, then all \eqn{n} files are moved
#' there. If there are \eqn{n} directories, then each file is put into its
#' respective directory. This function also works for directories.
#' @param files A character vector of files to move (relative or absolute
#'   paths).
#' @param destinations A character vector of the destination directories into
#'   which to move the files.
#' @return A logical vector with a `TRUE` for each time the operation
#'   succeeded and a `FALSE` for every fail.
#' @export
MoveFiles <- function(files, destinations) {
  if(length(destinations) == length(files)) {
    mapply(MoveFile, files, destinations)
  } else if (length(destinations) == 1) {
    vapply(files, MoveFile, logical(1), destinations)
  } else {
    stop("the number of destinations must be equal to 1 or equal to the ",
         "number of files to be moved")
  }
}

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
#' function wraps [NiceNums()], which does the string operations, but
#' not the renaming. To see examples of how this function works, see the
#' examples in that function's documentation.
#'
#' @param dir Path (relative or absolute) to the directory in which to do the
#'   renaming (default is current working directory).
#' @param pattern A regular expression. If specified, files to be renamed are
#'   restricted to ones matching this pattern (in their name).
#' @return A logical vector with a `TRUE` for each successful rename
#'   (should be all TRUEs) and `FALSE`s otherwise.
#'
#' @examples
#' dir.create("NiceFileNums_test")
#' setwd("NiceFileNums_test")
#' files <- c("1litres_1.txt", "1litres_30.txt", "3litres_5.txt")
#' file.create(files)
#' NiceFileNums()
#' NiceFileNums(pattern = "\\.txt$")
#' setwd("..")
#' RemoveDirs("NiceFileNums_test")
#' @export
NiceFileNums <- function(dir = ".", pattern = NA) {
  init.dir <- getwd()
  on.exit(setwd(init.dir))
  setwd(dir)
  if (is.na(pattern)) {
    lf <- list.files()
  } else {
    lf <- list.files(pattern = pattern)
  }
  renamed <- file.rename(lf, NiceNums(lf))
  renamed
}

#' Put files in a directory
#'
#' Move files to a directory, creating the directory if it does not already
#' exist.
#' @param file.names character vector of file names, relative or absolute paths
#' @param dir.name The name of the directory into which to move the files,
#'   relative or absolute path
#' @return A logical vector with a `TRUE` for each successful file move and
#'   a `FALSE` otherwise.
#' @export
PutFilesInDir <- function(file.names, dir.name) {
  CreateDirsIfNotThere(dir.name)
  MoveFiles(file.names, dir.name)
}

#' Remove spaces in file names
#'
#' Remove spaces in file names in a specified directory, replacing them with
#' whatever you want, default nothing.
#'
#' @param dir The directory in which to perform the operation.
#' @param pattern A regular expression. If specified, only files matching this
#'   pattern wil be treated.
#' @param replace.with What do you want to replace the spaces with? This
#'   defaults to nothing, another sensible choice would be an underscore.
#' @return A logical vector indicating which operation succeeded for each of the
#'   files attempted. Using a missing value for a file or path name will always
#'   be regarded as a failure.
#' @examples
#' dir.create("RemoveFileSpaces_test")
#' setwd("RemoveFileSpaces_test")
#' files <- c("1litres 1.txt", "1litres 30.txt", "3litres 5.txt")
#' file.create(files)
#' RemoveFileSpaces()
#' list.files()
#' setwd("..")
#' RemoveDirs("RemoveFileSpaces_test")
#' @export
RemoveFileSpaces <- function(dir = ".", pattern = "", replace.with = "") {
  init.dir <- getwd()
  on.exit(setwd(init.dir))
  setwd(dir)
  lf <- list.files(pattern = pattern)
  new.names <- str_replace_all(lf, " ", replace.with)
  success <- file.rename(lf, new.names)
  success
}

#' Replace file names with numbers.
#'
#' Rename the files in the directory, replacing file names with numbers only.
#' @param dir The directory in which to rename the files (relative or absolute
#'   path). Defaults to current working directory.
#' @param pattern A regular expression. If specified, only files with names
#'   matching this pattern will be treated.
#' @return A logical vector with a `TRUE` for each successful renaming.
#' @examples
#' dir.create("RenameWithNums_test")
#' setwd("RenameWithNums_test")
#' files <- c("1litres 1.txt", "1litres 30.txt", "3litres 5.txt")
#' file.create(files)
#' RenameWithNums()
#' list.files()
#' setwd("..")
#' RemoveDirs("RenameWithNums_test")
#' @export
RenameWithNums <- function(dir = ".", pattern = NULL) {
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
  new.names <- NiceNums(paste0(seq_len(l), ".", ext))
  if (any(new.names %in% lf)) {
    stop("Some of the names are already in the desired format, ",
         "unable to proceed as renaming may result in deletion.")
  }
  file.rename(lf, new.names)
}

#' Put files with the same unit measurements into directories.
#'
#' Say you have a number of files with "5min" in their names, number with
#' "10min" in the names, a number with "15min" in their names and so on, and
#' you'd like to put them into directories named "5min", "10min", "15min" and so
#' on. This function does this, but not just for the unit "min", for any unit.
#'
#' This function takes the number to be the last number (as defined in
#' [NthNumber()]) before the first occurrence of the unit name. There
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
#' # Keep an eye on your current working directory and the
#' # directory "UnitDirs_test" as you run the following code
#' dir.create("UnitDirs_test")
#' setwd("UnitDirs_test")
#' files <- c("1litres_1.txt", "1litres_3.txt", "3litres.txt", "5litres_1.txt")
#' file.create(files)
#' UnitDirs("litres", "\\.txt")
#' setwd("..")
#' RemoveDirs("UnitDirs_test")
#' @export
UnitDirs <- function(unit, pattern = NULL, dir = ".") {
  lf <- list.files(pattern = pattern)
  if (!all(str_detect(lf, unit))) {
    stop(paste0("The file names must all contain the word", unit ,"."))
  }
  up.to.first.units <- StrBeforeNth(lf, unit, 1)
  nums <- vapply(up.to.first.units, NthNumber, numeric(1), -1, decimals = TRUE)
  un <- unique(nums)
  for (i in un) {
    files <- lf[nums == i]
    PutFilesInDir(files, paste0(i, unit))
  }
  invisible(TRUE)
}

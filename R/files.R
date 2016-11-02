#' Create a directory if it doesn't already exist.
#'
#' Given the name of a (potential) directory, if that directory does not already
#' exist, create it.
#' @param dir.name The name of the directory, specified via relative or absolute
#'   path.
#' @return Invisibly, a vector with a \code{TRUE} for each time a directory was
#'   actually created and a \code{FALSE} otherwise.
#' @examples
#' CreateDirsIfNotThere("mydir")
#' CreateDirsIfNotThere("/home/mydir")
#' @export
CreateDirsIfNotThere <- function(dir.names) {
  created <- sapply(dir.names,
                    function(dir.name) {
                      if (!dir.exists(dir.name)) {
                        dir.create(dir.name)
                        TRUE
                      } else {
                        FALSE
                      }
                    }
  )
}

#' Remove directories.
#'
#' Delete directories and all of their contents (can just be one directory).
#' @param dir.names The names of the directories, specified via relative or
#'   absolute paths.
#' @return Logical vector with \code{TRUE} for each success and \code{FALSE} for
#'   failures.
#' @examples
#' RemoveDirs("mydir1", "mydir2")
#' RemoveDirs("/home/mydir")
#' @export
RemoveDirs <- function(dirs) {
  success <- !sapply(dirs, function(dir) unlink(dir, recursive = T))
  success
}

#' Merge Tables.
#'
#' Merge tables saved on disk as delimited files.
#'
#' @param file.names The paths to the files to merge.
#' @param delim Delimeter used to separate values.
#' @param out.name The path to the output file containing the merged tables.
#' @param header Do the tables to be merged have headers?
#' @export
MergeTables <- function(file.names, delim, out.name, header = T) {
  tables <- lapply(file.names, readr::read_delim, delim, col_names = header)
  ncs <- sapply(tables, ncol)
  if (!AllEqual(ncs)) stop("The tables have different numbers of columns.")
  if (header) {
    namess <- sapply(tables, names)
    if (!all(apply(namess, 1, AllEqual))) {
      stop("Tables have different colnames.")
    }
  }
  merged <- Reduce(rbind, tables)
  readr::write_delim(merged, file = out.name, delim = delim, col_names = header)
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
#' @return A logical vector with a \code{TRUE} for each time the operation
#'   succeeded and a \code{FALSE} for every fail.
#' @export
MoveFiles <- function(files, destinations) {
  if(length(destinations) == length(files)) {
    mapply(MoveFile, files, destinations)
  } else if (length(destinations) == 1) {
    sapply(files, MoveFile, destinations)
  } else {
    stop("the number of destinations must be equal to 1 or equal to the number of files to be moved")
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
#' function wraps \code{\link{NiceNums}}, which does the string operations, but
#' not the renaming. To see examples of how this function works, see the
#' examples in that function's documentation.
#'
#' @param dir Path (relative or absolute) to the directory in which to do the
#'   renaming (default is current working directory).
#' @param pattern A regular expression. If specified, files to be renamed are
#'   restricted to ones matching this pattern (in their name).
#' @return A logical vector with a \code{TRUE} for each successful rename
#'   (should be all TRUEs) and \code{FALSE}s otherwise.
#' @export
NiceFileNums <- function(dir = ".", pattern = NA) {
  init.dir <- getwd()
  setwd(dir)
  if (is.na(patt)) {
    lf <- list.files()
  } else {
    lf <- list.files(pattern = pattern)
  }
  renamed <- file.rename(lf, NiceNums(lf))
  setwd(init.dir)
  renamed
}

#' Put files in a directory
#'
#' Move files to a directory, creating the directory if it does not already
#' exist.
#' @param file.names character vector of file names, relative or absolute paths
#' @param dir.name The name of the directory into which to move the files,
#'   relative or absolute path
#' @return A logical vector with a \code{TRUE} for each successful file move and
#'   a \code{FALSE} otherwise.
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
#' @return A logical vector indicating which operation succeeded for each of the
#'   files attempted. Using a missing value for a file or path name will always
#'   be regarded as a failure.
#' @export
RemoveFileSpaces <- function(dir = ".", pattern = "", replace.with = "") {
  init.dir <- getwd()
  setwd(dir)
  lf <- list.files(pattern = pattern)
  new.names <- str_replace_all(lf, " ", replace.with)
  success <- file.rename(lf, new.names)
  setwd(init.dir)
  success
}

#' Replace file names with numbers.
#'
#' Rename the files in the directory, replacing file names with numbers only.
#' @param dir The directory in which to rename the files (relative or absolute
#'   path). Defaults to current working directory.
#' @param pattern A regular expression. If specified, only files with names
#'   matching this pattern will be treated.
#' @return A logical vector with a \code{TRUE} for each successful renaming.
#' @export
RenameWithNums <- function(dir = ".", pattern = NULL) {
  init.dir <- getwd()
  setwd(dir)
  lf <- list.files(pattern = pattern)
  l <- length(lf)
  if (l == 0) stop("No files found to rename.")
  new.names <- NiceNums(paste0(1:l, ext))
  if (any(new.names %in% lf)) stop("Some of the names are already in the desired format, unable to proceed as renaming may result in deletion.")
  success <- file.rename(lf, new.names)
  setwd(init.dir)
  success
}

#' Put files with the same unit measurements into directories.
#'
#' Say you have a number of files with "5min" in their names, number with
#' "10min" in the names, a number with "15min" in their names and so on, and
#' you'd like to put them into directories named "5min", "10min", "15min" and so
#' on. This function does this, but not just for the unit "min", for any unit.
#'
#' This function takes the number to be the last number (as defined in
#' \code{\link{LastNumber}}) before the first occurrence of the unit name. There
#' is the option to only treat files matching a certain pattern.
#'
#' @param unit The unit upon which to base the categorising.
#' @param pattern If set, only files with names matching this pattern will be
#'   treated.
#' @return \code{TRUE} if the operation is successful, if not there will be an
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
  nums <- sapply(up.to.first.units, NthNumber, -1, decimals = TRUE)
  un <- unique(nums)
  for (i in un) {
    print(lf)
    files <- lf[nums == i]
    print(files)
    PutFilesInDir(files, paste0(i, unit))
  }
  TRUE
}

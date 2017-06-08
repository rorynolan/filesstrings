#' @param dir.names The name of the directories, specified via relative or
#'   absolute paths.
#' @rdname filesstrings-deprecated
#' @export
CreateDirsIfNotThere <- function(dir.names) {
  .Deprecated("create_dirs")
  create_dirs(dir.names)
}

#' @param dirs The names of the directories, specified via relative or absolute
#'   paths.
#' @return Invisibly, a logical vector with `TRUE` for each success and
#'   `FALSE` for failures. See [base::unlink].
#' @rdname filesstrings-deprecated
#' @export
RemoveDirs <- function(dirs) {
  .Deprecated("remove_dirs")
  remove_dirs(dirs)
}

#' @param file.names The paths to the files to merge.
#' @param delim Delimeter used to separate values.
#' @param out.name The path to the output file containing the merged tables.
#' @param header Do the tables to be merged have headers?
#' @param ... Additional arguments passed to [readr::read_delim].
#' @rdname filesstrings-deprecated
#' @export
MergeTablesOnDisk <- function(file.names, delim, out.name, header = TRUE, ...) {
  .Deprecated("merge_tables_on_disk")
  merge_tables_on_disk(file.names, delim, out.name, header = TRUE, ...)
}

MoveFile <- function(file, destination) {
  # This also works for directories
  file <- normalizePath(file)  # get full path
  file.name.base <- basename(file)
  destination <- normalizePath(destination)  # remove risk of tilde use
  new.name <- paste0(destination, "/", file.name.base)
  file.rename(file, new.name)
}

#' @param destinations A character vector of the destination directories into
#'   which to move the files.
#' @rdname filesstrings-deprecated
#' @export
MoveFiles <- function(files, destinations) {
  .Deprecated("move_files")
  move_files(files, destinations)
}

#' @rdname filesstrings-deprecated
#' @export
NiceFileNums <- function(dir = ".", pattern = NA) {
  .Deprecated("nice_file_nums")
  nice_file_nums(dir, pattern)
}

#' @param replace.with What do you want to replace the spaces with? This
#'   defaults to nothing, another sensible choice would be an underscore.
#' @rdname filesstrings-deprecated
#' @export
RemoveFileNameSpaces <- function(dir = ".", pattern = "", replace.with = "") {
  .Deprecated("remove_filename_spaces")
  remove_filename_spaces(dir, pattern, replace.with)
}

#' @param dir The directory in which to rename the files (relative or absolute
#'   path). Defaults to current working directory.
#' @rdname filesstrings-deprecated
#' @export
RenameWithNums <- function(dir = ".", pattern = NULL) {
  .Deprecated("rename_with_nums")
  rename_with_nums(dir, pattern)
}

#' @param unit The unit upon which to base the categorising.
#' @rdname filesstrings-deprecated
#' @export
UnitDirs <- function(unit, pattern = NULL, dir = ".") {
  .Deprecated("unitize_dirs")
  unitize_dirs(unit, pattern, dir)
}

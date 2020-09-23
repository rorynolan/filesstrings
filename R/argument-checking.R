argchk_move_files <- function(files, destinations, overwrite) {
  checkmate::assert_character(files)
  checkmate::assert_character(destinations)
  checkmate::assert_flag(overwrite)
  checkmate::assert_file_exists(files)
  checkmate::assert_character(destinations, min.chars = 1)
  anydup_files <- anyDuplicated(files)
  if (anydup_files) {
    stop(
      "`files` must not have any duplicated elements.", "\n",
      "    * Element ", anydup_files, " of `files` is a duplicate."
    )
  }
  if (length(destinations) == 1) destinations %<>% rep(length(files))
  if (length(destinations) != length(files)) {
    stop(
      "The number of destinations must be equal to 1 or equal to the ",
      "number of files to be moved."
    )
  }
  list(files = files, destinations = destinations, overwrite = overwrite)
}

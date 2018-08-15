#' Ensure a file name has the intended extension.
#'
#' Say you want to ensure a name is fit to be the name of a csv file. Then, if
#' the input doesn't end with ".csv", this function will tack ".csv" onto the
#' end of it. This is vectorized over the first argument.
#'
#' @param string The intended file name.
#' @param ext The intended file extension (with or without the ".").
#' @param replace If the file has an extension already, replace it (or append
#'   the new extension name)?
#'
#' @return A string: the file name in your intended form.
#'
#' @examples
#' give_ext(c("abc", "abc.csv"), "csv")
#' give_ext("abc.csv", "pdf")
#' give_ext("abc.csv", "pdf", replace = TRUE)
#' @export
give_ext <- function(string, ext, replace = FALSE) {
  stopifnot(is.character(string), length(ext) == 1)
  ext <- str_match(ext, "^\\.*(.*)")[, 2]
  if (replace) {
    string <- tools::file_path_sans_ext(string)
  } else {
    correct_ext <- str_detect(string, str_c("\\.", ext, "$"))
    string[correct_ext] <- tools::file_path_sans_ext(string[correct_ext])
  }
  str_c(string, ".", ext)
}

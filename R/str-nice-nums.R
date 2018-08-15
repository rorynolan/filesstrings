#' Make string numbers comply with alphabetical order
#'
#' If strings are numbered, their numbers may not *comply* with
#' alphabetical order, i.e. "abc2" comes after "abc10" in alphabetical order. We
#' might (for whatever reason) wish to change them such that they come in the
#' order *that we would like*. This function alters the strings such that
#' they comply with alphabetical order, so here "abc2" would be renamed to
#' "abc02". It works on file names with more than one number in them e.g.
#' "abc01def3" (a string with 2 numbers). All the file names that it works on
#' must have the same number of numbers, and the non-number bits must be the
#' same.
#'
#' @param strings A vector of strings.
#' @examples
#' strings <- paste0("abc", 1:12)
#' strings
#' nice_nums(strings)
#'
#' nice_nums(c("abc9def55", "abc10def7"))
#' nice_nums(c("01abc9def55", "5abc10def777", "99abc4def4"))
#' nice_nums(1:10)
#'
#' \dontrun{
#' nice_nums(c("abc9def55", "abc10xyz7"))}
#' @export
nice_nums <- function(strings) {
  checkmate::assert(checkmate::check_numeric(strings),
                    checkmate::check_character(strings))
  if (is.numeric(strings)) strings %<>% as.character()
  have_nums <- str_detect(strings, "\\d")
  if (!all(have_nums)) {
    stop("Some of the input strings have no numbers in them.")
  }
  non_nums <- extract_non_numerics(strings)
  if (!all_equal(non_nums)) {
    stop("The non-number bits of the strings are different.")
  }
  nums <- extract_numbers(strings, leave_as_string = TRUE)
  if (!all_equal(lengths(nums))) {
    stop("Some of the strings contain different numbers of numbers")
  }
  nums <- simplify2array(nums)
  if (!is.matrix(nums)) nums <- t(nums)
  ncn <- nchar(nums)
  max_lengths <- matrixStats::rowMaxs(ncn)
  min_length <- min(ncn)
  to_prefix <- rep("0", max(max_lengths) - min_length) %>% str_c(collapse = "")
  nums <- str_c(to_prefix, nums)
  starts <- -rep(max_lengths, ncol(ncn))
  nums <- str_sub(nums, starts, -1) %>%
    split(rep(seq_len(ncol(ncn)), each = nrow(ncn)))
  num_first <- str_elem(strings, 1) %>% can_be_numeric
  if (!all_equal(num_first)) {
    stop("Some strings start with numbers and some don't")
  }
  if (num_first[1]) {
    interleaves <- interleave_char_lists(nums, non_nums)
  } else {
    interleaves <- interleave_char_lists(non_nums, nums)
  }
  paste_collapse_list_elems(interleaves)
}

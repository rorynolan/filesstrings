nb_get_img <- function(img) {
  if (is.character(img)) {
    if (length(img) != 1) {
      custom_stop(
        "If 'img' is specified as a character vector (i.e. a file name),
        it must have length 1.
        ",
        "Your `img` is a character vector of length {length(img)}."
      )
    }
    img %<>% ijtiff::read_tif()
  }
  checkmate::assert_array(img, min.d = 3, max.d = 4)
  checkmate::assert_numeric(img, lower = 0)
  if (!isTRUE(all.equal(img, floor(img), check.attributes = FALSE))) {
    img[is.na(img)] <- 0
    bad_index <- match(TRUE, as.vector(img) != as.vector(floor(img)))
    custom_stop(
      "`img` must be positive integers (and NAs) only.",
      "
      Element {bad_index} of `img` is {img[bad_index]} which is neither NA
      nor positive integer.
      "
    )
  }
  img
}

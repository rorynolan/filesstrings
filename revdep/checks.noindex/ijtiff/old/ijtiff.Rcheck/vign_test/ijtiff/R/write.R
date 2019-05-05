#' Write images in TIFF format
#'
#' Write images into a TIFF file.
#'
#' @inheritParams ijtiff_img
#' @param path file name or a raw vector
#' @param bits_per_sample number of bits per sample (numeric scalar). Supported
#'   values are 8, 16, and 32. The default `"auto"` automatically picks the
#'   smallest workable value based on the maximum element in `img`. For example,
#'   if the maximum element in `img` is 789, then 16-bit will be chosen because
#'   789 is greater than 2 ^ 8 - 1 but less than or equal to 2 ^ 16 - 1.
#' @param compression A string, the desired compression algorithm. Must be one
#'   of `"none"`, `"LZW"`, `"PackBits"`, `"RLE"`, `"JPEG"`, `"deflate"` or
#'   `"Zip"`. If you want compression but don't know which one to go for, I
#'   recommend `"Zip"`, it gives a large file size reduction and it's lossless.
#'   Note that `"deflate"` and `"Zip"` are the same thing. Avoid using `"JPEG"`
#'   compression in a TIFF file if you can; I've noticed it can be buggy.
#' @param overwrite If writing the image would overwrite a file, do you want to
#'   proceed?
#' @param msg Print an informative message about the image being written?
#'
#' @return The input `img` (invisibly).
#'
#' @author Simon Urbanek wrote most of this code for the 'tiff' package. Rory
#'   Nolan lifted it from there and changed it around a bit for this 'ijtiff'
#'   package. Credit should be directed towards Lord Urbanek.
#' @seealso [read_tif()]
#' @examples
#'
#' img <- read_tif(system.file("img", "Rlogo.tif", package="ijtiff"))
#' temp_dir <- tempdir()
#' write_tif(img, paste0(temp_dir, "/", "Rlogo"))
#' img <- matrix(1:4, nrow = 2)
#' write_tif(img, paste0(temp_dir, "/", "tiny2x2"))
#' list.files(temp_dir, pattern = "tif$")
#'
#' @export
write_tif <- function(img, path, bits_per_sample = "auto",
                      compression = "none", overwrite = FALSE, msg = TRUE) {
  checkmate::assert_string(path)
  path %<>% stringr::str_replace_all(stringr::coll("\\"), "/") # windows safe
  if (stringr::str_detect(path, "/")) { # I've noticed that write_tif()
    init_wd <- getwd() # sometimes fails when writing to
    on.exit(setwd(init_wd)) # far away directories.
    tiff_dir <- filesstrings::str_before_last(path, "/")
    checkmate::assert_directory_exists(tiff_dir)
    setwd(tiff_dir)
    path %<>% filesstrings::str_after_last("/")
  }
  to_invisibly_return <- img
  checkmate::assert_scalar(bits_per_sample)
  checkmate::assert(
    checkmate::check_string(bits_per_sample),
    checkmate::check_int(bits_per_sample)
  )
  if (isTRUE(checkmate::check_string(bits_per_sample))) {
    if (startsWith("auto", tolower(bits_per_sample))) {
      bits_per_sample <- "auto"
    } else {
      custom_stop("
         If `bits_per_sample` is a string, then 'auto' is the only
         allowable value.
        ",
        "You have used '{bits_per_sample}'."
      )
    }
  } else {
    if (!bits_per_sample %in% c(8, 16, 32)) {
      custom_stop(
        "If specifying `bits_per_sample`, it must be one of 8, 16 or 32.",
        "You have used `bits_per_sample = {bits_per_sample}`.")
    }
  }
  checkmate::assert_string(compression)
  if (endsWith(tolower(path), ".tiff") || endsWith(tolower(path), ".tif")) {
    path <- paste0(filesstrings::before_last_dot(path), ".tif")
  }
  path %<>% filesstrings::give_ext("tif")
  if (file.exists(path) && overwrite == FALSE) {
    custom_stop(
      "The file {path}, already exists and `overwrite` is set to `FALSE`.",
      "To enable overwriting, use `overwrite = TRUE`."
    )
  }
  checkmate::assert_array(img)
  checkmate::assert_array(img, min.d = 2, max.d = 4)
  checkmate::assert_numeric(img)
  img %<>% ijtiff_img()
  d <- dim(img)
  compressions <- c(
    none = 1L, RLE = 2L, LZW = 5L, PackBits = 32773L, JPEG = 7L,
    deflate = 8L, Zip = 8L
  )
  compression %<>% filesstrings::match_arg(names(compressions),
    ignore_case = TRUE
  ) %>% {
    compressions[match(., names(compressions))]
  }
  floats <- FALSE
  if (anyNA(img)) floats <- TRUE
  if (!floats) if (!filesstrings::all_equal(img, floor(img))) floats <- TRUE
  if (!floats) {
    if (any(img < 0)) {
      if (min(img) < -float_max()) {
        custom_stop(
          "The lowest allowable negative value in `img` is {-float_max()}.",
          "The lowest value in your `img` is {min(img)}.",
          "
           The `write_txt_img()` function allows you to write images without
           restriction on the values therein. Maybe you should try that?
          "
        )
      }
      if (max(img) > float_max()) {
        custom_stop("
           If `img` has negative values (which the input `img` does),
           then the maximum allowed positive value is {float_max()}.
          ",
          "The largest value in your `img` is {max(img)}.",
          "
           The `write_txt_img()` function allows you to write images without
           restriction on the values therein. Maybe you should try that?
          "
        )
      }
      floats <- TRUE
    }
  }
  if (is.integer(img)) {
    img %<>% as.numeric() # The C function needs img to be numeric
    dim(img) <- d
  }
  if (floats) {
    checkmate::assert_numeric(img,
      lower = -float_max(),
      upper = float_max()
    )
    if (bits_per_sample == "auto") bits_per_sample <- 32
    if (bits_per_sample != 32) {
      custom_stop("
         Your image needs to be written as floating point numbers
         (not integers). For this, it is necessary to have 32 bits per
         sample.
        ",
        "You have selected {bits_per_sample} bits per sample."
      )
    }
  } else {
    ideal_bps <- 8
    mx <- floor(max(img))
    if (mx > 2^32 - 1) {
      custom_stop("
         The maximum value in 'img' is {mx} which is greater than 2 ^ 32 - 1 and
         therefore too high to be written to a TIFF file.
        ",
        "The `write_txt_img()` function allows you to write images without
         restriction on the values therein. Maybe you should try that?
        "
      )
    } else if (mx > 2^16 - 1) {
      ideal_bps <- 32
    } else if (mx > 2^8 - 1) {
      ideal_bps <- 16
    }
    if (bits_per_sample == "auto") bits_per_sample <- ideal_bps
    if (bits_per_sample < ideal_bps) {
      custom_stop("
         You are trying to write a {bits_per_sample}-bit image,
         however the maximum element in `img` is {mx}, which is too big.
        ", "
         The largest allowable value in a {bits_per_sample}-bit image is
         {2 ^ bits_per_sample -1}.
        ", "
         To write your `img` to a TIFF file, you need at least {ideal_bps}
         bits per sample.
        "
      )
    }
  }
  if (msg) {
    bps <- bits_per_sample %>% {
      dplyr::case_when(
        . == 8 ~ "an 8-bit, ",
        . == 16 ~ "a 16-bit, ",
        . == 32 ~ "a 32-bit, ",
        TRUE ~ "a 0-bit, "
      )
    }
    pretty_msg(
      "Writing ", path, ": ", bps, d[1], "x", d[2], " pixel image of ",
      ifelse(floats, "floating point", "unsigned integer"),
      " type with ", d[3],
      " ", "channel", ifelse(d[3] > 1, "s", ""), " and ",
      d[4], " frame", ifelse(d[4] > 1, "s", ""), " . . ."
    )
  }
  what <- enlist_img(img)
  written <- .Call("write_tif_C", what, path, bits_per_sample, compression,
    floats,
    PACKAGE = "ijtiff"
  )
  if (msg) pretty_msg("\b Done.")
  invisible(to_invisibly_return)
}

enlist_img <- function(img) {
  checkmate::assert_numeric(img)
  checkmate::assert_array(img, d = 4)
  enlist_img_cpp(img)
}

dims <- function(lst) {
  checkmate::assert_list(lst)
  dims_cpp(lst)
}

enlist_planes <- function(arr) {
  checkmate::assert_array(arr, d = 3)
  purrr::map(seq_len(dim(arr)[3]), ~arr[, , .])
}

extract_desired_plane <- function(arr) {
  checkmate::assert_array(arr, min.d = 2, max.d = 3)
  d <- dim(arr)
  if (length(d) == 3) {
    nonzero_planes <- !purrr::map_lgl(
      seq_len(d[3]),
      ~filesstrings::all_equal(arr[, , .], 0)
    )
    if (sum(nonzero_planes) == 1) {
      arr <- arr[, , nonzero_planes]
    } else if (filesstrings::all_equal(enlist_planes(arr))) {
      arr <- arr[, , 1]
    } else {
      n_nonzero_unique_planes <- arr %>%
        enlist_planes() %>%
        unique() %>%
        length()
      custom_stop(
        "Cannot extract the desired plane.",
        "
                   There are {n_nonzero_unique_planes} unique nonzero planes,
                   so it is impossible to decipher which is the correct one
                   to extract.
                  "
      )
    }
  }
  arr
}

#' Count the number of images in a TIFF file.
#'
#' TIFF files can hold many images. Often this is sensible, e.g. each image
#' could be a time-point in a video or a slice of a z-stack. Sometimes
#' ImageJ-written images have one image per channel per slice.
#'
#' For those familiar with TIFF files, this function counts the number of
#' directories in a TIFF file.
#'
#' @inheritParams read_tif
#'
#' @return A number.
#'
#' @examples
#' count_imgs(system.file("img", "Rlogo.tif", package="ijtiff"))
#' count_imgs(system.file("img", "2ch_ij.tif", package="ijtiff"))
#'
#' @export
count_imgs <- function(path) {
  checkmate::assert_string(path)
  path %<>% stringr::str_replace_all(stringr::coll("\\"), "/") # windows safe
  checkmate::assert_file_exists(path)
  if (stringr::str_detect(path, "/")) {
    init_wd <- setwd(filesstrings::str_before_last(path, "/"))
    on.exit(setwd(init_wd))
    path %<>% filesstrings::str_after_last("/")
    # `read_tif()` sometimes fails when writing to far away directories.
  }
  .Call("count_directories_C", path, PACKAGE = "ijtiff")
}

is_installed <- function(package) {
  checkmate::assert_string(package)
  installed_packages <- utils::installed.packages() %>%
    rownames()
  package %in% installed_packages
}

can_be_intish <- function(x) {
  filesstrings::all_equal(x, floor(x))
}

#' Convert an [ijtiff_img] to an [EBImage::Image].
#'
#' This is for interoperability with the the `EBImage` package.
#'
#' The guess for the `colormode` is made as follows: * If `img` has an attribute
#' `color_space` with value `"RGB"`, then `colormode` is set to `"Color"`. *
#' Else if `img` has 3 or 4 channels, then `colormode` is set to `"Color"`. *
#' Else `colormode` is set to "Grayscale".
#'
#' @param img An [ijtiff_img] object (or something coercible to one).
#' @param colormode A numeric or a character string containing the color mode
#'   which can be either `"Grayscale"` or `"Color"`. If not specified, a guess
#'   is made. See 'Details'.
#' @param scale Scale values in an integer image to the range `[0, 1]`? Has no
#'   effect on floating-point images.
#' @param force This function is designed to take [ijtiff_img]s as input. To
#'   force any old array through this function, use `force = TRUE`, but take
#'   care to check that the result is what you'd like it to be.
#'
#' @return An [EBImage::Image].
#'
#' @examples
#' if (require(EBImage)) {
#'   img <- read_tif(system.file("img", "Rlogo.tif", package = "ijtiff"))
#'   str(img)
#'   str(as_EBImage(img))
#'   img <- read_tif(system.file("img", "2ch_ij.tif", package = "ijtiff"))
#'   str(img)
#'   str(as_EBImage(img))
#' }
#' @export
as_EBImage <- function(img, colormode = NULL, scale = TRUE, force = TRUE) {
  if (!is_installed("EBImage")) {
    stop(paste0(
      "To use this function, you need to have the `EBImage` package ",
      "installed.", "\n", ebimg_install_msg()
    ))
  }
  checkmate::assert_flag(scale)
  checkmate::assert_flag(force)
  if (!methods::is(img, "ijtiff_img")) {
    if (methods::is(img, "Image")) {
      return(img)
    } else {
      if (force) {
        img %<>% ijtiff_img()
      } else {
        custom_stop("
          This function expects the input `img` to be of class 'ijtiff_img',
          however the `img` you have supplied is not.
         ", "
          To force your array through this function, use `force = TRUE`, but
          take care to check that the result is what you'd like it to be.
         ")
      }
    }
  }
  if (is.null(colormode)) {
    if ("color_space" %in% names(attributes(img))) {
      if (attr(img, "color_space") == "RGB") {
        colormode <- "c"
      }
    }
  }
  if (is.null(colormode)) {
    if (dim(img)[3] %in% 3:4) {
      colormode <- "c"
    } else {
      colormode <- "g"
    }
  }
  checkmate::assert_string(colormode)
  if (tolower(colormode) %in% c("g", "gr")) {
    colormode <- "greyscale"
  }
  if (startsWith("colo", tolower(colormode))) {
    colormode <- "colour"
  }
  colormode %<>% filesstrings::match_arg(c(
    "Color", "Colour",
    "Grayscale", "Greyscale"
  ),
  ignore_case = TRUE
  )
  if (colormode == "Colour") colormode <- "Color"
  if (colormode == "Greyscale") colormode <- "Grayscale"
  if (scale && (!all(is.na(img)))) {
    if (can_be_intish(img)) {
      if (all(img < 2^8, na.rm = TRUE)) {
        img %<>% {
          . / (2^8 - 1)
        }
      } else if (all(img < 2^16, na.rm = TRUE)) {
        img %<>% {
          . / (2^16 - 1)
        }
      } else if (all(img < 2^32, na.rm = TRUE)) {
        img %<>% {
          . / (2^32 - 1)
        }
      } else {
        img %<>% {
          . / max(.)
        }
      }
    }
  }
  img %<>% aperm(c(2, 1, 3, 4))
  EBImage::Image(img, colormode = colormode)
}

ebimg_install_msg <- function() {
  paste0(
    "  * To install `EBImage`:", "\n",
    "    - Install `BiocManager` with `install.packages(\"BiocManager\")`.\n",
    "    - Then run `BiocManager::install(\"EBImage\")`."
  )
}

#' Rejig linescan images.
#'
#' `ijtiff` has the fourth dimension of an [ijtiff_img] as its time dimension.
#' However, some linescan images (images where a single line of pixels is
#' acquired over and over) have the time dimension as the y dimension, (to avoid
#' the need for an image stack). These functions allow one to convert this type
#' of image into a conventional [ijtiff_img] (with time in the fourth dimension)
#' and to convert back.
#'
#' @param linescan_img A 4-dimensional array in which the time axis is the first
#'   axis. Dimension 4 must be 1 i.e. `dim(linescan_img)[4] == 1`.
#' @param img A conventional [ijtiff_img], to be turned into a linescan image.
#'   Dimension 1 must be 1 i.e. `dim(img)[1] == 1`.
#'
#' @return The converted image, an object of class [ijtiff_img].
#'
#' @examples
#' linescan <- ijtiff_img(array(rep(1:4, each = 4), dim = c(4, 4, 1, 1)))
#' print(linescan)
#' stack <- linescan_to_stack(linescan)
#' print(stack)
#' linescan <- stack_to_linescan(stack)
#' print(linescan)
#'
#' @name linescan-conversion
NULL

#' @rdname linescan-conversion
#' @export
linescan_to_stack <- function(linescan_img) {
  linescan_img %<>% ijtiff_img()
  if (dim(linescan_img)[4] != 1) {
    custom_stop(
      "
       The fourth dimension of `linescan_img` should be equal to 1
       (or else it's not a linescan image).
      ",
      "Yours has dim(linescan_img)[4] == {dim(linescan_img)[4]}."
    )
  }
  linescan_img %>%
    aperm(c(4, 2, 3, 1)) %>%
    ijtiff_img()
}

#' @rdname linescan-conversion
#' @export
stack_to_linescan <- function(img) {
  img %<>% ijtiff_img()
  if (dim(img)[1] != 1) {
    custom_stop(
      "
       The first dimension of `img` should be equal to 1
       (or else it's not a stack that can be converted to a linescan).
      ",
      "Yours has dim(img)[1] == {dim(img)[1]}."
    )
  }
  img %>%
    aperm(c(4, 2, 3, 1)) %>%
    ijtiff_img()
}

#' Construct the bullet point bits for `custom_stop()`.
#'
#' @param string The message for the bullet point.
#'
#' @return A string with the bullet-pointed message nicely formatted for the
#'   console.
#'
#' @noRd
custom_stop_bullet <- function(string) {
  checkmate::assert_string(string)
  string %<>% strwrap(width = 57)
  string[1] %<>% {
    glue::glue("    * {.}")
  }
  if (length(string) > 1) {
    string[-1] %<>% {
      glue::glue("      {.}")
    }
  }
  glue::glue_collapse(string, sep = "\n")
}

#' Nicely formatted error message.
#'
#' Format an error message with bullet-pointed sub-messages with nice
#' line-breaks.
#'
#' Arguments should be entered as `glue`-style strings.
#'
#' @param main_message The main error message.
#' @param ... Bullet-pointed sub-messages.
#'
#' @noRd
custom_stop <- function(main_message, ..., .envir = parent.frame()) {
  checkmate::assert_string(main_message)
  main_message %<>% glue::glue(.envir = .envir)
  out <- strwrap(main_message, width = 63)
  dots <- unlist(list(...))
  if (length(dots)) {
    if (!is.character(dots)) {
      stop("\nThe arguments in ... must all be of character type.")
    }
    dots %<>% purrr::map_chr(glue::glue, .envir = .envir) %>%
      purrr::map_chr(custom_stop_bullet)
    out %<>% {
      glue::glue_collapse(c(., dots), sep = "\n")
    }
  }
  rlang::abort(glue::glue("{out}"))
}

#' Wrap messages to make them prettier.
#'
#' Format messages with line breaks so that single words don't appear on multiple lines.
#'
#' @param ... Bits of the message to be pasted together.
#'
#' @noRd
pretty_msg <- function(...) {
  dots <- unlist(list(...))
  checkmate::assert_character(dots)
  glue::glue_collapse(dots) %>%
    strwrap(width = 63) %>%
    glue::glue_collapse(sep = "\n") %>%
    message()
}

#' TIFF tag reference.
#'
#' A dataset containing the information on all known baseline and extended TIFF
#' tags.
#'
#' A data frame with 96 rows and 10 variables: \describe{
#' \item{code_dec}{decimal numeric code of the TIFF tag}
#' \item{code_hex}{hexadecimal numeric code of the TIFF tag} \item{name}{the
#' name of the TIFF tag} \item{short_description}{a short description of the
#' TIFF tag} \item{tag_type}{the type of TIFF tag: either "baseline" or
#' "extended"} \item{url}{the URL of the TIFF tag at
#' \url{https://www.awaresystems.be}} \item{libtiff_name}{the TIFF tag name in
#' the libtiff C library} \item{c_type}{the C type of the TIFF tag data in
#' libtiff} \item{count}{the number of elements in the TIFF tag data}
#' \item{default}{the default value of the data held in the TIFF tag} }
#' @source \url{https://www.awaresystems.be}
#'
#' @examples
#' get_tiff_tags_reference()
#'
#' @export
get_tiff_tags_reference <- function() {
  "TIFF_tags.csv" %>%
    system.file("extdata", ., package = "ijtiff") %>%
    {suppressMessages(readr::read_csv(.))}
}


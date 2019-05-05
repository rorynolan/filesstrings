#' Read an image stored in the TIFF format
#'
#' Reads an image from a TIFF file/content into a numeric array or list.
#'
#' TIFF files have the capability to store multiple images, each having multiple
#' channels. Typically, these multiple images represent the sequential frames in
#' a time-stack or z-stack of images and hence each of these images has the same
#' dimension. If this is the case, they are all read into a single 4-dimensional
#' array `img` where `img` is indexed as `img[y, x, channel, frame]` (where we
#' have `y, x` to comply with the conventional `row, col` indexing of a matrix -
#' it means that images displayed as arrays of numbers in the R console will
#' have the correct orientation). However, it is possible that the images in the
#' TIFF file have varying dimensions (most people have never seen this), in
#' which case they are read in as a list of images, where again each element of
#' the list is a 4-dimensional array `img`, indexed as `img[y, x, channel,
#' frame]`.
#'
#' A (somewhat random) set of TIFF tags are attributed to the read image. These
#' are IMAGEDEPTH, BITSPERSAMPLE, SAMPLESPERPIXEL, SAMPLEFORMAT, PLANARCONFIG,
#' COMPRESSION, THRESHHOLDING, XRESOLUTION, YRESOLUTION, RESOLUTIONUNIT, INDEXED
#' and ORIENTATION. More tags should be added in a subsequent version of this
#' package. You can read about TIFF tags at
#' https://www.awaresystems.be/imaging/tiff/tifftags.html.
#'
#' TIFF images can have a wide range of internal representations, but only the
#' most common in image processing are supported (8-bit, 16-bit and 32-bit
#' integer and 32-bit float samples).
#'
#' @param path A string. The path to the tiff file to read.
#' @param list_safety A string. This is for type safety of this function. Since
#'   returning a list is unlikely and probably unexpected, the default is to
#'   error. You can instead opt to throw a warning (`list_safety = "warning"`)
#'   or to just return the list quietly (`list_safety = "none"`).
#' @param msg Print an informative message about the image being read?
#'
#' @return An object of class [ijtiff_img] or a list of [ijtiff_img]s.
#'
#' @note \itemize{ \item 12-bit TIFFs are not supported. \item There is no
#'   standard for packing order for TIFFs beyond 8-bit so we assume big-endian
#'   packing}.
#'
#' @author Simon Urbanek wrote most of this code for the 'tiff' package. Rory
#'   Nolan lifted it from there and changed it around a bit for this 'ijtiff'
#'   package. Credit should be directed towards Lord Urbanek.
#'
#' @seealso [write_tif()]
#'
#' @examples
#' img <- read_tif(system.file("img", "Rlogo.tif", package = "ijtiff"))
#' img <- read_tif(system.file("img", "2ch_ij.tif", package = "ijtiff"))
#' str(img)  # we see that `ijtiff` correctly recognises this image's 2 channels
#'
#' @export
read_tif <- function(path, list_safety = "error", msg = TRUE) {
  checkmate::assert_string(path)
  path %<>% stringr::str_replace_all(stringr::coll("\\"), "/") # windows safe
  checkmate::assert_file_exists(path)
  if (stringr::str_detect(path, "/")) {
    tiff_dir <- filesstrings::str_before_last(path, "/")
    checkmate::assert_directory_exists(tiff_dir)
    init_wd <- setwd(tiff_dir)
    on.exit(setwd(init_wd))
    path %<>% filesstrings::str_after_last("/")
    # `read_tif()` sometimes fails when writing to far away directories.
  }
  checkmate::assert_logical(msg, max.len = 1)
  checkmate::assert_string(list_safety)
  list_safety %<>% filesstrings::match_arg(c("error", "warning", "none"),
    ignore_case = TRUE
  )
  out <- .Call("read_tif_C", path.expand(path), PACKAGE = "ijtiff")
  checkmate::assert_list(out)
  ds <- dims(out)
  if (filesstrings::all_equal(ds)) {
    d <- ds[[1]]
    attrs1 <- attributes(out[[1]])
    n_ch <- 1
    if ("samples_per_pixel" %in% names(attrs1)) n_ch <- attrs1$samples_per_pixel
    if ("description" %in% names(attrs1)) {
      description <- attrs1$description
      if (startsWith(description, "ImageJ")) {
        ij_n_ch <- FALSE
        if (stringr::str_detect(description, "channels=")) {
          n_ch <- description %>%
            filesstrings::str_after_first("channels=") %>%
            filesstrings::first_number()
          ij_n_ch <- TRUE
        }
        n_imgs <- NA
        if (stringr::str_detect(description, "images=")) {
          n_imgs <- description %>%
            filesstrings::str_after_first("images=") %>%
            filesstrings::first_number()
        }
        n_slices <- NA
        if (stringr::str_detect(description, "slices=")) {
          n_slices <- description %>%
            filesstrings::str_after_first("slices=") %>%
            filesstrings::first_number()
        }
        if (stringr::str_detect(description, "frames=")) {
          n_frames <- description %>%
            filesstrings::str_after_first("frames=") %>%
            filesstrings::first_number()
          if (!is.na(n_slices)) {
            stop(
              "The ImageJ-written image you're trying to read says it has ",
              n_frames, " frames AND ", n_slices, " slices. To be read by ",
              "the 'ijtiff' package, the number of slices OR the number ",
              "of frames should be specified in the description tiff tag ",
              "(and they're interpreted as the same thing), but not both. "
            )
          }
          n_slices <- n_frames
        }
        if (!is.na(n_slices) && !is.na(n_imgs)) {
          if (ij_n_ch) {
            if (n_imgs != n_ch * n_slices) {
              stop(
                "The ImageJ-written image you're trying to read says in its",
                " TIFFTAG_DESCRIPTION that it has ", n_imgs, " images of ",
                n_slices, " slices of ", n_ch,
                " channels. However, with ", n_slices, " slices of ", n_ch,
                " channels, one would expect there to be ", n_slices, "x",
                n_ch, "=", n_ch * n_slices, " images. ",
                "This discrepancy means that the ",
                "'ijtiff' package can't read your image correctly. One ",
                "possible source of this kind of error is that your image ",
                "is temporal and volumetric. 'ijtiff' can handle either ",
                "time-based or volumetric stacks, but not both."
              )
            }
          }
        }
        if ((isTRUE(length(out) == n_imgs) && ij_n_ch) ||
          ((!ij_n_ch) && n_ch == 1)) {
          if (length(d) > 2) out %<>% purrr::map(extract_desired_plane)
        }
      }
    }
    out %<>% unlist()
    if (attrs1$sample_format == "uint") {
      bps <- attrs1$bits_per_sample
      checkmate::assert_int(bps, lower = 8, upper = 32)
      max_allowed <- 2^bps - 1
      if (any(out > max_allowed)) {
        biggest_offender <- max(out)
        while (all(out %% (2^bps) == 0))
          out <- out / 2^bps
        if (any(out > max_allowed)) {
          stop(
            "ijtiff encountered a fatal error trying to read your image.\n",
            "* Your image is ", bps, "-bit, meaning that the maximum ",
            "possible value in it is ", 2^bps - 1, ", however ijtiff has ",
            "managed to read values up to ", biggest_offender,
            "which is clearly wrong. \n", "Please file a bug at ",
            "https://github.com/rorynolan/ijtiff/issues ",
            "and attach the offending image. Sorry and thanks."
          )
        }
      }
    }
    dim(out) <- c(d[1:2], n_ch, length(out) / prod(c(d[1:2], n_ch)))
    if ("dim" %in% names(attrs1)) attrs1$dim <- NULL
    do_call_list <- c(list(img = out), attrs1)
    out <- do.call(ijtiff_img, do_call_list)
  }
  if (is.list(out)) {
    if (list_safety == "error") stop("`read_tif()` tried to return a list.")
    if (list_safety == "warning") warning("`read_tif()` is returning a list.")
    if (list_safety == "none") {
      if (msg) {
        message("Reading a list of images with differing dimensions . . .")
      }
    }
  } else if (msg) {
    ints <- attr(out, "sample_format") == "uint"
    bps <- attr(out, "bits_per_sample") %>% {
      dplyr::case_when(
        . == 8 ~ "an 8-bit, ",
        . == 16 ~ "a 16-bit, ",
        . == 32 ~ "a 32-bit, ",
        TRUE ~ "a 0-bit, "
      )
    }
    dim(out) %>% {
      pretty_msg(
        "Reading ", path, ": ", bps, .[1], "x", .[2], " pixel image of ",
        ifelse(ints, "unsigned integer", "floating point"), " type with ",
        .[3], " channel", ifelse(.[3] > 1, "s", ""), " and ", .[4],
        " frame", ifelse(.[4] > 1, "s", ""), " . . ."
      )
    }
  }
  if (msg) pretty_msg("\b Done.")
  out
}

#' Read TIFF tag information without actually reading the image array.
#'
#' TIFF files contain metadata about images in their _TIFF tags_. This function
#' is for reading this information without reading the actual image.
#'
#' @inheritParams read_tif
#' @param all TIFF files can contain multiple images. With `all = TRUE`, the
#'   information about all images is returned in a list of lists. To just get
#'   the information about some images, pass those image numbers to the `all`
#'   parameter (see examples). `all = FALSE` is equivalent to `all = 1`.
#'
#' @return A list of lists.
#'
#' @author Simon Urbanek, Kent Johnson, Rory Nolan.
#'
#' @seealso [read_tif()]
#'
#' @examples
#' read_tags(system.file("img", "Rlogo.tif", package="ijtiff"))
#' read_tags(system.file("img", "2ch_ij.tif", package="ijtiff"))
#' read_tags(system.file("img", "2ch_ij.tif", package="ijtiff"), all = c(2, 4))
#'
#' @export
read_tags <- function(path, all = TRUE) {
  checkmate::assert_string(path)
  path %<>% stringr::str_replace_all(stringr::coll("\\"), "/") # windows safe
  checkmate::assert_file_exists(path)
  if (stringr::str_detect(path, "/")) {
    init_wd <- setwd(filesstrings::str_before_last(path, "/"))
    on.exit(setwd(init_wd))
    path %<>% filesstrings::str_after_last("/")
    # `read_tags()` sometimes fails when writing to far away directories.
  }
  checkmate::assert(
    checkmate::check_flag(all),
    checkmate::check_integerish(all, lower = 1)
  )
  if (is.numeric(all)) {
    n_tiff_dirs <- count_imgs(path)
    all_max <- max(all)
    if (all_max > n_tiff_dirs) {
      custom_stop(
        "Cannot access image {all_max}.",
        "
         You have tried to access information from image {all_max},
         but there are only {n_tiff_dirs} images in total.
        "
      )
    }
  }
  if (rlang::is_false(all)) all <- 1
  out <- .Call("read_tags_C", path, all, PACKAGE = "ijtiff")
  if (rlang::is_true(all)) {
    names(out) <- paste0("img", seq_along(out))
  } else {
    names(out) <- paste0("img", all)
  }
  out
}

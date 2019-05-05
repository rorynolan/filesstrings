extend_for_all_chs <- function(x, n_ch) {
  if (is.null(x)) x <- list(NA)[rep(1, n_ch)]
  if (length(x) == 1) {
    if (is.list(x)) {
      x <- x[rep(1, n_ch)]
    } else {
      x <- list(x)[rep(1, n_ch)]
    }
  }
  sq <- seq_len(n_ch)
  x <- x[sq]
  for (i in sq) if (is.null(x[[i]])) x[[i]] <- NA
  x
}

empty_char_vec_to_empty_string <- function(x) {
  checkmate::assert_character(x)
  if (length(x) == 0) return("")
  x
}

make_nb_filename_ending <- function(nb_img) {
  checkmate::assert_array(nb_img, d = 4)
  def <- attr(nb_img, "def")
  swaps <- attr(nb_img, "swaps")
  auto <- attr(swaps, "auto")
  thresh <- attr(nb_img, "thresh")
  pasted_class <- paste(class(nb_img), collapse = "")
  nb <- dplyr::if_else(
    stringr::str_detect(pasted_class, "number"),
    "number", "brightness"
  )
  is_ts <- stringr::str_detect(pasted_class, "_ts_")
  fps_att_name <- "frames_per_set"
  if (is_ts) {
    if (!fps_att_name %in% names(attributes(nb_img))) {
      custom_stop("
                  If `nb_img` is a number or brightness time series, then
                  it must have an attribute '{fps_att_name}'.
                  ", "
                  Your `nb_img` appears to be a number or brightness time series
                  without a '{fps_att_name}' attribute.
                  ")
    }
  }
  d <- dim(nb_img)
  n_ch <- d[3]
  if ("autothresh_method" %in% names(attributes(thresh))) {
    thresh_method <- attr(thresh, "autothresh_method")
  } else {
    thresh_method <- rep(NA, n_ch)
  }
  filt <- attr(nb_img, "filt")
  if (!filesstrings::all_equal(c(
    length(swaps), length(auto), length(thresh),
    length(thresh_method), length(filt), n_ch
  ))) {
    custom_stop(
      "
      The lengths of the 'thresh', 'swaps' and 'filt' attributes and the
      'autothresh_method' and 'auto' attriutes of the thresh and swaps
      attributes respectively of `nb_img` must all be the same
      as the number of channels in `img`.
      ",
      "There are {n_ch} channels in `img`.",
      "The 'thresh' attribute has length {length(thresh)}.",
      "The 'swaps' attribute has length {length(swaps)}.",
      "The 'filt' attribute has length {length(filt)}.",
      "
      The 'autothresh_method' attribute of the 'thresh' attribute has
      length {length(thresh_method)}.
      ",
      "The 'auto' attribute of the 'swaps' attribute has length {length(auto)}."
    )
  }
  swaps_part <- purrr::map2_chr(auto, swaps,
    ~glue::glue(dplyr::if_else(.x, "auto=", ""), "{.y},")) %>%
    glue::glue_collapse() %>%
    stringr::str_sub(1, -2)
  thresh_part <- purrr::map2_chr(thresh_method, thresh,
    ~glue::glue(dplyr::if_else(is.na(.x),
                               glue::as_glue(""), glue::glue("{.x}=")),
                "{.y},")) %>%
    glue::glue_collapse() %>%
    stringr::str_sub(1, -2)
  out <- glue::glue(
    "_{nb}_{def}_",
    dplyr::if_else(
      is_ts,
      glue::glue("timeseries_frames_per_set={attr(nb_img, fps_att_name)}_") %>%
        empty_char_vec_to_empty_string() %>%
        glue::as_glue(),
      glue::as_glue("")
    ),
    "swaps={swaps_part}_thresh={thresh_part}_filt=",
    dplyr::if_else(is.na(filt), "NA", as.character(filt)) %>%
      glue::glue_collapse(sep = ",")
  )
  if (is_ts) {
    overlapped <- attr(nb_img, "overlapped")
    out %<>% stringr::str_replace(
      "timeseries",
      dplyr::if_else(overlapped,
                     "overlapped_timeseries", "contiguous_timeseries")
    )
  }
  out
}

make_cc_nb_filename_ending <- function(cc_nb_img) {
  checkmate::assert_array(cc_nb_img, d = 4)
  swaps <- attr(cc_nb_img, "swaps")
  auto <- attr(swaps, "auto")
  thresh <- attr(cc_nb_img, "thresh")
  pasted_class <- paste(class(cc_nb_img), collapse = "")
  cc_nb <- dplyr::if_else(
    stringr::str_detect(pasted_class, "number"),
    "number", "brightness"
  ) %>% paste0("cc_", .)
  is_ts <- stringr::str_detect(pasted_class, "_ts_")
  fps_att_name <- "frames_per_set"
  if (is_ts) {
    if (!fps_att_name %in% names(attributes(cc_nb_img))) {
      custom_stop("
                  If `cc_nb_img` is a cross-correlated number or brightness time
                  series, then it must have an attribute '{fps_att_name}'.
                  ", "
                  Your `cc_nb_img` appears to be a cross-correlated number or
                  brightness time series without a '{fps_att_name}' attribute.
                  ")
    }
  }
  if ("autothresh_method" %in% names(attributes(thresh))) {
    thresh_method <- attr(thresh, "autothresh_method")
  } else {
    thresh_method <- rep(NA, length(thresh))
  }
  filt <- attr(cc_nb_img, "filt")
  checkmate::assert_numeric(thresh, len = 2)
  checkmate::assert_character(thresh_method, len = 2)
  checkmate::assert_numeric(swaps, len = 2)
  checkmate::assert_logical(auto, len = 2)
  checkmate::assert_string(filt, na.ok = TRUE)
  swaps_part <- purrr::map2_chr(auto, swaps,
    ~glue::glue(dplyr::if_else(.x, "auto=", ""), "{.y},")) %>%
    glue::glue_collapse() %>%
    stringr::str_sub(1, -2)
  thresh_part <- purrr::map2_chr(thresh_method, thresh,
    ~glue::glue(dplyr::if_else(is.na(.x),
                               glue::as_glue(""), glue::glue("{.x}=")),
                "{.y},")) %>%
    glue::glue_collapse() %>%
    stringr::str_sub(1, -2)
  out <- glue::glue(
    "_{cc_nb}_",
    dplyr::if_else(
      is_ts,
      glue::glue(
        "timeseries_frames_per_set={attr(cc_nb_img, fps_att_name)}_"
      ) %>%
        empty_char_vec_to_empty_string() %>%
        glue::as_glue(""),
      glue::as_glue("")
    ),
    "swaps={swaps_part}_thresh={thresh_part}_filt=",
    dplyr::if_else(is.na(filt), "NA", as.character(filt)) %>%
      glue::glue_collapse(sep = ",")
  )
  if (is_ts) {
    overlapped <- attr(cc_nb_img, "overlapped")
    out %<>% stringr::str_replace(
      "timeseries",
      dplyr::if_else(overlapped,
                     "overlapped_timeseries", "contiguous_timeseries")
    )
  }
  out
}

deduplicate_nb_filename <- function(path) {
  checkmate::assert_string(path)
  if (any(stringr::str_detect(path, paste0(
    "detrended_",
    c(
      "boxcar", "[Rr]obin",
      "exponential",
      "polynomial"
    )
  )))) {
    thresh_pattern <- "_thresh=.*?_"
    n_threshs <- stringr::str_count(path, thresh_pattern)
    if (n_threshs == 2) {
      second_thresh_indices <- stringr::str_locate_all(
        path,
        thresh_pattern
      )[[1]][2, ]
      if (stringr::str_detect(
        stringr::str_sub(
          path,
          second_thresh_indices["start"],
          second_thresh_indices["end"]
        ),
        "_thresh=NA,*(NA,)*(NA)*_"
      )) {
        if (second_thresh_indices["end"] == nchar(path)) {
          path %<>% stringr::str_sub(1, second_thresh_indices["start"] - 1)
        } else {
          path <- paste0(
            stringr::str_sub(
              path,
              1, second_thresh_indices["start"] - 1
            ),
            stringr::str_sub(
              path,
              second_thresh_indices["end"], -1
            )
          )
        }
      }
    }
    swaps_pattern <- "_swaps=.*?_"
    n_swaps <- stringr::str_count(path, swaps_pattern)
    if (n_swaps == 2) {
      second_swaps_indices <- stringr::str_locate_all(
        path,
        swaps_pattern
      )[[1]][2, ]
      if (stringr::str_detect(
        stringr::str_sub(
          path,
          second_swaps_indices["start"],
          second_swaps_indices["end"]
        ),
        "_swaps=NA,*(NA,)*(NA)*_"
      )) {
        if (second_swaps_indices["end"] == nchar(path)) {
          path %<>% stringr::str_sub(1, second_swaps_indices["start"] - 1)
        } else {
          path <- paste0(
            stringr::str_sub(
              path,
              1, second_swaps_indices["start"] - 1
            ),
            stringr::str_sub(
              path,
              second_swaps_indices["end"], -1
            )
          )
        }
      }
    }
  }
  path
}

deduplicate_cc_nb_filename <- function(path) {
  checkmate::assert_string(path)
  if (any(stringr::str_detect(path, paste0(
    "detrended_",
    c(
      "boxcar", "[Rr]obin",
      "exponential",
      "polynomial"
    )
  )))) {
    thresh_pattern <- "_thresh=.*?_"
    n_threshs <- stringr::str_count(path, thresh_pattern)
    if (n_threshs == 2) {
      second_thresh_indices <- stringr::str_locate_all(
        path,
        thresh_pattern
      )[[1]][2, ]
      if (stringr::str_detect(
        stringr::str_sub(
          path,
          second_thresh_indices["start"],
          second_thresh_indices["end"]
        ),
        "_thresh=NA,*(NA,)*(NA)*_"
      )) {
        if (second_thresh_indices["end"] == nchar(path)) {
          path %<>% stringr::str_sub(1, second_thresh_indices["start"] - 1)
        } else {
          path <- paste0(
            stringr::str_sub(
              path,
              1, second_thresh_indices["start"] - 1
            ),
            stringr::str_sub(
              path,
              second_thresh_indices["end"], -1
            )
          )
        }
      }
    }
    swaps_pattern <- "_swaps=.*?_"
    n_swaps <- stringr::str_count(path, swaps_pattern)
    if (n_swaps == 2) {
      second_swaps_indices <- stringr::str_locate_all(
        path,
        swaps_pattern
      )[[1]][2, ]
      if (stringr::str_detect(
        stringr::str_sub(
          path,
          second_swaps_indices["start"],
          second_swaps_indices["end"]
        ),
        "_swaps=NA,*(NA,)*(NA)*_"
      )) {
        if (second_swaps_indices["end"] == nchar(path)) {
          path %<>% stringr::str_sub(1, second_swaps_indices["start"] - 1)
        } else {
          path <- paste0(
            stringr::str_sub(
              path,
              1, second_swaps_indices["start"] - 1
            ),
            stringr::str_sub(
              path,
              second_swaps_indices["end"], -1
            )
          )
        }
      }
    }
  }
  path
}

fix_filt <- function(filt) {
  if (is.null(filt)) filt <- NA_character_
  checkmate::assert_character(filt)
  filt %<>% tolower()
  filt[startsWith("mean", filt)] <- "mean"
  filt[startsWith("median", filt)] <- "median"
  if (!all((filt %in% c("mean", "median")) | is.na(filt))) {
    bad_index <- match(FALSE, filt %in% c("mean", "median"))
    custom_stop(
      "All elements of `filt` must be either 'mean', 'median' or NA.",
      "
      The offending element is element {bad_index} which is
      '{filt[bad_index]}'.
      ")
  }
  filt
}

nb_get_img <- function(img) {
  checkmate::assert(
    checkmate::check_array(img, min.d = 3, max.d = 4),
    checkmate::check_file_exists(img)
  )
  if (is.character(img)) {
    img %<>% ijtiff::read_tif()
  }
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
  if (length(dim(img)) == 3) {
    dim(img) %<>% {
      c(.[1:2], 1, .[3])
    }
  }
  if (dim(img)[4] == 1) {
    custom_stop("Your image only has one frame.",
                "
                 Images to be used for number and brightness analysis must
                 have more than one frame.
                ")
  }
  img
}

c_list_attr_na <- function(x) {
  l <- length(x)
  if (is.list(x)) {
    x_attr_names <- x %>%
      purrr::map(~names(attributes(.))) %>%
      unlist() %>%
      unique()
    for (i in seq_along(x)) {
      attr(x[[i]], "class") <- class(x[[i]])[1]
      for (name in x_attr_names) {
        if (!name %in% names(attributes(x[[i]]))) {
          attr(x[[i]], name) <- NA
        }
      }
    }
    atts <- x %>%
      purrr::map(~attributes(.)) %>%
      dplyr::bind_rows()
    atts$class <- NULL
    x %<>% purrr::reduce(c)
    attributes(x) <- atts
  }
  assertthat::assert_that(length(x) == l)  # should never trigger
  x
}

can_be_numeric <- function(vec) {
  checkmate::assert_atomic(vec)
  nas_before <- sum(suppressWarnings(is.na(vec)))
  nas_after <- sum(suppressWarnings(is.na(as.numeric(vec))))
  ifelse(nas_after > nas_before, FALSE, TRUE)
}

prepare_thresh <- function(thresh) {
  checkmate::assert(
    checkmate::check_logical(thresh, min.len = 1, max.len = 2),
    checkmate::check_null(thresh),
    checkmate::check_character(thresh, min.len = 1, max.len = 2),
    checkmate::check_numeric(thresh, min.len = 1, max.len = 2),
    checkmate::check_list(thresh, min.len = 1, max.len = 2)
  )
  if (is.null(thresh)) thresh <- as.character(rep(NA, 2))
  if (all(is.na(thresh))) thresh <- as.character(rep(NA, 2))
  if (length(thresh) == 1) thresh <- as.character(rep(thresh[[1]], 2))
  purrr::map(thresh, ~ifelse(can_be_numeric(.), as.numeric(.), .))
}

prepare_filt <- function(filt) {
  if (is.null(filt)) filt <- NA
  if (is.na(filt)) return(NA_character_)
  checkmate::assert_string(filt)
  if (!all(startsWith("smooth", filt) | startsWith("median", filt))) {
    filt %<>% glue::glue_collapse(sep = "', '")
    custom_stop(
      "The allowable values for filt are 'smooth', 'median' or NA. ",
      "You have `filt = c('{filt}')`."
    )
  }
  filt %<>% filesstrings::match_arg(c("smooth", "median"), ignore_case = TRUE)
  filt
}

deduplicate_cc_nb_filename <- function(path) {
  checkmate::assert_string(path)
  if (any(stringr::str_detect(path, paste0(
    "detrended_",
    c(
      "boxcar", "[Rr]obin",
      "exponential",
      "polynomial"
    )
  )))) {
    thresh_pattern <- "_thresh=.*?_"
    n_threshs <- stringr::str_count(path, thresh_pattern)
    if (n_threshs == 2) {
      second_thresh_indices <- stringr::str_locate_all(
        path,
        thresh_pattern
      )[[1]][2, ]
      if (stringr::str_detect(
        stringr::str_sub(
          path,
          second_thresh_indices["start"],
          second_thresh_indices["end"]
        ),
        "_thresh=NA,*(NA,)*(NA)*_"
      )) {
        if (second_thresh_indices["end"] == nchar(path)) {
          path %<>% stringr::str_sub(1, second_thresh_indices["start"] - 1)
        } else {
          path <- paste0(
            stringr::str_sub(
              path,
              1, second_thresh_indices["start"] - 1
            ),
            stringr::str_sub(
              path,
              second_thresh_indices["end"], -1
            )
          )
        }
      }
    }
    swaps_pattern <- "_swaps=.*?_"
    n_swaps <- stringr::str_count(path, swaps_pattern)
    if (n_swaps == 2) {
      second_swaps_indices <- stringr::str_locate_all(
        path,
        swaps_pattern
      )[[1]][2, ]
      if (stringr::str_detect(
        stringr::str_sub(
          path,
          second_swaps_indices["start"],
          second_swaps_indices["end"]
        ),
        "_swaps=NA,*(NA,)*(NA)*_"
      )) {
        if (second_swaps_indices["end"] == nchar(path)) {
          path %<>% stringr::str_sub(1, second_swaps_indices["start"] - 1)
        } else {
          path <- paste0(
            stringr::str_sub(
              path,
              1, second_swaps_indices["start"] - 1
            ),
            stringr::str_sub(
              path,
              second_swaps_indices["end"], -1
            )
          )
        }
      }
    }
  }
  path
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

#' Prepare a Test Error Messsage.
#'
#' Take the command copied to the clipboard and prepare the error message that
#' it outputs for expectation with [testthat::expect_error].
#'
#' @return The string that was copied to the clipboard (invisibly).
#'
#' @noRd
ptem <- function() {
  out <- character(0)
  if (all(c("ore", "clipr", "styler") %in%
          rownames(utils::installed.packages()))) {
    cmd <- clipr::read_clip() %>%
      paste(collapse = " ") %>%
      rlang::parse_expr()
    out <- tryCatch(eval(cmd),
                    error = function(e) {
                      conditionMessage(e)
                    }
    )
    ends_with_period <- out %>%
      stringr::str_trim() %>%
      filesstrings::str_elem(-1) %>%
      filesstrings::all_equal(".")
    out %<>%
      stringr::str_replace_all("\\s+\\s+\\**\\s*", "heregoesdotplus") %>%
      stringr::str_replace_all("[\\.\\n]+", "heregoesdotplus") %>%
      ore::ore.escape() %>%
      stringr::str_replace_all("heregoesdotplus", ".+") %>%
      stringr::str_replace_all("(\\.\\+)+$", "") %>%
      filesstrings::singleize(ore::ore.escape(".+")) %>%
      stringr::str_replace("^Error: ", "") %>%
      strwrap(width = 55)
    if (ends_with_period) out[length(out)] %<>% paste0(".")
    left <- rep("\"", length(out))
    left[1] <- "paste0(\""
    right <- rep("\\s?\",", length(out))
    right[length(right)] <- "\")"
    out %<>%
      paste0(left, ., right) %>%
      stringr::str_replace_all("\\\\", "\\\\\\\\") %>%
      styler::style_text()
    if (length(out) == 1) {
      out %<>% stringr::str_sub(nchar("paste0(") + 1,
                                nchar(.) - nchar(")"))
    }
    clipr::write_clip(out)
  }
  out
}

err_fun <- function() {
  stop("An error message to give ptem() full coverage.")
}

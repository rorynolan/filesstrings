translate_parallel <- function(parallel) {
  checkmate::assert(
    checkmate::check_int(parallel),
    checkmate::check_logical(parallel, len = 1)
  )
  n_cores <- 1
  if (isTRUE(parallel)) {
    n_cores <- parallel::detectCores()
  } else if (is.numeric(parallel)) {
    n_cores <- parallel
    if (n_cores > parallel::detectCores()) n_cores <- parallel::detectCores()
  }
  n_cores
}

get_seed <- function() sample.int(.Machine$integer.max, 1)

#' Apply a function to each pillar of a 3-dimensional array.
#'
#' Define a 'pillar' of a 3-dimensional array as pillar `i,j` off array
#' `arr` being `arr[i, j, ]`. This function applies a specified
#' function to each pillar.
#'
#' @param arr3d A 3-dimensional array.
#' @param FUN A function which takes a vector as input and, for a given input
#'   length, outputs a vector of constant length (can be 1).
#'
#' @return If `FUN` is returning length 1 vectors, a matrix whereby
#'   `mat[i, j] = FUN(arr3d[i, j, ])`. If FUN is returning vectors of
#'   length `l > 1`, a 3-dimensional array whereby \code{arr[i, j, ] =
#'   FUN(arr3d[i, j, ])}.
#' @export
apply_on_pillars <- function(arr3d, FUN) {
  apply(arr3d, c(1, 2), FUN) %>% {
    if (length(dim(.)) == 3) {
      aperm(., c(2, 3, 1))
    } else {
      .
    }
  }
}

## Convert array indices to vector indices.
## Just a faster version of arrayhelpers::array2vec().
myarray2vec <- function(iarr, dim) {
  if (!is.matrix(iarr)) {
    dim(iarr) <- c(1, length(iarr))
  }
  if (ncol(iarr) != length(dim)) {
    custom_stop(
      "Number of columns in `iarr` and number of dimensions must be the same.",
      "
      There are {ncol(iarr)} columns in `iarr` and you have specified
      {length(dim)} dimensions.
      "
    )
  }
  if (any(sweep(iarr, 2, dim) > 0)) {
    bad <- iarr > dim
    row <- match(TRUE, matrixStats::rowAnys(bad))
    rowc <- paste0("c(", glue::glue_collapse(iarr[row, ], sep = ", "), ")")
    dimc <- paste0("c(", glue::glue_collapse(dim, sep = ", "), ")")
    col <- match(TRUE, bad[row, ])
    custom_stop(
      "You are requesting an array index outside the dimension of your array.",
      "
      Row {row} of `iarr` is `{rowc}` and `dim` is `{dimc}`, so element {col} of
      that row, {iarr[row, col]}, is too big as it is bigger than element
      {col} of `dim`, {dim[col]}.
      "
    )
  }
  pdim <- c(1, cumprod(dim[-length(dim)]))
  iarr <- iarr - 1
  rowSums(sweep(iarr, 2, pdim, "*")) + 1
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
  dots <- list(...) %>%
    unlist() %>%
    purrr::map_chr(toString)
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

get_os <- function() {
  sysinf <- Sys.info()
  if (!is.null(sysinf)) {
    os <- sysinf["sysname"]
    if (os == "Darwin") {
      os <- "osx"
    }
  } else { ## mystery machine
    os <- .Platform$OS.type
    if (grepl("^darwin", R.version$os)) {
      os <- "osx"
    }
    if (grepl("linux-gnu", R.version$os)) {
      os <- "linux"
    }
  }
  if (os == "osx") os <- "mac"
  tolower(os)
}

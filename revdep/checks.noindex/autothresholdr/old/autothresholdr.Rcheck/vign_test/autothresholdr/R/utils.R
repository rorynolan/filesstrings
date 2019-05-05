## Translate the fail argument from what the user selects to what failed pixels
## should be set to.
translate_fail <- function(arr, fail) {
  checkmate::assert_scalar(fail, na.ok = TRUE)
  if (is.na(fail)) return(NA)
  if (is.numeric(fail)) {
    if (fail < 0) {
      custom_stop(
        "
        If `fail` is specified as a number, then that number must be greater
        than zero.
        ",
        "You have specified `fail = {format(fail, scientific = FALSE)}`.")
    }
  } else if (is.character(fail)) {
    fail <- filesstrings::match_arg(fail, c("saturate", "zero"),
                                   ignore_case = TRUE)
  }
  if (fail == "zero") {
    fail <- 0
  } else if (fail == "saturate") {
    mx <- max(arr, na.rm = TRUE)
    bits_per_sample <- 8
    if (mx > 2 ^ 16 - 1) {
      bits_per_sample <- 32
    } else if (mx > 2 ^ 8 - 1) {
      bits_per_sample <- 16
    }
    fail <- 2 ^ bits_per_sample - 1
  }
  fail
}

eval_text <- function(string) {
  checkmate::assert_scalar(string)
  checkmate::assert_character(string)
  eval(parse(text = string))
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
      styler::style_text() %>%
      as.character()
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

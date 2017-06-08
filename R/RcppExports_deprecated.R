#' @rdname filesstrings-deprecated
#' @param char_list A list of character vectors.
#' @param collapse See \code{\link{paste}}.
#' @export
PasteCollapseListElems <- function(char_list, collapse = "") {
  .Deprecated("paste_collapse_list_elems")
  paste_collapse_list_elems(char_list, collapse)
}

#' @rdname filesstrings-deprecated
#' @export
StrListRemoveEmpties <- function(char_list) {
  .Deprecated("str_list_remove_empties")
  str_list_remove_empties(char_list)
}

#' @rdname filesstrings-deprecated
#' @export
CharListElemsNthElem <- function(char_list, n) {
  .Deprecated("str_list_nth_elems")
  str_list_nth_elems(char_list, n)
}

#' @rdname filesstrings-deprecated
#' @param num_list A list of numeric vectors.
#' @export
NumListElemsNthElem <- function(num_list, n) {
  .Deprecated("num_list_nth_elems")
  num_list_nth_elems(num_list, n)
}

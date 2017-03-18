#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME:
Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP filesstrings_CharListElemsNthElem(SEXP, SEXP);
extern SEXP filesstrings_CorrectInterleave(SEXP, SEXP, SEXP);
extern SEXP filesstrings_CorrectInterleave0(SEXP, SEXP, SEXP);
extern SEXP filesstrings_InterleaveStringList(SEXP, SEXP);
extern SEXP filesstrings_InterleaveStrings(SEXP, SEXP);
extern SEXP filesstrings_NumListElemsNthElem(SEXP, SEXP);
extern SEXP filesstrings_PasteCollapse(SEXP, SEXP);
extern SEXP filesstrings_PasteCollapseListElems(SEXP, SEXP);
extern SEXP filesstrings_StrListRemoveEmpties(SEXP);

static const R_CallMethodDef CallEntries[] = {
  {"filesstrings_CharListElemsNthElem",   (DL_FUNC) &filesstrings_CharListElemsNthElem,   2},
  {"filesstrings_CorrectInterleave",      (DL_FUNC) &filesstrings_CorrectInterleave,      3},
  {"filesstrings_CorrectInterleave0",     (DL_FUNC) &filesstrings_CorrectInterleave0,     3},
  {"filesstrings_InterleaveStringList",   (DL_FUNC) &filesstrings_InterleaveStringList,   2},
  {"filesstrings_InterleaveStrings",      (DL_FUNC) &filesstrings_InterleaveStrings,      2},
  {"filesstrings_NumListElemsNthElem",    (DL_FUNC) &filesstrings_NumListElemsNthElem,    2},
  {"filesstrings_PasteCollapse",          (DL_FUNC) &filesstrings_PasteCollapse,          2},
  {"filesstrings_PasteCollapseListElems", (DL_FUNC) &filesstrings_PasteCollapseListElems, 2},
  {"filesstrings_StrListRemoveEmpties",   (DL_FUNC) &filesstrings_StrListRemoveEmpties,   1},
  {NULL, NULL, 0}
};

void R_init_filesstrings(DllInfo *dll)
{
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}

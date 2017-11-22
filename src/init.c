#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP _filesstrings_correct_interleave(SEXP, SEXP, SEXP);
extern SEXP _filesstrings_correct_interleave_helper(SEXP, SEXP, SEXP);
extern SEXP _filesstrings_interleave_char_lists(SEXP, SEXP);
extern SEXP _filesstrings_interleave_strings(SEXP, SEXP);
extern SEXP _filesstrings_intmat_list_bind_nth_rows(SEXP, SEXP);
extern SEXP _filesstrings_intmat_list_nrows(SEXP);
extern SEXP _filesstrings_num_list_nth_elems(SEXP, SEXP);
extern SEXP _filesstrings_paste_collapse(SEXP, SEXP);
extern SEXP _filesstrings_paste_collapse_list_elems(SEXP, SEXP);
extern SEXP _filesstrings_str_list_nth_elems(SEXP, SEXP);
extern SEXP _filesstrings_str_list_remove_empties(SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"_filesstrings_correct_interleave",        (DL_FUNC) &_filesstrings_correct_interleave,        3},
    {"_filesstrings_correct_interleave_helper", (DL_FUNC) &_filesstrings_correct_interleave_helper, 3},
    {"_filesstrings_interleave_char_lists",     (DL_FUNC) &_filesstrings_interleave_char_lists,     2},
    {"_filesstrings_interleave_strings",        (DL_FUNC) &_filesstrings_interleave_strings,        2},
    {"_filesstrings_intmat_list_bind_nth_rows", (DL_FUNC) &_filesstrings_intmat_list_bind_nth_rows, 2},
    {"_filesstrings_intmat_list_nrows",         (DL_FUNC) &_filesstrings_intmat_list_nrows,         1},
    {"_filesstrings_num_list_nth_elems",        (DL_FUNC) &_filesstrings_num_list_nth_elems,        2},
    {"_filesstrings_paste_collapse",            (DL_FUNC) &_filesstrings_paste_collapse,            2},
    {"_filesstrings_paste_collapse_list_elems", (DL_FUNC) &_filesstrings_paste_collapse_list_elems, 2},
    {"_filesstrings_str_list_nth_elems",        (DL_FUNC) &_filesstrings_str_list_nth_elems,        2},
    {"_filesstrings_str_list_remove_empties",   (DL_FUNC) &_filesstrings_str_list_remove_empties,   1},
    {NULL, NULL, 0}
};

void R_init_filesstrings(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}

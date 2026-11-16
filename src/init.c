#include <R.h>
#include <R_ext/Rdynload.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL

/* FIXME:
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP C_validate_args(SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"C_validate_args", (DL_FUNC)&C_validate_args, 2}, {NULL, NULL, 0}};

void R_init_argvet(DllInfo *dll) {
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}

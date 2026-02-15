#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

SEXP boomer_promise_evaled(SEXP name, SEXP env) {
  SEXP object = Rf_findVar(name, env);
  if (TYPEOF(object) != PROMSXP) {
    return ScalarLogical(TRUE);
  }
  return ScalarLogical(PRVALUE(object) != R_UnboundValue);
}

static const R_CallMethodDef CallEntries[] = {
  {"boomer_promise_evaled", (DL_FUNC) &boomer_promise_evaled, 2},
  {NULL, NULL, 0}
};

void R_init_boomer(DllInfo *dll) {
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}

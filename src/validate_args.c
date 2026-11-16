#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include <Rversion.h> /* R_VERSION, R_Version() */
#include <limits.h>   /* INT_MAX */
#include <string.h>

static inline SEXP force_value(SEXP v, SEXP env) {
  if (TYPEOF(v) == PROMSXP)
    v = Rf_eval(v, env);
  return v;
}

/* Frame-only variable lookup, portable across R versions: returns the
   binding value (promise or active binding included) or R_UnboundValue.
   R >= 4.6.0 removed Rf_findVarInFrame from the public API; its approved
   replacements are R_existsVarInFrame() + R_getVar(). */
static SEXP frame_get(SEXP sym, SEXP env) {
#if defined(R_VERSION) && R_VERSION >= R_Version(4, 6, 0)
  /* NB: R_existsVarInFrame() takes (env, symbol) -- environment first. */
  if (R_existsVarInFrame(env, sym))
    return R_getVar(sym, env, FALSE);
  return R_UnboundValue;
#else
  return Rf_findVarInFrame(env, sym);
#endif
}

/* Value of the `...` binding in env, or R_UnboundValue when the frame has no
   dots formal. Unlike frame_get() this never signals "argument is missing":
   an empty `...` is stored as R_MissingArg and has to be forwarded as such,
   otherwise the evaluator below would not find any dots at all. */
static SEXP frame_get_dots(SEXP env) {
#if defined(R_VERSION) && R_VERSION >= R_Version(4, 6, 0)
  switch (R_GetBindingType(R_DotsSymbol, env)) {
  case R_BindingTypeUnbound:
    return R_UnboundValue;
  case R_BindingTypeMissing:
    return R_MissingArg;
  default:
    return R_getVar(R_DotsSymbol, env, FALSE);
  }
#else
  return Rf_findVarInFrame(env, R_DotsSymbol);
#endif
}

/* Must match the entry registered in src/init.c ("C_validate_args"). */
SEXP C_validate_args(SEXP expr, SEXP env) {
  if (TYPEOF(env) != ENVSXP)
    Rf_error("'env' must be an environment.");

  int nprot = 0;

  /* ls(env, all.names = TRUE, sorted = TRUE) */
  SEXP all = PROTECT(R_lsInternal3(env, TRUE, TRUE));
  nprot++;
  R_xlen_t n_all = XLENGTH(all);

  SEXP vals = PROTECT(Rf_allocVector(VECSXP, n_all));
  nprot++;
  SEXP nms = PROTECT(Rf_allocVector(STRSXP, n_all));
  nprot++;

  /* Symbol cache: resolve each name once instead of on every later use. */
  SEXP syms = PROTECT(Rf_allocVector(VECSXP, n_all));
  nprot++;

  /* setdiff + mget */
  R_xlen_t k = 0;
  for (R_xlen_t i = 0; i < n_all; i++) {
    SEXP nm = STRING_ELT(all, i);
    if (strcmp(CHAR(nm), "...") == 0)
      continue;

    SEXP sym = Rf_installTrChar(nm);
    SEXP v = frame_get(sym, env);
    if (v == R_UnboundValue)
      continue;
    if (v == R_MissingArg)
      /* An unsupplied formal with no default: raise R's canonical error
         instead of silently dropping the binding from the result. */
      Rf_error("argument \"%s\" is missing, with no default",
               Rf_translateChar(nm));
    v = force_value(v, env);

    SET_VECTOR_ELT(vals, k, v);
    SET_STRING_ELT(nms, k, nm);
    SET_VECTOR_ELT(syms, k, sym);
    k++;
  }

  if (k < n_all) {
    vals = PROTECT(Rf_lengthgets(vals, k));
    nprot++;
    nms = PROTECT(Rf_lengthgets(nms, k));
    nprot++;
    syms = PROTECT(Rf_lengthgets(syms, k));
    nprot++;
  }
  Rf_setAttrib(vals, R_NamesSymbol, nms);

  /* list2env(args, parent = env) */
  R_xlen_t hash_size = k > 29 ? k : 29;
  if (hash_size > INT_MAX) /* R_NewEnv() takes an int hash-table size */
    hash_size = INT_MAX;
  SEXP eval_env = PROTECT(R_NewEnv(env, TRUE, (int)hash_size));
  nprot++;
  for (R_xlen_t j = 0; j < k; j++) {
    Rf_defineVar(VECTOR_ELT(syms, j), VECTOR_ELT(vals, j), eval_env);
  }

  /* Forward `...` so that expr can use rlang::check_dots_empty(), rlang's
     dots helpers and `..1`/`..2`. It is not part of the returned list: a
     dots object cannot be stored in a list, so it stays out of `syms`. */
  SEXP dots = frame_get_dots(env);
  if (dots != R_UnboundValue)
    Rf_defineVar(R_DotsSymbol, dots, eval_env);

  Rf_eval(expr, eval_env);

  for (R_xlen_t j = 0; j < k; j++) {
    SEXP v = frame_get(VECTOR_ELT(syms, j), eval_env);
    if (v == R_UnboundValue)
      continue;
    SET_VECTOR_ELT(vals, j, force_value(v, eval_env));
  }

  UNPROTECT(nprot);
  return vals;
}
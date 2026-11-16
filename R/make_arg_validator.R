#' @title Create a Function that Validates and Transforms Caller Arguments
#'
#' @description
#' `make_arg_validator()` is a function factory: it takes an unquoted
#' expression written in terms of argument names and returns a *validator*
#' function. When that validator is called from inside another function, it
#' captures the calling function's arguments, evaluates the expression against
#' them, and returns the (possibly checked or coerced) arguments as a named
#' list.
#'
#' This keeps argument checking and normalization out of the body of the
#' function that uses them, and lets several functions share a single set of
#' rules.
#'
#' @param expr An unquoted R expression, usually a `{ ... }` block. Symbols in
#'   the expression are resolved against the caller's arguments, and
#'   assignments inside the expression update those arguments. The expression
#'   is evaluated in a child of the calling frame of the validator, so ordinary
#'   lexical scoping applies: helpers, constants and attached packages visible
#'   from the caller remain visible, while package functions should be called
#'   with `::`. The caller's `...` is forwarded to that environment as well,
#'   so dots helpers such as `rlang::check_dots_empty()`, `...length()` or
#'   `..1` behave exactly as they would in the calling function.
#'
#' @useDynLib argvet, .registration = TRUE
#'
#' @return `make_arg_validator()` returns a validator: a function with a single
#'   argument, `env`, which defaults to `rlang::caller_env()`, i.e. the
#'   environment of the function that called the validator.
#'
#'   Calling the validator forces every promise bound in `env` (so lazy default
#'   values are evaluated), evaluates `expr` against those bindings, and
#'   returns a named list of the objects bound in `env`, ordered by name.
#'   Values reassigned by `expr` replace the originals; the caller's own
#'   bindings are never modified in place.
#'
#' @details
#' ## What gets captured
#'
#' The validator copies every object in the caller's own evaluation frame
#' (`inherits = FALSE`). As a consequence:
#' \itemize{
#'   \item local temporaries created before the validator is called are
#'     captured too, not only the formal arguments. Filter the result if you
#'     want the formals only, for example with `filter_args_for_func()` from
#'     `standalone-args_to_func.R`.
#'   \item objects from enclosing scopes (the global environment, an outer
#'     function, the package namespace) are *not* part of the result, even
#'     though `expr` can still read them.
#'   \item `...` is never part of the result, because a dots object cannot be
#'     stored in a list. It is still *forwarded* to the environment `expr` runs
#'     in, so `list(...)`, `...length()`, `...elt()`, `..1` and
#'     `rlang::check_dots_empty()` all behave as they do inside the calling
#'     function.
#' }
#'
#' ## What gets returned
#'
#' Only names that already exist in `env` are kept, so a variable that `expr`
#' creates from scratch is dropped: the validator updates arguments, it does
#' not add them. The updates reach the caller exclusively through the returned
#' list. The two common patterns are therefore
#' `args <- validate_x(); args$a` for a one-off value, and
#' `list2env(validate_x(), envir = environment())` to write everything back
#' into the calling frame.
#'
#' ## Errors
#'
#' Errors signalled by `expr` propagate to the caller unchanged, so classed
#' conditions can be caught with `tryCatch()`. An argument without a default
#' that was never supplied fails with
#' `"argument \"x\" is missing, with no default"` when its promise is forced
#' by the capture step.
#'
#' ## Supplying `env` explicitly
#'
#' Passing `env` bypasses the caller lookup and validates the given
#' environment instead, which is convenient for validating a list of options,
#' a config object, or a `shiny` session environment.
#'
#' @examples
#'
#' # ---- 1. Check the arguments, then use the validated values ---------------
#' validate_add <- make_arg_validator({
#'   if (!is.numeric(x) || length(x) != 1L) {
#'     rlang::abort("`x` must be a single number.", call = NULL)
#'   }
#'   if (!is.numeric(y) || length(y) != 1L) {
#'     rlang::abort("`y` must be a single number.", call = NULL)
#'   }
#' })
#'
#' add <- function(x, y) {
#'   args <- validate_add()
#'   args$x + args$y
#' }
#'
#' add(1, 2)
#' #> [1] 3
#'
#' # validation errors keep their message and reach the caller unchanged
#' tryCatch(add("a", 2), error = function(cnd) conditionMessage(cnd))
#' #> [1] "`x` must be a single number."
#'
#' # ---- 2. Coerce first, write the result back into the calling frame -------
#' validate_ratio <- make_arg_validator({
#'   numerator <- as.numeric(numerator)
#'   denominator <- as.numeric(denominator)
#'   if (denominator == 0) {
#'     rlang::abort("`denominator` must not be zero.", call = NULL)
#'   }
#' })
#'
#' ratio <- function(numerator, denominator) {
#'   list2env(validate_ratio(), envir = environment())
#'   numerator / denominator
#' }
#'
#' ratio("3", 2)
#' #> [1] 1.5
#'
#' tryCatch(ratio(1, 0), error = function(cnd) conditionMessage(cnd))
#' #> [1] "`denominator` must not be zero."
#'
#' # ---- 3. Share one validator between several functions --------------------
#' validate_draw <- make_arg_validator({
#'   color <- match.arg(color, c("red", "green", "blue"))
#'   size <- max(size, 1)
#' })
#'
#' draw_point <- function(x, y, color = "red", size = 0) {
#'   args <- validate_draw()
#'   sprintf(
#'     "point (%s, %s) color=%s size=%s", x, y, args$color, args$size
#'   )
#' }
#'
#' draw_point(1, 2, color = "green", size = 0)
#' #> [1] "point (1, 2) color=green size=1"
#'
#' # ---- 4. Only bindings that already existed are returned ------------------
#' validate_scale <- make_arg_validator({
#'   span <- max(x) - min(x)
#'   x <- (x - min(x)) / span
#' })
#'
#' scale01 <- function(x) validate_scale()
#'
#' scale01(c(0, 5, 10))
#' #> $x
#' #> [1] 0.0 0.5 1.0
#' # `span` was created inside the expression, so it is not part of the result
#'
#' # ---- 5. Validate an environment directly ---------------------------------
#' validate_opts <- make_arg_validator({
#'   name <- trimws(name)
#'   threads <- as.integer(threads)
#' })
#'
#' opts <- list2env(list(name = "  model  ", threads = "4"))
#' validate_opts(opts)
#' #> $name
#' #> [1] "model"
#' #>
#' #> $threads
#' #> [1] 4
#'
#' # ---- 6. Local temporaries are captured as well ---------------------------
#' validate_pos <- make_arg_validator({
#'   if (x <= 0) {
#'     rlang::abort("`x` must be positive.", call = NULL)
#'   }
#' })
#'
#' sqrt_checked <- function(x) {
#'   tmp <- "not an argument"
#'   args <- validate_pos()
#'   names(args)
#' }
#'
#' sqrt_checked(4)
#' #> [1] "tmp" "x"
#'
#' # ---- 7. Classed conditions can be caught ---------------------------------
#' validate_n <- make_arg_validator({
#'   if (!is.numeric(n)) {
#'     rlang::abort(
#'       "`n` must be numeric.",
#'       class = "rpkgkit_bad_n",
#'       call = NULL
#'     )
#'   }
#' })
#'
#' count_up <- function(n) validate_n()
#'
#' tryCatch(
#'   count_up("a"),
#'   rpkgkit_bad_n = function(cnd) "caught a classed error"
#' )
#' #> [1] "caught a classed error"
#'
#' # ---- 8. `...` is forwarded to `expr` but never returned ------------------
#' validate_no_dots <- make_arg_validator({
#'   rlang::check_dots_empty()
#' })
#'
#' strict <- function(x, ...) validate_no_dots()
#'
#' strict(1)
#' #> $x
#' #> [1] 1
#'
#' # the dots check sees the caller's `...`, not an empty environment
#' tryCatch(strict(1, 2), rlib_error_dots_nonempty = function(cnd) "dots seen")
#' #> [1] "dots seen"
#'
#' validate_dots <- make_arg_validator({
#'   if (...length() > 2L) {
#'     rlang::abort("at most two values are supported.", call = NULL)
#'   }
#' })
#'
#' pick <- function(x, ...) validate_dots()
#'
#' tryCatch(pick(1, 2, 3, 4), error = function(cnd) conditionMessage(cnd))
#' #> [1] "at most two values are supported."
#'
#'
#' @export
make_arg_validator <- function(expr) {
  expr <- substitute(expr)
  function(env = rlang::caller_env()) {
    .Call(C_validate_args, expr, env)
  }
}

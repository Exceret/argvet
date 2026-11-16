test_that("make_arg_validator() returns a validator without evaluating expr", {
  validator <- make_arg_validator(stop("must not run at construction time"))

  expect_type(validator, "closure")
  expect_identical(names(formals(validator)), "env")
})

test_that("a validator captures every caller binding in sorted order", {
  validate <- make_arg_validator({})
  f <- function(x, y = 2) {
    tmp <- "a local temporary"
    validate()
  }

  args <- f(1)

  expect_type(args, "list")
  expect_identical(names(args), sort(c("tmp", "x", "y")))
  expect_identical(args, list(tmp = "a local temporary", x = 1, y = 2))
})

test_that("hidden bindings are captured too", {
  validate <- make_arg_validator({
    .offset <- .offset + 1L
  })
  f <- function(x) {
    .offset <- 1L
    validate()
  }

  args <- f(10)

  expect_setequal(names(args), c(".offset", "x"))
  expect_identical(args$.offset, 2L)
  expect_identical(args$x, 10)
})

test_that("lazy default arguments are forced exactly once", {
  calls <- 0L
  validate <- make_arg_validator({})
  f <- function(
    x = {
      calls <<- calls + 1L
      42
    }
  ) {
    validate()
  }

  args <- f()

  expect_identical(args$x, 42)
  expect_identical(calls, 1L)
})

test_that("unmodified values are returned without coercion", {
  validate <- make_arg_validator({})
  env <- list2env(list(
    df = data.frame(a = 1:2),
    fn = function() 1
  ))

  args <- validate(env)

  expect_identical(args$df, env$df)
  expect_identical(args$fn, env$fn)
})

test_that("expr reassignment updates the result but not the caller's frame", {
  validate <- make_arg_validator({
    x <- x * 2
    y <- as.character(y)
  })
  f <- function(x, y) {
    args <- validate()
    list(args = args, x = x, y = y)
  }

  out <- f(3, 4)

  expect_identical(out$args, list(x = 6, y = "4"))
  expect_identical(out$x, 3)
  expect_identical(out$y, 4)
})

test_that("bindings created by expr are dropped from the result", {
  validate <- make_arg_validator({
    span <- max(x) - min(x)
    x <- (x - min(x)) / span
  })
  f <- function(x) validate()

  args <- f(c(0, 5, 10))

  expect_identical(names(args), "x")
  expect_equal(args$x, c(0, 0.5, 1))
})

test_that("expr reads enclosing-scope objects without returning them", {
  offset <- 10
  validate <- make_arg_validator({
    x <- x + offset
  })
  f <- function(x) validate()

  expect_identical(f(1), list(x = 11))
})

test_that("an explicit env is validated with inherits = FALSE", {
  parent <- list2env(list(shared = 100))
  env <- new.env(parent = parent)
  env$x <- 1
  validate <- make_arg_validator({
    x <- x + shared
  })

  args <- validate(env)

  expect_identical(args, list(x = 101))
  expect_identical(env$x, 1)
  expect_identical(parent$shared, 100)
})

test_that("an empty env yields an empty list", {
  validate <- make_arg_validator({})

  args <- validate(new.env(parent = baseenv()))

  expect_type(args, "list")
  expect_length(args, 0)
})

test_that("env must be an environment", {
  validate <- make_arg_validator({})

  expect_error(validate(1), "'env' must be an environment.", fixed = TRUE)
  expect_error(
    validate(list(x = 1)),
    "'env' must be an environment.",
    fixed = TRUE
  )
})

test_that("a missing argument without a default raises R's canonical error", {
  validate <- make_arg_validator({})
  f <- function(x) validate()

  expect_error(f(), 'argument "x" is missing, with no default', fixed = TRUE)
})

test_that("errors signalled by expr propagate with their class and message", {
  validate <- make_arg_validator({
    rlang::abort(
      "`n` must be numeric.",
      class = "argvet_bad_n",
      call = NULL
    )
  })
  f <- function(n) validate()

  expect_error(f("a"), class = "argvet_bad_n")
  expect_error(f("a"), "`n` must be numeric.", fixed = TRUE)
})

test_that("plain base errors from expr are not wrapped", {
  validate <- make_arg_validator(stop("boom", call. = FALSE))
  f <- function(x) validate()

  expect_error(f(1), "boom", fixed = TRUE)
})

test_that("one validator can be shared by several functions", {
  validate <- make_arg_validator({
    color <- match.arg(color, c("red", "green", "blue"))
    size <- max(size, 1)
  })
  draw <- function(x, color = "red", size = 0) validate()
  label <- function(color = "blue", size = 5) validate()

  expect_identical(
    draw(1, color = "green"),
    list(color = "green", size = 1, x = 1)
  )
  expect_identical(label(), list(color = "blue", size = 5))
})

test_that("validated values can be written back with list2env()", {
  validate <- make_arg_validator({
    numerator <- as.numeric(numerator)
    denominator <- as.numeric(denominator)
  })
  ratio <- function(numerator, denominator) {
    list2env(validate(), envir = environment())
    numerator / denominator
  }

  expect_equal(ratio("3", 2), 1.5)
})

test_that("dots are forwarded to expr but never returned", {
  validate <- make_arg_validator({
    rlang::check_dots_empty()
  })
  strict <- function(x, ...) validate()

  expect_identical(strict(1), list(x = 1))
  expect_error(strict(1, 2), class = "rlib_error_dots_nonempty")
})

test_that("dots helpers see the caller's dots", {
  collect <- make_arg_validator({
    if (...length() != 2L) {
      rlang::abort("expected two dots", call = NULL)
    }
    x <- list(first = ..1, rest = list(...), total = ...length())
  })
  pick <- function(x, ...) collect()

  args <- pick(1, 2, 3)

  expect_identical(names(args), "x")
  expect_identical(
    args$x,
    list(first = 2, rest = list(2, 3), total = 2L)
  )
  expect_error(pick(1, 2), "expected two dots", fixed = TRUE)
})

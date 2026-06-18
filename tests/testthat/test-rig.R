options(boomer.safe_print = TRUE)

test_that("rig() works", {
  expect_snapshot({
    fun <- function(x) {
      n <- 1 + 2 * 3
      sum(base::nchar(utils:::head(x, -n)))
    }
    rigged <- rig(fun)
    rigged(letters)
  })
})


test_that("rigger() works", {
  expect_snapshot({
    fun <- function(x) {
      n <- 1 + 2 * 3
      sum(base::nchar(utils:::head(x, -n)))
    }
    # we print it without the print fun so the bytecode doesn't destabilize the snapshot
    r <- rigger()
    r[["print"]] <- NULL
    r
    rigged <- rigger() + fun
    rigged(letters)
  })
})

test_that("functions created at runtime are boomed", {
  foo2 <- function(x) {
    x <- x * 2
    SQRT <- sqrt
    SQRT(x)
  }
  expect_snapshot(rig(foo2)(2))
})

test_that("missing arguments don't break rig()", {
  fun <- function(a, b, c) {
    a + b
    missing(c)
  }
  expect_snapshot(rig(fun) (1, 2))
})

test_that("wrapped functions evaluated in the mask itself don't error (#140)", {
  # Some callers evaluate a wrapped function directly in the mask, e.g.
  # `rlang::arg_match()` resolves a formal's choices with
  # `eval_bare(formals(fn)[[arg]], get_env(fn))` and for a rigged function
  # `get_env(fn)` is the mask. The wrapper must then stop at the mask instead of
  # walking past the namespace chain into the empty environment, which used to
  # raise "the empty environment has no parent".
  foo <- function(x = c("a", "b")) c(x)
  rigged <- rig(foo)
  mask <- environment(rigged)
  expect_no_error(capture.output(eval(quote(c("a", "b")), mask)))
})

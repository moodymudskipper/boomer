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


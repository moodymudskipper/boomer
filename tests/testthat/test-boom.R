options(boomer.safe_print = TRUE)

test_that("boom() works", {
  expect_snapshot({
    boom(1 + 2 * 3)
    boom(sum(base::nchar(utils:::head(letters, -3))))
    boom(for(i in 1:10) i)
  })
})

test_that("boom() works with a namespaced non function", {
  expect_snapshot({
    boom(base::letters)
    boom(base:::letters)
    boom(base::letters, clock = TRUE)
    boom(base:::letters, clock = TRUE)
  })
})

test_that("boom() works with a global function", {
  expect_snapshot({
    fun <- function(x) {
      x
    }
    boom(fun(1:3))
  })
})

test_that("boom() works with README examples", {
  `%>%` <- magrittr::`%>%`

  expect_snapshot({
    boom(1 + !1 * 2)
    boom(subset(head(mtcars, 2), qsec > 17))

    mtcars %>%
      head(2) %>%
      subset(qsec > 17) %>%
      boom()
    boom(head(sapply(seq(10^2), sqrt)), print = str)
  })
})

test_that("print arg works", {
  expect_snapshot({
    boom(data.frame(a=1, b=2), print = str)
  })
})

test_that("print arg works with list", {
  expect_snapshot({
    boom(data.frame(a=1, b=2), print = list(data.frame = str))
    boom(data.frame(a=1, b=2), print = list(data.frame = str, print))
    boom(data.frame(a=1, b=2), print = list(str))
  })
})

test_that("clock arg works", {
  # can't reproduce output because time can change between iterations
  # so just taking that it doesn't fail
  expect_error(
    boom(subset(head(mtcars, 2), qsec > 17), clock = TRUE), NA
  )
  expect_error(
    boom(base::subset(utils:::head(mtcars, 2), qsec > 17), clock = TRUE), NA
  )
})

test_that("visible_only arg works", {
  expect_snapshot({
    boom(1 + invisible(1))
    options(boomer.visible_only = TRUE)
    boom(1 + invisible(1))
    options(boomer.visible_only = FALSE)
  })
})

test_that("can debug failing pipes (#17)", {
  expect_snapshot(error = TRUE, {
    1 %>%
      identity() %>%
      I() %>%
      boomer::boom()

    1 %>%
      identity() %>%
      missing_function() %>%
      I() %>%
      boomer::boom()

    eagerly_failing_function <- function(x) {
      stop("oops")
    }

    1 %>%
      identity() %>%
      eagerly_failing_function() %>%
      I() %>%
      boomer::boom()

    failing_function <- function(x) {
      force(x)
      stop("oops")
    }

    1 %>%
      identity() %>%
      failing_function() %>%
      I() %>%
      boomer::boom()
  })
})

test_that("functions created at runtime are boomed", {
  expect_snapshot(boom({
    x <- 2
    x <- x * 2
    SQRT <- sqrt
    SQRT(x)
  }))
})

test_that("assignments work", {
  expect_snapshot({
    boom({
      x <- 1 + 2
      y <- quote(a)
      u = 1 + 2
      v = quote(a)
    })

    options(boomer.ignore = NULL)
    boom({
      x <- 1 + 2
      y <- quote(a)
      u = 1 + 2
      v = quote(a)
    })
    options(boomer.ignore = c("~", "{", "(", "<-", "<<-", "="))
  })
})

test_that("multi-line calls are collapsed properly", {
  expect_snapshot({
    boom(c(0,1,2,3,4,5,6,7,8,9, 0,1,2,3,4,5,6,7,8,9,0))
    boom(c(0,1,2,3,4,5,6,7,8,9, 0,1,2,3,4,5,6,7,8,9,c(0, 1)))
  })
})

test_that("long calls are trimmed", {
  expect_snapshot({
  aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa <- 1
  bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb <- 2
  boom(aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa * 2 + bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb)
  })
})

test_that("`boomer.abbreviate` option works", {
  expect_snapshot({
    options(boomer.abbreviate = TRUE)
    boom(1 + 2 * 3)
    boom(c(0,1,2,3,4,5,6,7,8,9, 0,1,2,3,4,5,6,7,8,9,0))
    boom(c(0,1,2,3,4,5,6,7,8,9, 0,1,2,3,4,5,6,7,8,9,c(0, 1)))
    aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa <- 1
    bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb <- 2
    boom(aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa * 2 + bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb)
    options(boomer.abbreviate = FALSE)
  })
})

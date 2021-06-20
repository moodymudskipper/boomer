options(boom.safe_print = TRUE)

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
  })
  expect_snapshot({
    boom(base:::letters)
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
    boom(head(sapply(seq(10^6), sqrt)), print = str)
  })
})

test_that("print arg works", {
  expect_snapshot({
    boom(data.frame(a=1, b=2), print = str)
  })
})

test_that("clock arg works", {
  # can't reproduce output because time can change between iterations
  # so just taking that it doesn't fail
  expect_error(
    capture.output(boom(subset(head(mtcars, 2), qsec > 17), clock = TRUE)), NA
  )
  expect_error(
    capture.output(boom(base::subset(utils:::head(mtcars, 2), qsec > 17), clock = TRUE)), NA
  )
})

test_that("visible_only arg works", {
  expect_snapshot({
    boom(1 + invisible(1))
  })
  expect_snapshot({
    boom(1 + invisible(1), visible_only = TRUE)
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

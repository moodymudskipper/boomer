test_that("boom()", {
  expect_snapshot({
    boom(1 + 2 * 3)
    boom(sum(base::nchar(utils:::head(letters, -3))))
    boom(for(i in 1:10) i)
  })
})

test_that("boom() works with a global function", {
  expect_snapshot({
    fun <- function(x) {
      x
    }
    boomer::boom(fun(1:3))
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

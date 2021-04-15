test_that("boom()", {
  expect_snapshot({
    boom(1 + 2 * 3)
    boom(sum(base::nchar(utils:::head(letters, -3))))
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
  expect_snapshot({
    boom(1 + !1 * 2)
    boom(subset(head(mtcars, 2), qsec > 17))
    library(magrittr, quietly = TRUE, verbose = FALSE)
    mtcars %>%
      head(2) %>%
      subset(qsec > 17) %>%
      boom()
    boom(head(sapply(seq(10^6), sqrt)), print = str)
  })
})

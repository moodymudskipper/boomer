test_that("boom.ignore", {
  expect_snapshot({
    boom((1 + 1))
    opts <- options("boomer.ignore" = NULL)
    boom((1 + 1))
    options("boomer.ignore" = list(`(` = `(`))
    boom((1 + 1))
    options("boomer.ignore" = opts$boomer.ignore)
  })
})

test_that("boom.ignore_args", {
  expect_snapshot({
    options("boomer.ignore_args" = NULL)
    data.frame(a = 1:3) %>%
      transform( b = a + 1) %>%
      boom()
    options("boomer.ignore_args" = list(transform))
    data.frame(a = 1:3) %>%
      transform( b = a + 1) %>%
      boom()
    options("boomer.ignore_args" = "transform")
    data.frame(a = 1:3) %>%
      transform( b = a + 1) %>%
      boom()
    options("boomer.ignore_args" = NULL)
  })
})

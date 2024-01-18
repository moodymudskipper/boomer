test_that("boom.ignore.inside", {
  expect_snapshot({
    options("boomer.ignore.inside" = NULL)
    data.frame(a = 1:3) %>%
      transform( b = a + 1) %>%
      boom()
    options("boomer.ignore.inside" = list(transform))
    data.frame(a = 1:3) %>%
      transform( b = a + 1) %>%
      boom()
    options("boomer.ignore.inside" = NULL)
  })
})

test_that("boom()", {
  expect_snapshot({
    boom(1 + 2 * 3)
    boom(sum(base::nchar(utils:::head(letters, -3))))
  })
})

test_that("boom()", {
  expect_snapshot({
    fun <- function(x) {
      n <- 1 + 2 * 3
      sum(base::nchar(utils:::head(x, -n)))
    }
    rigged <- rig(fun)
    rigged(letters)
  })
})

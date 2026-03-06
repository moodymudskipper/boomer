options(boomer.safe_print = TRUE)

test_that("rig_in_place() works", {
  skip_on_ci()
  expect_snapshot({
    fact <- function(n) {
      if (n <= 1) 1
      else n * fact(n - 1)
    }
    
    rig_in_place(fact)
    fact(3)
  })
})
test_that("boom_on() and boom_off() toggle booming", {
  withr::local_options(boomer.safe_print = TRUE)
  fun <- function() {
    boom_on(browser())
    1 + 1
    boom_off()
    2 + 2
  }
  expect_snapshot(fun())
})

test_that("boom_on() warns when the caller is byte-compiled", {
  fun <- compiler::cmpfun(function() {
    boom_on()
    boom_off()
  })
  expect_warning(fun(), "byte-compiled")
})

test_that("boom_on() doesn't warn when given an unevaled `browser()`", {
  fun <- compiler::cmpfun(function() {
    boom_on(browser())
    boom_off()
  })
  expect_no_warning(fun())
})

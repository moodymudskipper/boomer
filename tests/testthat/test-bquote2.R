test_that("bquote2() works", {
  expect_identical(
    bquote2(.IF(TRUE, a)), quote(a))
  expect_identical(
    bquote2({.IF(FALSE, a)}), quote({}))
  expect_identical(
    bquote2({.IF(FALSE, a, b)}), quote({b}))
})

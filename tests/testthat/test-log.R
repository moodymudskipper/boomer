test_that("boomer.log can log the exploded output to a file (#142)", {
  withr::local_options(boomer.safe_print = TRUE)
  f <- withr::local_tempfile(fileext = ".log")
  withr::local_options(boomer.log = f)

  console <- capture.output(res <- boom(1 + 2 * 3))
  logged <- readLines(f)

  # the value is returned transparently
  expect_equal(res, 7)
  # the exploded sub-call is written to the file ...
  expect_true(any(grepl("2 * 3", logged, fixed = TRUE)))
  # ... and not to the console
  expect_false(any(grepl("2 * 3", console, fixed = TRUE)))
  # files are written without ANSI styling
  expect_false(any(grepl("\033", logged)))
})

test_that("boomer.log = 'console' keeps the historical behaviour (#142)", {
  withr::local_options(boomer.safe_print = TRUE, boomer.log = "console")
  console <- cli::ansi_strip(capture.output(boom(1 + 2 * 3)))
  expect_true(any(grepl("2 * 3", console, fixed = TRUE)))
})

test_that("boomer.log can tee to the console and a file at once (#142)", {
  withr::local_options(boomer.safe_print = TRUE)
  f <- withr::local_tempfile(fileext = ".txt")
  withr::local_options(boomer.log = c("console", f))

  console <- cli::ansi_strip(capture.output(boom(1 + 2 * 3)))
  logged <- readLines(f)

  # the same exploded line reaches both destinations
  expect_true(any(grepl("2 * 3", console, fixed = TRUE)))
  expect_true(any(grepl("2 * 3", logged, fixed = TRUE)))
})

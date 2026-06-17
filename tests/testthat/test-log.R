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

test_that("each file log entry is framed with a timestamp and a blank line (#142)", {
  withr::local_options(boomer.safe_print = TRUE)
  f <- withr::local_tempfile(fileext = ".log")
  withr::local_options(boomer.log = f)

  invisible(boom(1 + 2 * 3))
  invisible(boom(sqrt(16)))
  lines <- readLines(f)

  # one commented timestamp header per top-level call
  headers <- grep("^# [0-9]{4}-", lines)
  expect_length(headers, 2)
  # entries are separated by a single blank line, never two in a row
  blanks <- which(lines == "")
  expect_false(any(diff(blanks) == 1))
  # the header sits immediately before the entry's first trace line
  expect_match(lines[headers[2] - 1], "^$")
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

test_that("a freshly created log file gets a descriptive header, written once", {
  withr::local_options(boomer.safe_print = TRUE)
  f <- withr::local_tempfile(fileext = ".log")
  withr::local_options(boomer.log = f)

  invisible(boom(1 + 2 * 3))
  invisible(boom(sqrt(16)))
  lines <- readLines(f)

  # the header sits at the very top and describes the package
  expect_match(lines[1], "boomer R package", fixed = TRUE)
  # it is written exactly once, even across several appended entries
  expect_length(grep("boomer R package", lines, fixed = TRUE), 1)
  # the header precedes the first timestamped entry
  expect_lt(1, grep("^# [0-9]{4}-", lines)[1])
})

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

test_that("rig_in_place() updates the right S3 methods table (#141)", {
  # `format` is a base generic, so the registered copy of the method that S3
  # dispatch actually calls lives in base's S3 methods table, not in the
  # method's own namespace. Patching the method's namespace alone is not enough.
  fake_package(
    "boomerfakes3",
    unexported = list(format.boomerfakes3 = function(x, ...) {
      parts <- c("a", "b")
      paste(parts, collapse = "")
    }),
    s3 = list(c("format", "boomerfakes3")),
    attach = FALSE
  )
  rig_in_place(boomerfakes3:::format.boomerfakes3)

  obj <- structure(list(), class = "boomerfakes3")
  out <- cli::ansi_strip(capture.output(result <- format(obj)))

  # the rigged method ran during S3 dispatch and exploded its steps ...
  expect_true(any(grepl("format.boomerfakes3", out, fixed = TRUE)))
  expect_true(any(grepl("paste(parts, collapse", out, fixed = TRUE)))
  # ... while still returning the correct result
  expect_identical(result, "ab")
})
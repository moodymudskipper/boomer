# Manual / local-only checks that rig_in_place() patches the correct S3 methods
# table across different packages owning the generic vs. the method (#141).
#
# These are skipped on CI and CRAN: they rig functions of installed packages
# (rlang, vctrs) in place, which mutates the session and depends on those
# packages' internals, so they are unstable by nature.
#
# They check two things per combination:
#   * structural: the rigged copy lands in the S3 methods table that dispatch
#     actually reads (the table of the namespace owning the *generic*);
#   * behavioural: calling the generic explodes the rigged method's steps.
#
# A caveat about the "both in base" combination: rigging a base method in place
# only works behaviourally if the method (and its generic) are off the path that
# boomer's own output machinery uses. Rigging e.g. `format.*`, `rev.*` or
# `anyDuplicated.*` recurses to death, because boomer (and cli/styler) call those
# generics internally while rendering the boom output, so the rigged copy
# re-enters itself. We therefore use `as.Date.character`, which nothing in
# boomer's rendering touches. This is a pre-existing property of in-place rigging
# low-level base functions, independent of S3 table targeting.

rigged_in_table <- function(pkg, key) {
  tbl <- asNamespace(pkg)$.__S3MethodsTable__.
  isTRUE(attr(tbl[[key]], "boomer.rigged"))
}

booms <- function(generic_call, needle) {
  out <- cli::ansi_strip(capture.output(suppressWarnings(generic_call)))
  any(grepl(needle, out, fixed = TRUE))
}

test_that("method and generic both in base (#141)", {
  skip_on_ci()
  skip_on_cran()
  # `as.Date` is a base generic and `as.Date.character` a base method, so the
  # registered copy lives in base's S3 methods table.
  rig_in_place(as.Date.character)
  expect_true(rigged_in_table("base", "as.Date.character"))
  expect_true(booms(as.Date("2020-03-06"), "as.Date.character"))
})

test_that("method in rlang, generic in base: base table patched (#141)", {
  skip_on_ci()
  skip_on_cran()
  skip_if_not_installed("rlang")
  # `as.character.quosure` is defined in rlang but `as.character` is a base
  # generic, so the registered copy lives in base's S3 methods table, not
  # rlang's.
  rig_in_place(rlang:::as.character.quosure)
  expect_true(rigged_in_table("base", "as.character.quosure"))
  expect_true(booms(as.character(rlang::quo(x + 1)), "as.character.quosure"))
})

test_that("method and generic both in rlang: rigging the generic rigs methods (#141)", {
  skip_on_ci()
  skip_on_cran()
  skip_if_not_installed("rlang")
  # `as_bytes` and all its methods are defined in rlang.
  expect_message(rig_in_place(rlang:::as_bytes), "S3 generic")
  expect_true(rigged_in_table("rlang", "as_bytes.character"))
  expect_true(booms(rlang:::as_bytes("abc"), "as_bytes.character"))
})

test_that("method and generic both in another CRAN package (vctrs) (#141)", {
  skip_on_ci()
  skip_on_cran()
  skip_if_not_installed("vctrs")
  # `as_list_of` and its methods are defined in vctrs.
  expect_message(rig_in_place(vctrs:::as_list_of), "S3 generic")
  expect_true(rigged_in_table("vctrs", "as_list_of.list"))
  expect_true(booms(vctrs:::as_list_of(list(1, 2)), "as_list_of.list"))
})

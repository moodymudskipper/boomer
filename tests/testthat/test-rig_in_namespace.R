options(boomer.safe_print = TRUE)

# test_that("rig_in_namespace() works", {
#   skip_on_ci()
#   expect_snapshot({
#     fake_package("fake", list(
#       add2 = function(a, b) {
#         a + b
#       },
#       add4 = function(a, b, c, d) {
#         add2(a, b) + add2(c, d)
#       }
#     ))
#
#     options(boomer.print_args = TRUE)
#     rig_in_namespace(add2, add4)
#
#     # works
#     add4(1, 2, 3, 4)
#     options(boomer.print_args = FALSE)
#   })
# })

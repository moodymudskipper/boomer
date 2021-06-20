options(boom.safe_print = TRUE)

test_that("rig_in_namespace() works", {
  expect_snapshot({
    fake_package("fake", list(
      add2 = function(a, b) {
        a + b
      },
      add4 = function(a, b, c, d) {
        add2(a, b) + add2(c, d)
      },
      rec_factorial = function(x) {
        if(x == 1) return(1)
        x * rec_factorial(x-1)
      }
    ))

    rig_in_namespace(add2, add4, rec_factorial, print_args = TRUE)

    # works
    add4(1, 2, 3, 4)
  })
})

# boom()

    Code
      fun <- (function(x) {
        n <- 1 + 2 * 3
        sum(base::nchar(utils:::head(x, -n)))
      })
      rigged <- rig(fun)
      rigged(letters)
    Output
      <U+0001F4A3> +
      · <U+0001F4A3> *
      · <U+0001F4A5> 2 * 3
      [1] 6
      <U+0001F4A5> 1 + 2 * 3
      [1] 7
      <U+0001F4A3> sum
      · <U+0001F4A3> base::nchar
      · · <U+0001F4A3> utils:::head
      · · · <U+0001F4A3> -
      · · · <U+0001F4A5> -n
      [1] -7
      · · <U+0001F4A5> utils:::head(x, -n)
       [1] "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s"
      · <U+0001F4A5> base::nchar(utils:::head(x, -n))
       [1] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
      <U+0001F4A5> sum(base::nchar(utils:::head(x, -n)))
      [1] 19
      [1] 19

# functions created at runtime are boomed

    Code
      rig(foo2)(2)
    Message <simpleMessage>
      Not rigging undefined `SQRT()`.
    Output
      <U+0001F4A3> *
      <U+0001F4A5> x * 2
      [1] 4
      <U+0001F4A3> SQRT
      <U+0001F4A5> SQRT(x)
      [1] 2
      [1] 2


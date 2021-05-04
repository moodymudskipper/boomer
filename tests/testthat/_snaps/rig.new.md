# boom()

    Code
      fun <- (function(x) {
        n <- 1 + 2 * 3
        sum(base::nchar(utils:::head(x, -n)))
      })
    Code
      rigged <- rig(fun)
    Code
      rigged(letters)
    Output
      2 * 3
      [1] 6
      1 + 2 * 3
      [1] 7
      -n
      [1] -7
      utils:::head(x, -n)
       [1] "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s"
      base::nchar(utils:::head(x, -n))
       [1] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
      sum(base::nchar(utils:::head(x, -n)))
      [1] 19
    Output
      [1] 19


# boom()

    Code
      boom(1 + 2 * 3)
    Output
      2 * 3
      [1] 6
      1 + 2 * 3
      [1] 7
    Code
      boom(sum(base::nchar(utils:::head(letters, -3))))
    Output
      -3
      [1] -3
      utils:::head(letters, -3)
       [1] "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s"
      [20] "t" "u" "v" "w"
      base::nchar(utils:::head(letters, -3))
       [1] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
      sum(base::nchar(utils:::head(letters, -3)))
      [1] 23


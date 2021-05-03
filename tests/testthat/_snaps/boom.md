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

# boom() works with a global function

    Code
      fun <- (function(x) {
        x
      })
      boomer::boom(fun(1:3))
    Output
      1:3
      [1] 1 2 3
      fun(1:3)
      [1] 1 2 3

# boom() works with README examples

    Code
      boom(1 + (!1 * 2))
    Output
      1 * 2
      [1] 2
      !1 * 2
      [1] FALSE
      1 + (!1 * 2)
      [1] 1
    Code
      boom(subset(head(mtcars, 2), qsec > 17))
    Output
      head(mtcars, 2)
                    mpg cyl disp  hp drat    wt  qsec vs am gear carb
      Mazda RX4      21   6  160 110  3.9 2.620 16.46  0  1    4    4
      Mazda RX4 Wag  21   6  160 110  3.9 2.875 17.02  0  1    4    4
      qsec > 17
      [1] FALSE  TRUE
      subset(head(mtcars, 2), qsec > 17)
                    mpg cyl disp  hp drat    wt  qsec vs am gear carb
      Mazda RX4 Wag  21   6  160 110  3.9 2.875 17.02  0  1    4    4
    Code
      mtcars %>% head(2) %>% subset(qsec > 17) %>% boom()
    Output
      head(., 2)
                    mpg cyl disp  hp drat    wt  qsec vs am gear carb
      Mazda RX4      21   6  160 110  3.9 2.620 16.46  0  1    4    4
      Mazda RX4 Wag  21   6  160 110  3.9 2.875 17.02  0  1    4    4
      qsec > 17
      [1] FALSE  TRUE
      subset(., qsec > 17)
                    mpg cyl disp  hp drat    wt  qsec vs am gear carb
      Mazda RX4 Wag  21   6  160 110  3.9 2.875 17.02  0  1    4    4
      mtcars %>% head(2) %>% subset(qsec > 17)
                    mpg cyl disp  hp drat    wt  qsec vs am gear carb
      Mazda RX4 Wag  21   6  160 110  3.9 2.875 17.02  0  1    4    4
    Code
      boom(head(sapply(seq(10^6), sqrt)), print = str)
    Output
      10^6
       num 1e+06
      seq(10^6)
       int [1:1000000] 1 2 3 4 5 6 7 8 9 10 ...
      sapply(seq(10^6), sqrt)
       num [1:1000000] 1 1.41 1.73 2 2.24 ...
      head(sapply(seq(10^6), sqrt))
       num [1:6] 1 1.41 1.73 2 2.24 ...

# visible_only arg works

    Code
      boom(1 + invisible(1))
    Output
      invisible(1)
      [1] 1
      1 + invisible(1)
      [1] 2

---

    Code
      boom(1 + invisible(1), visible_only = TRUE)
    Output
      1 + invisible(1)
      [1] 2

# can debug failing pipes (#17)

    Code
      1 %>% identity() %>% I() %>% boomer::boom()
    Output
      identity(.)
      [1] 1
      I(.)
      [1] 1
      1 %>% identity() %>% I()
      [1] 1
    Code
      1 %>% identity() %>% missing_function() %>% I() %>% boomer::boom()
    Output
      I(.)
      Error: simpleError/error/condition
      1 %>% identity() %>% missing_function() %>% I()
      Error: simpleError/error/condition
    Error <simpleError>
      could not find function "missing_function"
    Code
      eagerly_failing_function <- (function(x) {
        stop("oops")
      })
      1 %>% identity() %>% eagerly_failing_function() %>% I() %>% boomer::boom()
    Output
      eagerly_failing_function(.)
      Error: simpleError/error/condition
      I(.)
      Error: simpleError/error/condition
      1 %>% identity() %>% eagerly_failing_function() %>% I()
      Error: simpleError/error/condition
    Error <simpleError>
      oops
    Code
      failing_function <- (function(x) {
        force(x)
        stop("oops")
      })
      1 %>% identity() %>% failing_function() %>% I() %>% boomer::boom()
    Output
      identity(.)
      [1] 1
      failing_function(.)
      Error: simpleError/error/condition
      I(.)
      Error: simpleError/error/condition
      1 %>% identity() %>% failing_function() %>% I()
      Error: simpleError/error/condition
    Error <simpleError>
      oops


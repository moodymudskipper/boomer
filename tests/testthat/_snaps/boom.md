# boom()

    Code
      boom(1 + 2 * 3)
    Output
      <U+0001F4A3> +
      · <U+0001F4A3> *
      · <U+0001F4A5> 2 * 3
      [1] 6
      <U+0001F4A5> 1 + 2 * 3
      [1] 7
    Code
      boom(sum(base::nchar(utils:::head(letters, -3))))
    Output
      <U+0001F4A3> sum
      · <U+0001F4A3> base::nchar
      · · <U+0001F4A3> utils:::head
      · · · <U+0001F4A3> -
      · · · <U+0001F4A5> -3
      [1] -3
      · · <U+0001F4A5> utils:::head(letters, -3)
       [1] "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s"
      [20] "t" "u" "v" "w"
      · <U+0001F4A5> base::nchar(utils:::head(letters, -3))
       [1] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
      <U+0001F4A5> sum(base::nchar(utils:::head(letters, -3)))
      [1] 23
    Code
      boom(for (i in 1:10) i)
    Output
      <U+0001F4A3> :
      <U+0001F4A5> 1:10
       [1]  1  2  3  4  5  6  7  8  9 10

# boom() works with a global function

    Code
      fun <- (function(x) {
        x
      })
      boomer::boom(fun(1:3))
    Output
      <U+0001F4A3> fun
      · <U+0001F4A3> :
      · <U+0001F4A5> 1:3
      [1] 1 2 3
      <U+0001F4A5> fun(1:3)
      [1] 1 2 3

# boom() works with README examples

    Code
      boom(1 + (!1 * 2))
    Output
      <U+0001F4A3> +
      · <U+0001F4A3> !
      · · <U+0001F4A3> *
      · · <U+0001F4A5> 1 * 2
      [1] 2
      · <U+0001F4A5> !1 * 2
      [1] FALSE
      <U+0001F4A5> 1 + (!1 * 2)
      [1] 1
    Code
      boom(subset(head(mtcars, 2), qsec > 17))
    Output
      <U+0001F4A3> subset
      · <U+0001F4A3> head
      · <U+0001F4A5> head(mtcars, 2)
                    mpg cyl disp  hp drat    wt  qsec vs am gear carb
      Mazda RX4      21   6  160 110  3.9 2.620 16.46  0  1    4    4
      Mazda RX4 Wag  21   6  160 110  3.9 2.875 17.02  0  1    4    4
      · <U+0001F4A3> >
      · <U+0001F4A5> qsec > 17
      [1] FALSE  TRUE
      <U+0001F4A5> subset(head(mtcars, 2), qsec > 17)
                    mpg cyl disp  hp drat    wt  qsec vs am gear carb
      Mazda RX4 Wag  21   6  160 110  3.9 2.875 17.02  0  1    4    4
    Code
      mtcars %>% head(2) %>% subset(qsec > 17) %>% boom()
    Output
      <U+0001F4A3> %>%
      · <U+0001F4A3> subset
      · · <U+0001F4A3> head
      · · <U+0001F4A5> head(., 2)
                    mpg cyl disp  hp drat    wt  qsec vs am gear carb
      Mazda RX4      21   6  160 110  3.9 2.620 16.46  0  1    4    4
      Mazda RX4 Wag  21   6  160 110  3.9 2.875 17.02  0  1    4    4
      · · <U+0001F4A3> >
      · · <U+0001F4A5> qsec > 17
      [1] FALSE  TRUE
      · <U+0001F4A5> subset(., qsec > 17)
                    mpg cyl disp  hp drat    wt  qsec vs am gear carb
      Mazda RX4 Wag  21   6  160 110  3.9 2.875 17.02  0  1    4    4
      <U+0001F4A5> mtcars %>% head(2) %>% subset(qsec > 17)
                    mpg cyl disp  hp drat    wt  qsec vs am gear carb
      Mazda RX4 Wag  21   6  160 110  3.9 2.875 17.02  0  1    4    4
    Code
      boom(head(sapply(seq(10^6), sqrt)), print = str)
    Output
      <U+0001F4A3> head
      · <U+0001F4A3> sapply
      · · <U+0001F4A3> seq
      · · · <U+0001F4A3> ^
      · · · <U+0001F4A5> 10^6
       num 1e+06
      · · <U+0001F4A5> seq(10^6)
       int [1:1000000] 1 2 3 4 5 6 7 8 9 10 ...
      · <U+0001F4A5> sapply(seq(10^6), sqrt)
       num [1:1000000] 1 1.41 1.73 2 2.24 ...
      <U+0001F4A5> head(sapply(seq(10^6), sqrt))
       num [1:6] 1 1.41 1.73 2 2.24 ...

# visible_only arg works

    Code
      boom(1 + invisible(1))
    Output
      <U+0001F4A3> +
      · <U+0001F4A3> invisible
      · <U+0001F4A5> invisible(1)
      [1] 1
      <U+0001F4A5> 1 + invisible(1)
      [1] 2

---

    Code
      boom(1 + invisible(1), visible_only = TRUE)
    Output
      <U+0001F4A3> +
      · <U+0001F4A3> invisible
      <U+0001F4A5> 1 + invisible(1)
      [1] 2

# can debug failing pipes (#17)

    Code
      1 %>% identity() %>% I() %>% boomer::boom()
    Output
      <U+0001F4A3> %>%
      · <U+0001F4A3> I
      · · <U+0001F4A3> identity
      · · <U+0001F4A5> identity(.)
      [1] 1
      · <U+0001F4A5> I(.)
      [1] 1
      <U+0001F4A5> 1 %>% identity() %>% I()
      [1] 1
    Code
      1 %>% identity() %>% missing_function() %>% I() %>% boomer::boom()
    Message <simpleMessage>
      Not booming undefined `missing_function()`.
    Output
      <U+0001F4A3> %>%
      · <U+0001F4A3> I
      · <U+0001F4A5> I(.)
      Error: simpleError/error/condition
      <U+0001F4A5> 1 %>% identity() %>% missing_function() %>% I()
      Error: simpleError/error/condition
    Error <simpleError>
      could not find function "missing_function"
    Code
      eagerly_failing_function <- (function(x) {
        stop("oops")
      })
      1 %>% identity() %>% eagerly_failing_function() %>% I() %>% boomer::boom()
    Output
      <U+0001F4A3> %>%
      · <U+0001F4A3> I
      · · <U+0001F4A3> eagerly_failing_function
      · · <U+0001F4A5> eagerly_failing_function(.)
      Error: simpleError/error/condition
      · <U+0001F4A5> I(.)
      Error: simpleError/error/condition
      <U+0001F4A5> 1 %>% identity() %>% eagerly_failing_function() %>% I()
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
      <U+0001F4A3> %>%
      · <U+0001F4A3> I
      · · <U+0001F4A3> failing_function
      · · · <U+0001F4A3> identity
      · · · <U+0001F4A5> identity(.)
      [1] 1
      · · <U+0001F4A5> failing_function(.)
      Error: simpleError/error/condition
      · <U+0001F4A5> I(.)
      Error: simpleError/error/condition
      <U+0001F4A5> 1 %>% identity() %>% failing_function() %>% I()
      Error: simpleError/error/condition
    Error <simpleError>
      oops

# functions created at runtime are boomed

    Code
      boom({
        x <- 2
        x <- x * 2
        SQRT <- sqrt
        SQRT(x)
      })
    Message <simpleMessage>
      Not booming undefined `SQRT()`.
    Output
      <U+0001F4A3> *
      <U+0001F4A5> x * 2
      [1] 4
      <U+0001F4A3> SQRT
      <U+0001F4A5> SQRT(x)
      [1] 2


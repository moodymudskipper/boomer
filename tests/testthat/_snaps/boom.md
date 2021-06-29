# boom() works

    Code
      boom(1 + 2 * 3)
    Output
      <  1 + 2 * 3 
      . <  >  2 * 3 
      . [1] 6
      . 
      >  1 + 2 * 3 
      [1] 7
      
      [1] 7
    Code
      boom(sum(base::nchar(utils:::head(letters, -3))))
    Output
      <  sum(base::nchar(utils:::head(letters, -3))) 
      . <  base::nchar(utils:::head(letters, -3)) 
      . . <  utils:::head(letters, -3) 
      . . . <  >  -3 
      . . . [1] -3
      . . . 
      . . >  utils:::head(letters, -3) 
      . .  [1] "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s"
      . . [20] "t" "u" "v" "w"
      . . 
      . >  base::nchar(utils:::head(letters, -3)) 
      .  [1] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
      . 
      >  sum(base::nchar(utils:::head(letters, -3))) 
      [1] 23
      
      [1] 23
    Code
      boom(for (i in 1:10) i)
    Output
      <  >  1:10 
       [1]  1  2  3  4  5  6  7  8  9 10
      

# boom() works with a namespaced non function

    Code
      boom(base::letters)
    Output
       [1] "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s"
      [20] "t" "u" "v" "w" "x" "y" "z"
    Code
      boom(base:::letters)
    Output
       [1] "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s"
      [20] "t" "u" "v" "w" "x" "y" "z"
    Code
      boom(base::letters, clock = TRUE)
    Output
       [1] "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s"
      [20] "t" "u" "v" "w" "x" "y" "z"
    Code
      boom(base:::letters, clock = TRUE)
    Output
       [1] "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s"
      [20] "t" "u" "v" "w" "x" "y" "z"

# boom() works with a global function

    Code
      fun <- (function(x) {
        x
      })
      boom(fun(1:3))
    Output
      <  fun(1:3) 
      . <  >  1:3 
      . [1] 1 2 3
      . 
      >  fun(1:3) 
      [1] 1 2 3
      
      [1] 1 2 3

# boom() works with README examples

    Code
      boom(1 + (!1 * 2))
    Output
      <  1 + (!1 * 2) 
      . <  !1 * 2 
      . . <  >  1 * 2 
      . . [1] 2
      . . 
      . >  !1 * 2 
      . [1] FALSE
      . 
      >  1 + (!1 * 2) 
      [1] 1
      
      [1] 1
    Code
      boom(subset(head(mtcars, 2), qsec > 17))
    Output
      <  subset(head(mtcars, 2), qsec > 17) 
      . <  >  head(mtcars, 2) 
      .               mpg cyl disp  hp drat    wt  qsec vs am gear carb
      . Mazda RX4      21   6  160 110  3.9 2.620 16.46  0  1    4    4
      . Mazda RX4 Wag  21   6  160 110  3.9 2.875 17.02  0  1    4    4
      . 
      . <  >  qsec > 17 
      . [1] FALSE  TRUE
      . 
      >  subset(head(mtcars, 2), qsec > 17) 
                    mpg cyl disp  hp drat    wt  qsec vs am gear carb
      Mazda RX4 Wag  21   6  160 110  3.9 2.875 17.02  0  1    4    4
      
                    mpg cyl disp  hp drat    wt  qsec vs am gear carb
      Mazda RX4 Wag  21   6  160 110  3.9 2.875 17.02  0  1    4    4
    Code
      mtcars %>% head(2) %>% subset(qsec > 17) %>% boom()
    Output
      <  mtcars %>%... 
      . <  subset(., qsec > 17) 
      . . <  head(., 2) 
      . . >  head(., 2) 
      . .               mpg cyl disp  hp drat    wt  qsec vs am gear carb
      . . Mazda RX4      21   6  160 110  3.9 2.620 16.46  0  1    4    4
      . . Mazda RX4 Wag  21   6  160 110  3.9 2.875 17.02  0  1    4    4
      . . 
      . . <  >  qsec > 17 
      . . [1] FALSE  TRUE
      . . 
      . >  subset(., qsec > 17) 
      .               mpg cyl disp  hp drat    wt  qsec vs am gear carb
      . Mazda RX4 Wag  21   6  160 110  3.9 2.875 17.02  0  1    4    4
      . 
      >  mtcars %>%
           head(2) %>%
           subset(qsec > 17) 
                    mpg cyl disp  hp drat    wt  qsec vs am gear carb
      Mazda RX4 Wag  21   6  160 110  3.9 2.875 17.02  0  1    4    4
      
                    mpg cyl disp  hp drat    wt  qsec vs am gear carb
      Mazda RX4 Wag  21   6  160 110  3.9 2.875 17.02  0  1    4    4
    Code
      boom(head(sapply(seq(10^2), sqrt)), print = str)
    Output
      <  head(sapply(seq(10^2), sqrt)) 
      . <  sapply(seq(10^2), sqrt) 
      . . <  seq(10^2) 
      . . . <  >  10^2 
      . . .  num 100
      . . . 
      . . >  seq(10^2) 
      . .  int [1:100] 1 2 3 4 5 6 7 8 9 10 ...
      . . 
      . >  sapply(seq(10^2), sqrt) 
      .  num [1:100] 1 1.41 1.73 2 2.24 ...
      . 
      >  head(sapply(seq(10^2), sqrt)) 
       num [1:6] 1 1.41 1.73 2 2.24 ...
      
      [1] 1.000000 1.414214 1.732051 2.000000 2.236068 2.449490

# print arg works

    Code
      boom(data.frame(a = 1, b = 2), print = str)
    Output
      <  >  data.frame(a = 1, b = 2) 
      'data.frame':	1 obs. of  2 variables:
       $ a: num 1
       $ b: num 2
      
        a b
      1 1 2

# print arg works with list

    Code
      boom(data.frame(a = 1, b = 2), print = list(data.frame = str))
    Output
      <  >  data.frame(a = 1, b = 2) 
      'data.frame':	1 obs. of  2 variables:
       $ a: num 1
       $ b: num 2
      
        a b
      1 1 2
    Code
      boom(data.frame(a = 1, b = 2), print = list(data.frame = str, print))
    Output
      <  >  data.frame(a = 1, b = 2) 
      'data.frame':	1 obs. of  2 variables:
       $ a: num 1
       $ b: num 2
      
        a b
      1 1 2
    Code
      boom(data.frame(a = 1, b = 2), print = list(str))
    Output
      <  >  data.frame(a = 1, b = 2) 
      'data.frame':	1 obs. of  2 variables:
       $ a: num 1
       $ b: num 2
      
        a b
      1 1 2

# visible_only arg works

    Code
      boom(1 + invisible(1))
    Output
      <  1 + invisible(1) 
      . <  >  invisible(1) 
      . [1] 1
      . 
      >  1 + invisible(1) 
      [1] 2
      
      [1] 2
    Code
      options(boomer.visible_only = TRUE)
      boom(1 + invisible(1))
    Output
      <  1 + invisible(1) 
      . <  >  invisible(1) 
      >  1 + invisible(1) 
      [1] 2
      
      [1] 2
    Code
      options(boomer.visible_only = FALSE)

# can debug failing pipes (#17)

    Code
      1 %>% identity() %>% I() %>% boomer::boom()
    Output
      <  1 %>%... 
      . <  I(.) 
      . . <  identity(.) 
      . . >  identity(.) 
      . . [1] 1
      . . 
      . >  I(.) 
      . [1] 1
      . 
      >  1 %>%
           identity() %>%
           I() 
      [1] 1
      
      [1] 1
    Code
      1 %>% identity() %>% missing_function() %>% I() %>% boomer::boom()
    Output
      <  1 %>%... 
      . <  I(.) 
      . >  I(.) 
      Error: simpleError/error/condition
      >  1 %>%
           identity() %>%
           missing_function() %>%
           I() 
      Error: simpleError/error/condition
    Error <simpleError>
      could not find function "missing_function"
    Code
      eagerly_failing_function <- (function(x) {
        stop("oops")
      })
      1 %>% identity() %>% eagerly_failing_function() %>% I() %>% boomer::boom()
    Output
      <  1 %>%... 
      . <  I(.) 
      . . <  eagerly_failing_function(.) 
      . . >  eagerly_failing_function(.) 
      Error: simpleError/error/condition
      . >  I(.) 
      Error: simpleError/error/condition
      >  1 %>%
           identity() %>%
           eagerly_failing_function() %>%
           I() 
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
      <  1 %>%... 
      . <  I(.) 
      . . <  failing_function(.) 
      . . . <  identity(.) 
      . . . >  identity(.) 
      . . . [1] 1
      . . . 
      . . >  failing_function(.) 
      Error: simpleError/error/condition
      . >  I(.) 
      Error: simpleError/error/condition
      >  1 %>%
           identity() %>%
           failing_function() %>%
           I() 
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
    Output
      <  >  x * 2 
      [1] 4
      
      <  >  SQRT(x) 
      [1] 2
      
      [1] 2

# assignments work

    Code
      boom({
        x <- 1 + 2
        y <- quote(a)
        u = 1 + 2
        v = quote(a)
      })
    Output
      <  >  1 + 2 
      [1] 3
      
      <  >  quote(a) 
      a
      
      <  >  quote(a) 
      a
      
      <  >  1 + 2 
      [1] 3
      
      <  >  quote(a) 
      a
      
      <  >  quote(a) 
      a
      
      a
    Code
      options(boomer.ignore = NULL)
      boom({
        x <- 1 + 2
        y <- quote(a)
        u = 1 + 2
        v = quote(a)
      })
    Output
      <  {... 
      . <  x <- 1 + 2 
      . . <  >  1 + 2 
      . . [1] 3
      . . 
      . >  x <- 1 + 2 
      . [1] 3
      . 
      . <  y <- quote(a) 
      . . <  >  quote(a) 
      . . a
      . . 
      . . <  >  quote(a) 
      . . a
      . . 
      . >  y <- quote(a) 
      . a
      . 
      . <  u <- 1 + 2 
      . . <  >  1 + 2 
      . . [1] 3
      . . 
      . >  u <- 1 + 2 
      . [1] 3
      . 
      . <  v <- quote(a) 
      . . <  >  quote(a) 
      . . a
      . . 
      . . <  >  quote(a) 
      . . a
      . . 
      . >  v <- quote(a) 
      . a
      . 
      >  {
           x <- 1 + 2
           y <- quote(a)
           u <- 1 + 2
           v <- quote(a)
         } 
      a
      
      a
    Code
      options(boomer.ignore = c("~", "{", "(", "<-", "<<-", "="))

# multi-line calls are collapsed properly

    Code
      boom(c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0))
    Output
      <  >  c(
              0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,
              0
            ) 
       [1] 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0
      
       [1] 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0
    Code
      boom(c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, c(0, 1)))
    Output
      <  c(... 
      . <  >  c(0, 1) 
      . [1] 0 1
      . 
      >  c(
           0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,
           c(0, 1)
         ) 
       [1] 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
      
       [1] 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1

# long calls are trimmed

    Code
      aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa <- 1
      bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb <- 2
      boom(aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa * 2 +
        bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb)
    Output
      <  aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa * 2 + bbbbbbbbbbbbbbbbbb... 
      . <  >  aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa * 2 
      . [1] 2
      . 
      >  aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa * 2 + bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb 
      [1] 4
      
      [1] 4

# `boomer.abbreviate` option works

    Code
      options(boomer.abbreviate = TRUE)
      boom(1 + 2 * 3)
    Output
      <  + 
      . <  >  2 * 3 
      . [1] 6
      . 
      >  1 + 2 * 3 
      [1] 7
      
      [1] 7
    Code
      boom(c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0))
    Output
      <  >  c(
              0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,
              0
            ) 
       [1] 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0
      
       [1] 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0
    Code
      boom(c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, c(0, 1)))
    Output
      <  c 
      . <  >  c(0, 1) 
      . [1] 0 1
      . 
      >  c(
           0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,
           c(0, 1)
         ) 
       [1] 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
      
       [1] 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
    Code
      aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa <- 1
      bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb <- 2
      boom(aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa * 2 +
        bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb)
    Output
      <  + 
      . <  >  aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa * 2 
      . [1] 2
      . 
      >  aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa * 2 + bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb 
      [1] 4
      
      [1] 4
    Code
      options(boomer.abbreviate = FALSE)


# rig() works

    Code
      fun <- (function(x) {
        n <- 1 + 2 * 3
        sum(base::nchar(utils:::head(x, -n)))
      })
      rigged <- rig(fun)
      rigged(letters)
    Output
      { fun
      <  1 + 2 * 3 
      . <  >  2 * 3 
      . [1] 6
      . 
      >  1 + 2 * 3 
      [1] 7
      
      <  sum(base::nchar(utils:::head(x, -n))) 
      . <  base::nchar(utils:::head(x, -n)) 
      . . <  utils:::head(x, -n) 
      . . . x :
      . . .  [1] "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s"
      . . . [20] "t" "u" "v" "w" "x" "y" "z"
      . . . <  >  -n 
      . . . [1] -7
      . . . 
      . . >  utils:::head(x, -n) 
      . .  [1] "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s"
      . . 
      . >  base::nchar(utils:::head(x, -n)) 
      .  [1] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
      . 
      >  sum(base::nchar(utils:::head(x, -n))) 
      [1] 19
      
      } fun
      [1] 19

# rigger() works

    Code
      fun <- (function(x) {
        n <- 1 + 2 * 3
        sum(base::nchar(utils:::head(x, -n)))
      })
      r <- rigger()
      r[["print"]] <- NULL
      r
    Output
      # rigger object, use the syntax `rigger(...) + function(...) {...}` to create a rigged function conveniently
      
      $clock
      NULL
      
    Code
      rigged <- rigger() + fun
      rigged(letters)
    Output
      { e2
      <  1 + 2 * 3 
      . <  >  2 * 3 
      . [1] 6
      . 
      >  1 + 2 * 3 
      [1] 7
      
      <  sum(base::nchar(utils:::head(x, -n))) 
      . <  base::nchar(utils:::head(x, -n)) 
      . . <  utils:::head(x, -n) 
      . . . x :
      . . .  [1] "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s"
      . . . [20] "t" "u" "v" "w" "x" "y" "z"
      . . . <  >  -n 
      . . . [1] -7
      . . . 
      . . >  utils:::head(x, -n) 
      . .  [1] "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s"
      . . 
      . >  base::nchar(utils:::head(x, -n)) 
      .  [1] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
      . 
      >  sum(base::nchar(utils:::head(x, -n))) 
      [1] 19
      
      } e2
      [1] 19

# functions created at runtime are boomed

    Code
      rig(foo2)(2)
    Message <simpleMessage>
      `SQRT()` is undefined outside of `foo2()` and its output might not be shown.
    Output
      { foo2
      x :
      [1] 2
      <  >  x * 2 
      [1] 4
      
      <  >  SQRT(x) 
      [1] 2
      
      } foo2
      [1] 2


# rig_in_namespace() works

    Code
      fake_package("fake", list(add2 = function(a, b) {
        a + b
      }, add4 = function(a, b, c, d) {
        add2(a, b) + add2(c, d)
      }, rec_factorial = function(x) {
        if (x == 1) return(1)
        x * rec_factorial(x - 1)
      }))
      rig_in_namespace(add2, add4, rec_factorial, print_args = TRUE)
      add4(1, 2, 3, 4)
    Output
      { add4
      < +
      . < add2
      . . { add2
      . . < +
      . . a :
      . . [1] 1
      . . b :
      . . [1] 2
      . . > a + b
      . . [1] 3
      . . 
      . . } add2
      . a :
      . [1] 1
      . b :
      . [1] 2
      . > add2(a, b)
      . [1] 3
      . 
      . < add2
      . . { add2
      . . < +
      . . a :
      . . [1] 3
      . . b :
      . . [1] 4
      . . > a + b
      . . [1] 7
      . . 
      . . } add2
      . c :
      . [1] 3
      . d :
      . [1] 4
      . > add2(c, d)
      . [1] 7
      . 
      > add2(a, b) + add2(c, d)
      [1] 10
      
      } add4
      [1] 10


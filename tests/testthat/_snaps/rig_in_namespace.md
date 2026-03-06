# rig_in_place() works

    Code
      fact <- (function(n) {
        if (n <= 1) 1 else n * fact(n - 1)
      })
      rig_in_place(fact)
      fact(3)
    Output
      [1] 6


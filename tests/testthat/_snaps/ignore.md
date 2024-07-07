# boom.ignore

    Code
      boom((1 + 1))
    Output
      <  >  1 + 1 
      [1] 2
      
      [1] 2
    Code
      opts <- options(boomer.ignore = NULL)
      boom((1 + 1))
    Output
      <  (1 + 1) 
      . <  >  1 + 1 
      . [1] 2
      . 
      >  (1 + 1) 
      [1] 2
      
      [1] 2
    Code
      options(boomer.ignore = list(`(` = `(`))
      boom((1 + 1))
    Output
      <  >  1 + 1 
      [1] 2
      
      [1] 2
    Code
      options(boomer.ignore = opts$boomer.ignore)

# boom.ignore_args

    Code
      options(boomer.ignore_args = NULL)
      data.frame(a = 1:3) %>% transform(b = a + 1) %>% boom()
    Output
      <  data.frame(a = 1:3) %>% transform(b = a + 1) 
      . <  transform(., b = a + 1) 
      . . <  data.frame(a = 1:3) 
      . . . <  >  1:3 
      . . . [1] 1 2 3
      . . . 
      . . >  data.frame(a = 1:3) 
      . .   a
      . . 1 1
      . . 2 2
      . . 3 3
      . . 
      . . <  >  a + 1 
      . . [1] 2 3 4
      . . 
      . >  transform(., b = a + 1) 
      .   a b
      . 1 1 2
      . 2 2 3
      . 3 3 4
      . 
      >  data.frame(a = 1:3) %>% transform(b = a + 1) 
        a b
      1 1 2
      2 2 3
      3 3 4
      
        a b
      1 1 2
      2 2 3
      3 3 4
    Code
      options(boomer.ignore_args = list(transform))
      data.frame(a = 1:3) %>% transform(b = a + 1) %>% boom()
    Output
      <  data.frame(a = 1:3) %>% transform(b = a + 1) 
      . . <  data.frame(a = 1:3) 
      . . . <  >  1:3 
      . . . [1] 1 2 3
      . . . 
      . . >  data.frame(a = 1:3) 
      . .   a
      . . 1 1
      . . 2 2
      . . 3 3
      . . 
      . <  >  transform(., b = a + 1) 
      .   a b
      . 1 1 2
      . 2 2 3
      . 3 3 4
      . 
      >  data.frame(a = 1:3) %>% transform(b = a + 1) 
        a b
      1 1 2
      2 2 3
      3 3 4
      
        a b
      1 1 2
      2 2 3
      3 3 4
    Code
      options(boomer.ignore_args = "transform")
      data.frame(a = 1:3) %>% transform(b = a + 1) %>% boom()
    Output
      <  data.frame(a = 1:3) %>% transform(b = a + 1) 
      . . <  data.frame(a = 1:3) 
      . . . <  >  1:3 
      . . . [1] 1 2 3
      . . . 
      . . >  data.frame(a = 1:3) 
      . .   a
      . . 1 1
      . . 2 2
      . . 3 3
      . . 
      . <  >  transform(., b = a + 1) 
      .   a b
      . 1 1 2
      . 2 2 3
      . 3 3 4
      . 
      >  data.frame(a = 1:3) %>% transform(b = a + 1) 
        a b
      1 1 2
      2 2 3
      3 3 4
      
        a b
      1 1 2
      2 2 3
      3 3 4
    Code
      options(boomer.ignore_args = NULL)


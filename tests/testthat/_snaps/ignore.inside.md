# boom.ignore.inside

    Code
      options(boomer.ignore.inside = NULL)
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
      options(boomer.ignore.inside = list(transform))
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
      options(boomer.ignore.inside = NULL)


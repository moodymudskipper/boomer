# Print the Output of Intermediate Steps of a Call

- `boom()` prints the intermediate results of a call or a code chunk.

- `rig()` creates a copy of a function which will display the
  intermediate results of all the calls of it body.

- `rig_in_namespace()` rigs a namespaced function in place, so its
  always verbose even when called by other existing functions. It is
  especially handy for package development. To undo, call `load_all()`
  for the development package or
  [`pkgload::unload()`](https://pkgload.r-lib.org/reference/unload.html)
  on other packages, or restart the session if your rigged a base
  package. Shouldn't be used on S3 generics, but works on S3 methods.

- `rigger()` provides a convenient way to rig an anonymous function by
  using the `rigger(...) + function(...) {...}` syntax.

## Usage

``` r
boom(expr, clock = NULL, print = NULL)

rig(fun, clock = NULL, print = NULL)

rigger(clock = NULL, print = NULL)

rig_in_namespace(..., clock = NULL, print = NULL)
```

## Arguments

- expr:

  call to explode

- clock:

  whether to time intermediate steps. Defaults to
  `getOption("boomer.clock")` evaluated at run time (`FALSE` unless you
  change it). The execution time of a step doesn't include the execution
  time of its previously printed sub-steps.

- print:

  A function, a formula or a list of functions or formulas, used to
  modify the way the output is printed. Defaults to
  `getOption("boomer.print")` evaluated at run time
  ([`base::print`](https://rdrr.io/r/base/print.html) unless you change
  it)'.

- fun:

  function ro `rig()`

- ...:

  Functions to rig in their namespace

  If the `print` argument is a function, it will be used to print, or to
  transform the output before it's printed. Use `invisible` to display
  nothing, useful possibilities are `str` or
  [`dplyr::glimpse`](https://pillar.r-lib.org/reference/glimpse.html).

  *rlang*'s formula notation is supported, so for instance you can type:
  `print = ~ dplyr::glimpse(., width = 50)`.

  Sometimes you might want to print a specific type of object in a
  custom way, in this case you can provide a named list, if you provide
  an unnamed element it will be used as the default, and named elements
  will define how objects of the given S3 class are printed. For
  instance `print = list(str, data.frame = tibble::as_tibble)`

## Value

`boom()` returns the output of the call. `rig()` returns the modified
input function. `rig_in_namespace()` returns `invisible(NULL)` and is
called for side effects. `rigger()` returns a list containing the
arguments, with the class "rigger" to enable `+.rigger` and
`print.rigger`

## Examples

``` r
# explode a simple call
boom(subset(head(mtcars, 2), qsec > 17))
#> 💣 subset(head(mtcars, 2), qsec > 17) 
#> · 💣 💥 head(mtcars, 2) 
#> ·               mpg cyl disp  hp drat    wt  qsec vs am gear carb
#> · Mazda RX4      21   6  160 110  3.9 2.620 16.46  0  1    4    4
#> · Mazda RX4 Wag  21   6  160 110  3.9 2.875 17.02  0  1    4    4
#> · 
#> · 💣 💥 qsec > 17 
#> · [1] FALSE  TRUE
#> · 
#> 💥 subset(head(mtcars, 2), qsec > 17) 
#>               mpg cyl disp  hp drat    wt  qsec vs am gear carb
#> Mazda RX4 Wag  21   6  160 110  3.9 2.875 17.02  0  1    4    4
#> 
#>               mpg cyl disp  hp drat    wt  qsec vs am gear carb
#> Mazda RX4 Wag  21   6  160 110  3.9 2.875 17.02  0  1    4    4

# clock calls and customize how to print output
boom(subset(head(mtcars, 2), qsec > 17), clock = TRUE, print = str)
#> 💣 subset(head(mtcars, 2), qsec > 17) 
#> · 💣 💥 head(mtcars, 2) 
#> time: 0.195 ms
#> · 'data.frame': 2 obs. of  11 variables:
#> ·  $ mpg : num  21 21
#> ·  $ cyl : num  6 6
#> ·  $ disp: num  160 160
#> ·  $ hp  : num  110 110
#> ·  $ drat: num  3.9 3.9
#> ·  $ wt  : num  2.62 2.88
#> ·  $ qsec: num  16.5 17
#> ·  $ vs  : num  0 0
#> ·  $ am  : num  1 1
#> ·  $ gear: num  4 4
#> ·  $ carb: num  4 4
#> · 
#> · 💣 💥 qsec > 17 
#> time: 0.011 ms
#> ·  logi [1:2] FALSE TRUE
#> · 
#> 💥 subset(head(mtcars, 2), qsec > 17) 
#> time: 0.288 ms
#> 'data.frame':    1 obs. of  11 variables:
#>  $ mpg : num 21
#>  $ cyl : num 6
#>  $ disp: num 160
#>  $ hp  : num 110
#>  $ drat: num 3.9
#>  $ wt  : num 2.88
#>  $ qsec: num 17
#>  $ vs  : num 0
#>  $ am  : num 1
#>  $ gear: num 4
#>  $ carb: num 4
#> 
#>               mpg cyl disp  hp drat    wt  qsec vs am gear carb
#> Mazda RX4 Wag  21   6  160 110  3.9 2.875 17.02  0  1    4    4

# print str only for data frames
boom(subset(head(mtcars, 2), qsec > 17), print = list(data.frame = str))
#> 💣 subset(head(mtcars, 2), qsec > 17) 
#> · 💣 💥 head(mtcars, 2) 
#> · 'data.frame': 2 obs. of  11 variables:
#> ·  $ mpg : num  21 21
#> ·  $ cyl : num  6 6
#> ·  $ disp: num  160 160
#> ·  $ hp  : num  110 110
#> ·  $ drat: num  3.9 3.9
#> ·  $ wt  : num  2.62 2.88
#> ·  $ qsec: num  16.5 17
#> ·  $ vs  : num  0 0
#> ·  $ am  : num  1 1
#> ·  $ gear: num  4 4
#> ·  $ carb: num  4 4
#> · 
#> · 💣 💥 qsec > 17 
#> · [1] FALSE  TRUE
#> · 
#> 💥 subset(head(mtcars, 2), qsec > 17) 
#> 'data.frame':    1 obs. of  11 variables:
#>  $ mpg : num 21
#>  $ cyl : num 6
#>  $ disp: num 160
#>  $ hp  : num 110
#>  $ drat: num 3.9
#>  $ wt  : num 2.88
#>  $ qsec: num 17
#>  $ vs  : num 0
#>  $ am  : num 1
#>  $ gear: num 4
#>  $ carb: num 4
#> 
#>               mpg cyl disp  hp drat    wt  qsec vs am gear carb
#> Mazda RX4 Wag  21   6  160 110  3.9 2.875 17.02  0  1    4    4

# rig an existing function
rig(ave)(warpbreaks$breaks, warpbreaks$wool)
#> `FUN()` is undefined outside of `ave()` and its output might not be shown.
#> 👇 ave
#> 💣 if (missing(...)) {... 
#> · 💣 💥 missing(...) 
#> · [1] FALSE
#> · 
#> · 💣 💥 interaction(..., drop = TRUE) 
#> ·  [1] A A A A A A A A A A A A A A A A A A A A A A A A A A A B B B B B B B B B B B
#> · [39] B B B B B B B B B B B B B B B B
#> · Levels: A B
#> · 
#> · 💣 lapply(split(x, g), FUN) 
#> · · x :
#> · ·  [1] 26 30 54 25 70 52 51 26 67 18 21 29 17 12 18 35 30 36 36 21 24 18 10 43 28
#> · · [26] 15 26 27 14 29 19 29 31 41 20 44 42 26 19 16 39 28 21 39 29 20 21 24 17 13
#> · · [51] 15 15 16 28
#> · · 💣 💥 split(x, g) 
#> · · $A
#> · ·  [1] 26 30 54 25 70 52 51 26 67 18 21 29 17 12 18 35 30 36 36 21 24 18 10 43 28
#> · · [26] 15 26
#> · · 
#> · · $B
#> · ·  [1] 27 14 29 19 29 31 41 20 44 42 26 19 16 39 28 21 39 29 20 21 24 17 13 15 15
#> · · [26] 16 28
#> · · 
#> · · 
#> · 💥 lapply(split(x, g), FUN) 
#> · $A
#> · [1] 31.03704
#> · 
#> · $B
#> · [1] 25.25926
#> · 
#> · 
#> 💥 if (missing(...)) {
#>      x[] <- FUN(x)
#>    } else {
#>      g <- interaction(..., drop = TRUE)
#>      split(x, g) <- lapply(split(x, g), FUN)
#>    } 
#> $A
#> [1] 31.03704
#> 
#> $B
#> [1] 25.25926
#> 
#> 
#> 👆 ave
#>  [1] 31.03704 31.03704 31.03704 31.03704 31.03704 31.03704 31.03704 31.03704
#>  [9] 31.03704 31.03704 31.03704 31.03704 31.03704 31.03704 31.03704 31.03704
#> [17] 31.03704 31.03704 31.03704 31.03704 31.03704 31.03704 31.03704 31.03704
#> [25] 31.03704 31.03704 31.03704 25.25926 25.25926 25.25926 25.25926 25.25926
#> [33] 25.25926 25.25926 25.25926 25.25926 25.25926 25.25926 25.25926 25.25926
#> [41] 25.25926 25.25926 25.25926 25.25926 25.25926 25.25926 25.25926 25.25926
#> [49] 25.25926 25.25926 25.25926 25.25926 25.25926 25.25926

# rig an anonymous function
fun1 <- rigger() + function(x) x + 1 + 2 # same as rig(function(x) x + 1 + 2))
fun1(1)
#> 👇 e2
#> 💣 x + 1 + 2 
#> · x :
#> · [1] 1
#> · 💣 💥 x + 1 
#> · [1] 2
#> · 
#> 💥 x + 1 + 2 
#> [1] 4
#> 
#> 👆 e2
#> [1] 4
fun2 <- rigger(TRUE, typeof) + function(x) x + 1 + 2
fun2(1)
#> 👇 e2
#> 💣 x + 1 + 2 
#> · x :
#> · [1] "double"
#> · 💣 💥 x + 1 
#> time: 0.011 ms
#> · [1] "double"
#> · 
#> 💥 x + 1 + 2 
#> time: 0.034 s
#> [1] "double"
#> 
#> 👆 e2
#> [1] 4
```

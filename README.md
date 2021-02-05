
<!-- README.md is generated from README.Rmd. Please edit that file -->

# boomer <img src='man/figures/logo.PNG' align="right" height="139" />

*{boomer}* lets you look at intermediate results of a call. It
“explodes” the call into its parts hence the name. It is useful for
debugging and teaching operation precedence.

## Installation

Install with:

``` r
remotes::install_github("moodymudskipper/boomer")
```

## Examples

``` r
library(boomer)
boom(1 + !1 * 2)
#> 1 * 2
#> [1] 2
#> !1 * 2
#> [1] FALSE
#> 1 + !1 * 2
#> [1] 1

boom(subset(head(mtcars, 2), qsec > 17))
#> head(mtcars, 2)
#>               mpg cyl disp  hp drat    wt  qsec vs am gear carb
#> Mazda RX4      21   6  160 110  3.9 2.620 16.46  0  1    4    4
#> Mazda RX4 Wag  21   6  160 110  3.9 2.875 17.02  0  1    4    4
#> qsec > 17
#> [1] FALSE  TRUE
#> subset(head(mtcars, 2), qsec > 17)
#>               mpg cyl disp  hp drat    wt  qsec vs am gear carb
#> Mazda RX4 Wag  21   6  160 110  3.9 2.875 17.02  0  1    4    4
```

You can use `boom()` with *{magrittr}* pipes, just pipe to `boom()` at
the end of a pipe chain.

``` r
library(magrittr)
mtcars %>%
  head(2) %>%
  subset(qsec > 17) %>%
  boom()
#> head(., 2)
#>               mpg cyl disp  hp drat    wt  qsec vs am gear carb
#> Mazda RX4      21   6  160 110  3.9 2.620 16.46  0  1    4    4
#> Mazda RX4 Wag  21   6  160 110  3.9 2.875 17.02  0  1    4    4
#> qsec > 17
#> [1] FALSE  TRUE
#> subset(., qsec > 17)
#>               mpg cyl disp  hp drat    wt  qsec vs am gear carb
#> Mazda RX4 Wag  21   6  160 110  3.9 2.875 17.02  0  1    4    4
#> mtcars %>% head(2) %>% subset(qsec > 17)
#>               mpg cyl disp  hp drat    wt  qsec vs am gear carb
#> Mazda RX4 Wag  21   6  160 110  3.9 2.875 17.02  0  1    4    4
```

If a call fails, *{boomer}* will print intermediate outputs up to the
occurrence of the error, it can help with debugging:

``` r
"tomato" %>%
  substr(1, 3) %>%
  toupper() %>%
  sqrt() %>%
  boom()
#> substr(., 1, 3)
#> [1] "tom"
#> toupper(.)
#> [1] "TOM"
#> Error in .Primitive("sqrt")(.): non-numeric argument to mathematical function
```

`boom()` features a couple of optional arguments:

  - Set `clock` to `TRUE` to see how long each step (in isolation\!)
    took to run.

  - Set the `print` argument to a function such as `str` to change what
    is printed (see `?boom` to see how to print differently depending on
    class). Useful alternatives would be `dplyr::glimpse` of `invisible`
    (to print nothing). This is useful when the output is too long.

<!-- end list -->

``` r
boom(head(sapply(seq(10^6), sqrt)), clock = TRUE, print = str)
#> 10^6
#> time: 0 us
#>  num 1e+06
#> seq(10^6)
#> time: 0 us
#>  int [1:1000000] 1 2 3 4 5 6 7 8 9 10 ...
#> sapply(seq(10^6), sqrt)
#> time: 1.454 s
#>  num [1:1000000] 1 1.41 1.73 2 2.24 ...
#> head(sapply(seq(10^6), sqrt))
#> time: 0 us
#>  num [1:6] 1 1.41 1.73 2 2.24 ...
```

`boom()` also works works on loops and multi-line expression, you can
also `rig()` a function in order to `boom()` all the calls of its body :

``` r
# try it out on you own, a bit too verbose for a README :)
rig(ave)(warpbreaks$breaks, warpbreaks$wool)
```

## Addin

To avoid typing `boom()` all the time you can use the provided addin
named *“Explode a call with `boom()`”*: just attribute a key combination
to it (I use ctrl+shift+alt+B on windows), select the call you’d like to
explode and fire away\!

The default values of the `clock` and `print` arguments of `boom()` are
given by the options `"boomer.clock"` and `"boomer.print"`, so by
modifying those you will change the behavior of the addin.

## Notes

*{boomer}* prints the output of intermediate steps as they are executed,
and thus doesn’t say anything about what isn’t executed, it is in
contrast with functions like `lobstr::ast()` which return the parse
tree.

This will be noticeable with some uses of non standard evaluation.

``` r
lobstr::ast(deparse(quote(1+2+3+4)))
#> o-deparse 
#> \-o-quote 
#>   \-o-`+` 
#>     +-o-`+` 
#>     | +-o-`+` 
#>     | | +-1 
#>     | | \-2 
#>     | \-3 
#>     \-4

boom(deparse(quote(1+2+3+4)))
#> quote(1 + 2 + 3 + 4)
#> 1 + 2 + 3 + 4
#> deparse(quote(1 + 2 + 3 + 4))
#> [1] "1 + 2 + 3 + 4"

# standard evaluation
boom(1+2+3+4)
#> 1 + 2
#> [1] 3
#> 1 + 2 + 3
#> [1] 6
#> 1 + 2 + 3 + 4
#> [1] 10
```

An earlier version of the package was released as *{boom}*, but a
package *{Boom}* already exists on CRAN so it was renamed. *{boom}*
won’t be maintained and will be archived, possibly removed.

Thanks to @data\_question for suggesting the name *{boomer}*.

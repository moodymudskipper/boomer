# Switch "boom" debugging on and off

While debugging a function, call `boom_on()` and all subsequent calls
will be boomed, call `boom_off()` to return to standard debugging.

## Usage

``` r
boom_on(clock = NULL, print = NULL)

boom_off()
```

## Arguments

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

## Value

Returns `NULL` invisibly, called for side effects.

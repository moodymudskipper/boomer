# How boomer works

This vignette summarizes how the internals of boomer work.

## Overview

An important principle of {boomer} is that we don’t modify the body of
the function we rig.

``` r
rigged_file_ext <- boomer::rig(tools::file_ext)
tools::file_ext
#> function (x) 
#> {
#>     pos <- regexpr("\\.([[:alnum:]]+)$", x)
#>     ifelse(pos > -1L, substring(x, pos + 1L), "")
#> }
#> <bytecode: 0x56446a6d3bb0>
#> <environment: namespace:tools>
rigged_file_ext
#> function (x) 
#> {
#>     pos <- regexpr("\\.([[:alnum:]]+)$", x)
#>     ifelse(pos > -1L, substring(x, pos + 1L), "")
#> }
#> <environment: 0x56446a811f90>
```

Instead we copy the original function but give it a new environment.
This new environment is a child of the original environment, and is
populated with shims of the functions called by the original function.
We call this environment the mask.

``` r
# the original environment
environment(tools::file_ext)
#> <environment: namespace:tools>

# our new environment
env <- environment(rigged_file_ext)
env
#> <environment: 0x56446a811f90>

# its parent
parent.env(env)
#> <environment: namespace:tools>

# its content
ls(env)
#>  [1] "-"         "::"        ":::"       "("         "{"         "+"        
#>  [7] "<-"        "="         ">"         "ifelse"    "regexpr"   "substring"
```

`rig_impl()` does this job and is the main function of the package. It
calls `wrap()`, the other important function, whose mission is to build
verbose shims of functions used in the rigged function. Both are
detailed thereafter.

## Individual functions

### `boom()` and `rig()`

[`boom()`](https://moodymudskipper.github.io/boomer/reference/boom.md)
is a wrapper around `rig_impl()` , it rigs the calling function and runs
the call, it also has some hacky code so we can pipe to
[`boom()`](https://moodymudskipper.github.io/boomer/reference/boom.md)
with {magrittr} (The hack is not needed for the base pipe)

### `rig_impl()`

Here’s the diagram of dependencies of `rig_impl()`

``` r
flow::flow_view_deps(boomer:::rig_impl, show_imports = "packages")
#> PhantomJS not found. You can install it with webshot::install_phantomjs(). If it is installed, please make sure the phantomjs executable can be found via the PATH variable.
#> Error in knitr::include_graphics(png): Cannot find the file(s): "/tmp/RtmpfIlpI5/flow_38616120282f.png"
```

`rig_impl()` :

- Creates a new environment (the mask) as a child of the original
  environment
- Populates it with shims of `::` and `:::` so when we find `pkg::fun`
  or `pkg:::fun` in the original function we don’t wrap their output and
  not the `::` or `:::` operator
- Populates it with shims of `<-` and `=` so functions created in the
  original function, and thus impossible to shim at “rig time”, can be
  made verbose too.
- Populates it with shims of all other called functions, using `wrap()`
- Creates special variables `..FIRST_CALL..` and `..EVALED_ARGS..` in
  this environment, `..FIRST_CALL..` is a boolean initiated to `TRUE`
  and set to `FALSE` once the first wrapper call is triggered,
  `..EVALED_ARGS..` is a named logical vector that keeps track of which
  arguments have been evaled.

### `wrap()`

`wrap()` builds verbose wrapper functions.

Its main aim is to print information directly related to the wrapped
function (e.g. argument values and execution time).

However it does a couple more things:

- If a wrapper function is the first called it prints information
  signaling that we are entering the rig function, and it sets up our
  rigged function so on exit it will print information that we are
  exiting it. This uses the `..FIRST_CALL..` special variable introduced
  in above section.
- It checks if the rigged function’s arguments have been evaluated
  (remember in R arguments are not evaluated unless their values are
  requested down the line) and prints them when they are. This uses the
  \`..EVALED_ARGS..\`\` special variable introduced in above section.

### `rig_in_namespace()`

[`rig_in_namespace()`](https://moodymudskipper.github.io/boomer/reference/boom.md)
calls `rig_impl()` on its inputs but unlike
[`rig()`](https://moodymudskipper.github.io/boomer/reference/boom.md) we
want the rigged functions to be bound in the namespace instead of the
original functions.

To do this we unlock the namespace and assigned rigged functions there.

Since
[`rig_in_namespace()`](https://moodymudskipper.github.io/boomer/reference/boom.md)
accepts several functions as arguments, and that they might call each
other, we also make sure we include wrapped versions of our rigged
functions in all the masks.

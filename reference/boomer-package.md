# boomer: Debugging Tools to Inspect the Intermediate Steps of a Call

Provides debugging tools that let you inspect the intermediate results
of a call. The output looks as if we explode a call into its parts hence
the package name.

## Details

- [`boom()`](https://moodymudskipper.github.io/boomer/reference/boom.md)
  displays the intermediate results of a call or a code chunk.

- [`rig()`](https://moodymudskipper.github.io/boomer/reference/boom.md)
  creates a copy of a function which will display the intermediate
  results of all the calls of it body.

- [`rig_in_namespace()`](https://moodymudskipper.github.io/boomer/reference/boom.md)
  rigs a namespaced function in place, so its always verbose even when
  called by other existing functions. It is especially handy for package
  development.

- [`rigger()`](https://moodymudskipper.github.io/boomer/reference/boom.md)
  provides a convenient way to rig an anonymous function by using the
  `rigger(...) + function(...) {...}` syntax.

- The addin "Explode a call with 'boom()'" provides a way to
  [`boom()`](https://moodymudskipper.github.io/boomer/reference/boom.md)
  a call with a keyboard shortcut.

## Package options

Several options impact the display of exploded calls :

- `boomer.print`: If the `print` argument is not provided, this option
  will replace it at run time. Defaults to the
  [`base::print`](https://rdrr.io/r/base/print.html) function.

- `boomer.clock`: If the `clock` argument is not provided, this option
  will replace it at run time. Defaults to `FALSE`.

- `boomer.print_args`: Whether to print the arguments of rigged
  functions and their values when they are evaluated. Defaults to
  `TRUE`.

- `boomer.visible_only`: Whether to hide the output of functions which
  return invisibly. Defaults to `FALSE`.

- `boomer.ignore`: Vector of function names or named list of functions
  for which we don't want the result printed (usually because it's
  redundant). Defaults to `c("~", "{", "(", "<-", "<<-", "=")`.

- `boomer.ignore_args`: Vector of function names or named list of
  functions for which we don't want the arguments boomed, this might be
  useful when calling a function that loops too many times.

- `boomer.safe_print`: Whether to replace emoticons by characters
  compatible with all systems. This is useful for reprexes (see reprex
  package) and for knitted report in case the output of those doesn't
  look good on your system.

- `boomer.abbreviate`: Whether to show only the function's name rather
  than the call when it's entered.

## See also

Useful links:

- <https://moodymudskipper.github.io/boomer/>

- <https://github.com/moodymudskipper/boomer>

- Report bugs at <https://github.com/moodymudskipper/boomer/issues>

## Author

**Maintainer**: Antoine Fabri <antoine.fabri@gmail.com>

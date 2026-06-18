# Changelog

## boomer 1.0.0

Boomer is back on CRAN!

- [`rig_in_namespace()`](https://moodymudskipper.github.io/boomer/reference/rig_in_namespace.md)
  was renamed to
  [`rig_in_place()`](https://moodymudskipper.github.io/boomer/reference/boom.md),
  and is kept as a deprecated alias.
- [`rig_in_place()`](https://moodymudskipper.github.io/boomer/reference/boom.md)
  works on functions from any package and on several functions at once.
  It rigs them wherever a copy is used (the namespace, the attached
  package environment, the S3 methods table, and the import environments
  of other loaded packages) so they stay verbose even when called by
  other functions.
- Rigging an S3 method now updates the copy that S3 dispatch actually
  calls, and rigging an S3 generic rigs all of its registered methods.
- [`rig()`](https://moodymudskipper.github.io/boomer/reference/boom.md)
  and
  [`rig_in_place()`](https://moodymudskipper.github.io/boomer/reference/boom.md)
  accept character input, e.g. `rig("pkg::fun")`.
- [`rig()`](https://moodymudskipper.github.io/boomer/reference/boom.md)
  and
  [`rig_in_place()`](https://moodymudskipper.github.io/boomer/reference/boom.md)
  support recursive functions.
- [`rig_on_load()`](https://moodymudskipper.github.io/boomer/reference/boom.md),
  called from a package’s `.onLoad()`, rigs the functions listed in the
  `boomer.rig_on_load` option.
- New options `boomer.theme.args`, `boomer.theme.rigged_fun` and
  `boomer.theme.code` customize the colors used in the output.
- A new option `boomer.max_indent` helps keeping the indentation under
  control, useful for long pipe sequences or deeply nested calls.
- A new option `boomer.log` can send the exploded output to files in
  addition to (or instead of) the console.
- boomer doesn’t use the ‘pryr’ package anymore.
- Rigging a function that evaluates a wrapped call directly in its own
  environment no longer fails with “the empty environment has no
  parent”. This affected e.g. `constructive::construct()`, which uses
  [`rlang::arg_match()`](https://rlang.r-lib.org/reference/arg_match.html)
  to resolve a formal’s choices.

## boomer 0.2.0

CRAN release: 2024-07-09

Just a small release with a minor feature. The previous version got
kicked from CRAN because the Suggests dependency {flow} had been kicked
from CRAN at the time.

- New `boomer.ignore_args` experimental option used not to boom a
  function’s argument

## boomer 0.1.1

CRAN release: 2022-09-16

- Call
  [`boom_on()`](https://moodymudskipper.github.io/boomer/reference/boom_on.md)
  and
  [`boom_off()`](https://moodymudskipper.github.io/boomer/reference/boom_on.md)
  when browsing to toggle “boom debugging” on and off
- Call
  [`boom_shinyApp()`](https://moodymudskipper.github.io/boomer/reference/boom_shinyApp.md)
  and
  [`boom_runApp()`](https://moodymudskipper.github.io/boomer/reference/boom_shinyApp.md)
  to start a shiny app as you would with {shiny}’s `shinyApp()` and
  `runApp()`, but making chosen reactives verbose (very experimental).
- A new vignette for the above
- Another new vignette summarizing how {boomer} works
- {boomer} doesn’t choke anymore in case of missing arguments
- Rigged function names are displayed better when rig is called on a
  namespaced call

## boomer 0.1.0

CRAN release: 2021-07-20

- Call
  [`boom()`](https://moodymudskipper.github.io/boomer/reference/boom.md)
  to explode a call and print outputs of intermediate steps
- Call
  [`rig()`](https://moodymudskipper.github.io/boomer/reference/boom.md)
  to set up a function so that all its call will be exploded
- `clock` argument to clock steps
- `print` argument for custom printing of intermediate outputs
- Addin “Explode a call with
  [`boom()`](https://moodymudskipper.github.io/boomer/reference/boom.md)”
  to
  [`boom()`](https://moodymudskipper.github.io/boomer/reference/boom.md)
  selection
- Call
  [`rigger()`](https://moodymudskipper.github.io/boomer/reference/boom.md)
  to define an anonymous rigged function
- Call
  [`rig_in_namespace()`](https://moodymudskipper.github.io/boomer/reference/rig_in_namespace.md)
  to rig a function in place in its package
- Several options are implemented to customize the output
- Output is made more readable, using indents and emoticons
- The names of the rigged functions are displayed when entered
- The values of the arguments of rigged functions are displayed when
  they are evaluated
- We use {styler} to display readable calls
- Robustness was significantly improved

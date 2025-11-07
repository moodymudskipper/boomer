# Changelog

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
  [`rig_in_namespace()`](https://moodymudskipper.github.io/boomer/reference/boom.md)
  to rig a function in place in its package
- Several options are implemented to customize the output
- Output is made more readable, using indents and emoticons
- The names of the rigged functions are displayed when entered
- The values of the arguments of rigged functions are displayed when
  they are evaluated
- We use {styler} to display readable calls
- Robustness was significantly improved

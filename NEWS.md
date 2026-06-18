# boomer 1.0.0

Boomer is back on CRAN!

- `rig_in_namespace()` was renamed to `rig_in_place()`, and is kept as a
  deprecated alias.
- `rig_in_place()` works on functions from any package and on several functions
  at once. It rigs them wherever a copy is used (the namespace, the attached
  package environment, the S3 methods table, and the import environments of other
  loaded packages) so they stay verbose even when called by other functions.
- Rigging an S3 method now updates the copy that S3 dispatch actually calls, and
  rigging an S3 generic rigs all of its registered methods.
- `rig()` and `rig_in_place()` accept character input, e.g. `rig("pkg::fun")`.
- `rig()` and `rig_in_place()` support recursive functions.
- `rig_on_load()`, called from a package's `.onLoad()`, rigs the functions listed
  in the `boomer.rig_on_load` option.
- New options `boomer.theme.args`, `boomer.theme.rigged_fun` and
  `boomer.theme.code` customize the colors used in the output.
- A new option `boomer.max_indent` helps keeping the indentation under control, 
  useful for long pipe sequences or deeply nested calls.
- A new option `boomer.log` can send the exploded output to files in addition to
  (or instead of) the console.
- boomer doesn't use the 'pryr' package anymore.
- Rigging a function that evaluates a wrapped call directly in its own
  environment no longer fails with "the empty environment has no parent". This
  affected e.g. `constructive::construct()`, which uses `rlang::arg_match()` to
  resolve a formal's choices.

# boomer 0.2.0

Just a small release with a minor feature. The previous version got kicked from CRAN
because the Suggests dependency {flow} had been kicked from CRAN at the time. 

- New `boomer.ignore_args` experimental option used not to boom a function's argument

# boomer 0.1.1

* Call `boom_on()` and `boom_off()` when browsing to toggle "boom debugging" on and off
* Call `boom_shinyApp()` and `boom_runApp()` to start a shiny app as you would with {shiny}'s
`shinyApp()` and `runApp()`, but making chosen reactives verbose (very experimental).
* A new vignette for the above
* Another new vignette summarizing how {boomer} works
* {boomer} doesn't choke anymore in case of missing arguments
* Rigged function names are displayed better when rig is called on a namespaced call

# boomer 0.1.0

* Call `boom()` to explode a call and print outputs of intermediate steps
* Call `rig()` to set up a function so that all its call will be exploded
* `clock` argument to clock steps
* `print` argument for custom printing of intermediate outputs
* Addin "Explode a call with `boom()`" to `boom()` selection
* Call `rigger()` to define an anonymous rigged function
* Call `rig_in_namespace()` to rig a function in place in its package
* Several options are implemented to customize the output
* Output is made more readable, using indents and emoticons
* The names of the rigged functions are displayed when entered
* The values of the arguments of rigged functions are displayed when they are
evaluated
* We use {styler} to display readable calls
* Robustness was significantly improved 

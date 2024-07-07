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

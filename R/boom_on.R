#' Switch "boom" debugging on and off
#'
#' To scope the verbosity brought by 'boomer' to specific sections of the code we can call
#' `boom_on()` and `boom_off()`. These can be called interactively when using 
#' `browser()`, `debug()`, `debugonce()` for instance, or non-interactively, with the caveat explained
#' in the dedicated section below (in short, you need to call `boom_on(browser())` )
#' 
#' @section Calling `boom_on()` and `boom_off()` interactively:
#' 
#' While debugging a function, call `boom_on()` and all subsequent calls will be boomed,
#' call `boom_off()` to return to standard debugging.
#' 
#' @section Calling `boom_on()` and `boom_off()` non-interactively:
#' 
#' This use case has a quirky singularity. 
#' The first time any R function is called a flag is set on the object, invisible from R itself. 
#' Then when it's called a second time the function is compiled to bytecode before execution.
#' A consequence is that the tricks boomer uses to make functions chatty don't work on operators 
#' like `+` or control flow constructs like `if`.
#' 
#' There are 3 ways to make sure a function is not compiled:
#' 
#' * Run it in interactive mode (see section above)
#' * Fake the interactive mode by having an unevaled call to `browser()` in the function's body, 
#'   like a line of code `~ browser()`, or easier just `boom_on(browser())` where the function ignores the first arg.
#' * Disable `compiler::enableJIT(0)` before the function is compiled or set the env variable `R_ENABLE_JIT=0` in your .REnviron (setting it mid session won't work).
#' 
#' Pro tip: Have in your .RProfile `setHook(packageEvent("boomer", "onLoad"), function(...) compiler::enableJIT(0))`, 
#' it will disable compilation only when boomer's namespace is loaded, at a performance cost that is most likely not
#' impactful in a debugging context. This way you can call `boom_on()` normally without bothering with these odd
#' looking `browser()` calls.
#' 
#' @inheritParams boom
#' @param ... Not used by the code, but passing `browser()` makes sure the caller function is not compiled
#' into bytecode.
#' 
#' @export
#' @return Returns `NULL` invisibly, called for side effects.
boom_on <- function(..., clock = NULL, print = NULL) {
  fun <- sys.function(-1)
  rigged_fun <- rig_impl(fun, clock, print, rigged_nm = NULL)
  e <- parent.frame()
  parent.env(e) <- environment(rigged_fun)
  invisible(NULL)
}

#' @export
#' @rdname boom_on
boom_off <- function() {
  e <- parent.frame()
  parent.env(e) <- parent.env(parent.env(e))
  invisible(NULL)
}

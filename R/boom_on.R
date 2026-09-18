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
#' * Call `compiler::enableJIT(0)` before the function is compiled or set the env variable `R_ENABLE_JIT=0` in your .REnviron (setting it mid session won't work).
#' 
#' Pro tip: Have in your .RProfile `setHook(packageEvent("boomer", "onLoad"), function(...) compiler::enableJIT(0))`, 
#' it will disable compilation only when boomer's namespace is loaded, at a performance cost that is most likely not
#' impactful in a debugging context. This way you can call `boom_on()` normally without bothering with these odd
#' looking `browser()` calls.
#' 
#' `boom_on()` warns when it detects that the calling function was compiled, unless it was
#' given an argument through `...` or called from a `browser()` prompt.
#' 
#' @inheritParams boom
#' @param ... Not used by the code, but passing `browser()` makes sure the caller function is not compiled
#' into bytecode.
#' 
#' @export
#' @return Returns `NULL` invisibly, called for side effects.
boom_on <- function(..., clock = NULL, print = NULL) {
  fun <- sys.function(-1)
  warn_if_compiled(fun, has_dots = ...length() > 0)
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

# A byte-compiled caller won't have its operators and control flow constructs
# boomed, so we tell the user how to avoid the compilation. Calls typed at a
# browser prompt are fine, and so are calls that were given an unevaled
# `browser()` through `...`, which is what prevented the compilation.
warn_if_compiled <- function(fun, has_dots) {
  if (has_dots || !is_compiled(fun) || is_browsing()) return(invisible(NULL))
  warning(
    "`boom_on()` was called from a byte-compiled function, so operators like ",
    "`+` and control flow constructs like `if` won't be boomed.\n",
    "Call `boom_on(browser())` instead, or disable the compilation for the ",
    "session with `compiler::enableJIT(0)`.\n",
    "See `?boom_on` for details.",
    call. = FALSE
  )
}

is_compiled <- function(fun) {
  # R gives us no direct way to ask whether a closure carries bytecode, but
  # printing it shows a `<bytecode: ...>` line when it does
  any(startsWith(capture.output(print(fun)), "<bytecode:"))
}

is_browsing <- function() {
  # `browserText()` fails when no browser context is on the stack
  tryCatch({browserText(); TRUE}, error = function(e) FALSE)
}

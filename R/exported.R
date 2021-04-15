
#' Print the Output of Intermediate Steps of a Call
#'
#' `boom()` will explode a call into the output of its parts.
#' `rig()` a function to make all the calls of its body `boom()`.
#'
#' @param expr call to explode
#' @param fun function ro `rig()`
#' @param clock whether to time intermediate steps, `FALSE` by default unless you
#' set `options(boom.clock = TRUE)`. The execution time of a step doesn't include the
#' execution time of its previously printed sub-steps.
#' @param print A function, a formula or a list of functions or formulas.
#'
#' @details
#' By default, unless `options(boom.clock = print)` is set to a custom value, the
#' output of every step is printed in a standard way.
#'
#' If you provide another function such as
#' `str`, the console output of `str` will be printed. Use `invisible` to display
#' nothing, another useful alternative would be `dplyr::glimpse`.
#'
#' *{rlang}*'s formula notation is supported, so for instance you can type:
#' `print = ~ dplyr::glimpse(., width = 50)`.
#'
#' Sometimes you might want to print a specific type of object in a custom way,
#' in this case you can provide a named list, if you provide an unnamed element
#' it will be used as the default, and named elements will define how objects
#' of the given S3 class are printed. For instance `print = list(str, data.frame = tibble::as_tibble)`
#'
#' Modifying the options is especially useful if you want to use the addin
#' with custom behavior.
#'
#' @export
#'
#' @examples
#' # explode a simple call
#' boom(subset(head(mtcars, 2), qsec > 17))
#'
#' # clock calls and customize how to print output
#' boom(subset(head(mtcars, 2), qsec > 17), clock = TRUE, print = str)
#'
#' # print str only for data frames
#' boom(subset(head(mtcars, 2), qsec > 17), print = list(data.frame = str))
#'
#' # rig a function
#' rig(ave)(warpbreaks$breaks, warpbreaks$wool)
boom <- function(expr, clock = getOption("boom.clock"), print = getOption("boom.print")) {

  # if we are in a pipe chain, explode the chain above
  scs <- sys.calls()
  l <- length(scs)
  call_is_piped <-
    identical(scs[[l]][[2]], quote(.)) &&
    identical(scs[[l-1]][[1]], quote(`%>%`))
  if(call_is_piped) {
    call <- do.call(substitute, list(scs[[l]], list(. = scs[[l-1]][[2]])))
    eval.parent(call)
  }
  # reset the global times table
  reset_globals()

  pf   <- parent.frame()
  expr <- substitute(expr)
  funs <- setdiff(all.names(expr), c(all.vars(expr), "::", ":::"))
  mask <- list()
  # go through every existing function detected above and create a wrapper
  # in the mask to override it
  for (fun_chr in funs) {
    # `funs` will include namespaces and functions yet to be defined (in the case of a script)
    # so we don't want to fail here if the object doesn't exist
    if(!exists(fun_chr, pf)) next

    # fetch the env, primitives don't have one, but they're in the base package
    fun_val <- get(fun_chr, envir = pf)
    fun_env <- environment(fun_val)
    if(is.null(fun_env)) {
      fun_env <- asNamespace("base")
    }

    f <- wrap(fun_val, clock, print)
    environment(f) <- fun_env

    mask[[fun_chr]] <- f

  }
  mask$`::` <- double_colon(clock, print)
  mask$`:::` <- triple_colon(clock, print)
  invisible(eval(expr, envir = mask, enclos = parent.frame()))
}

#' @export
#' @rdname boom
rig <- function(fun, clock = getOption("boom.clock"), print = getOption("boom.print")) {
  expr <- body(fun)
  reset_globals()
  pf   <- parent.frame()
  funs <- setdiff(all.names(expr), c(
    all.vars(expr), "::", ":::"))
  mask <- new.env(parent = environment(fun))
  # go through every existing function detected above and create a wrapper
  # in the mask to override it
  for (fun_chr in funs) {
    # fun will include namespaces nad functions yet to be defined (in the case of a script)
    # so we don't want to fail here if the object doesn't exist
    if(!exists(fun_chr, pf)) next

    # fetch the env, primitives don't have one, but they're in the base package
    fun_val <- get(fun_chr, envir = pf)
    fun_env <- environment(fun_val)
    if(is.null(fun_env)) {
      fun_env <- asNamespace("base")
    }

    f <- wrap(fun_val, clock, print)
    environment(f) <- fun_env
    mask[[fun_chr]] <- f
  }
  mask$`::` <- double_colon(clock, print)
  mask$`:::` <- triple_colon(clock, print)
  environment(fun) <- mask
  fun
}

#' Create rigged function conveniently
#'
#' Allows `rigger + function(...) {...}` syntax to create a rigged function
#' conveniently
#' @export
rigger <- logical()
class(rigger) <- "rigger"


#' @export
print.rigger <- function(x, ...) {
  writeLines(paste0(
    "rigger object, use the syntax `rigger + function(...) {...}` to create a ",
    "rigged function conveniently"))
  invisible(x)
}

#' @export
`+.rigger` <- function(e1, e2) rig(e2)


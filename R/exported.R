
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
#' @param ignore functions to ignore, defaults to `c("~", "\{", "(", "<-", "<<-", "=")`
#'   unless the option `"boom.ignore"` is modified. `::` and `:::` are always ignored.
#' @param visible_only whether functions returning invisibly should be considered,
#'   by default they are unless the option `"boom.visible_only"` is set to `TRUE`.
#'
#' @details
#' By default, unless the "boom.print" option  is set to a custom value, the
#' output of every step is printed in a standard way.
#'
#' If you provide another function such, for instance `options(boom.print = str)`
#' the console output of `str` will be printed. Use `invisible` to display
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
boom <- function(
  expr,
  clock = getOption("boom.clock"),
  print = getOption("boom.print"),
  ignore = getOption("boom.ignore"),
  visible_only = getOption("boom.visible_only")) {

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
  funs <- setdiff(all.names(expr), c(all.vars(expr), "::", ":::", ignore))
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

    f <- wrap(fun_val, clock, print, visible_only)
    environment(f) <- fun_env

    mask[[fun_chr]] <- f

  }
  mask$`::` <- double_colon(clock, print, visible_only)
  mask$`:::` <- triple_colon(clock, print, visible_only)
  invisible(eval(expr, envir = mask, enclos = parent.frame()))
}

#' @export
#' @rdname boom
rig <- function(
  fun,
  clock = getOption("boom.clock"),
  print = getOption("boom.print"),
  ignore = getOption("boom.ignore"),
  visible_only = getOption("boom.visible_only")) {

  expr <- body(fun)
  reset_globals()
  rigged_fun_env   <- environment(fun)
  funs <- setdiff(all.names(expr), c(
    all.vars(expr), "::", ":::", ignore))
  mask <- new.env(parent = rigged_fun_env)
  # go through every existing function detected above and create a wrapper
  # in the mask to override it
  for (fun_chr in funs) {
    # fun will include namespaces nad functions yet to be defined (in the case of a script)
    # so we don't want to fail here if the object doesn't exist
    if(!exists(fun_chr, rigged_fun_env)) next

    # fetch the env, primitives don't have one, but they're in the base package
    fun_val <- get(fun_chr, envir = rigged_fun_env)
    fun_env <- environment(fun_val)
    if(is.null(fun_env)) {
      fun_env <- asNamespace("base")
    }

    f <- wrap(fun_val, clock, print, visible_only)
    environment(f) <- fun_env
    mask[[fun_chr]] <- f
  }
  mask$`::` <- double_colon(clock, print, visible_only)
  mask$`:::` <- triple_colon(clock, print, visible_only)
  environment(fun) <- mask
  fun
}

#' Create rigged function conveniently
#'
#' Allows `rigger(...) + function(...) {...}` syntax to create a rigged function
#' conveniently.
#'
#' @inheritParams boom
#'
#' @return a list containing the arguments, with the class "rigger" to enable
#' `+.rigger` and `print.rigger`
#' @export
#' @examples
#' fun1 <- rigger() + function(x) x + 1 + 2
#' fun1(1)
#' fun2 <- rigger(TRUE, typeof) + function(x) x + 1 + 2
#' fun2(1)
rigger <- function(
  clock = getOption("boom.clock"),
  print = getOption("boom.print"),
  ignore = getOption("boom.ignore"),
  visible_only = getOption("boom.visible_only")) {
  res <- list(
    clock = clock, print = print, ignore = ignore, visible_only = visible_only)
  class(res) <- "rigger"
  res
}



#' @export
print.rigger <- function(x, ...) {
  writeLines(paste0(
    "# rigger object, use the syntax `rigger(...) + function(...) {...}` to create a ",
    "rigged function conveniently\n"))
  print(unclass(x))
  invisible(x)
}

#' @export
`+.rigger` <- function(e1, e2) {
  rig(e2,
      clock = e1$clock,
      print = e1$print,
      ignore = e1$ignore,
      visible_only = e1$visible_only)
}


#' rig functions directly in their namespace
#'
#' To recover original functions, restart the R session
#'
#'
#' @inheritParams boom
#' @param ... functions to rig
#'
#' @export
rig_in_namespace <- function(
  ...,
  clock = getOption("boom.clock"),
  print = getOption("boom.print"),
  ignore = getOption("boom.ignore"),
  visible_only = getOption("boom.visible_only")) {

  nms <- as.character(substitute(alist(...))[-1])
  vals <- list(...)

  ## rig all functions in their own namespace
  for (i in seq_along(vals)) {

    nm <- nms[[i]]
    ns <- environment(vals[[i]])
    vals[[i]] <- rig(vals[[i]])
    val <- vals[[i]]

    unlockBinding(nm, ns)
    assign(nm, val, ns)
    pkg <- paste0("package:", base::getNamespaceName(ns))
    unlockBinding(nm, as.environment(pkg))
    assign(nm, val, pkg)
  }

  # list of modified functions
  rigged_funs <- setNames(vals, nms)
  wrapped_funs <- lapply(rigged_funs, wrap, clock, print, visible_only)

  # add all modified functions to each function's environment
  for(fun in vals) {
    list2env(wrapped_funs, environment(fun))
  }

  invisible(NULL)
}


#' Print the Output of Intermediate Steps of a Call
#'
#' - `boom()` prints the intermediate results of a call or a code chunk.
#' - `rig()` creates a copy of a function which will display the intermediate
#' results of all the calls of it body.
#' - `rig_in_namespace()` rigs a namespaced function in place, so its always
#' verbose even when called by other existing functions. It is especially handy
#' for package development.
#' - `rigger()` provides a convenient way to rig an
#' anonymous function by using the `rigger(...) + function(...) {...}` syntax.
#'
#' @param expr call to explode
#' @param fun function ro `rig()`
#' @param clock whether to time intermediate steps. Defaults to `getOption("boomer.clock")`
#' evaluated at run time (`FALSE` unless you change it). The execution time of
#' a step doesn't include the execution time of its previously printed sub-steps.
#' @param print A function, a formula or a list of functions or formulas, used to
#' modify the way the output is printed. Defaults to `getOption("boomer.print")`
#' evaluated at run time (`base::print` unless you change it)'.
#' @param ... Functions to rig in their namespace
#'
#' If the `print` argument is a function, it will be used to print, or to transform the output
#' before it's printed. Use `invisible` to display nothing, useful possibilities are
#' `str` or `dplyr::glimpse`.
#'
#' *{rlang}*'s formula notation is supported, so for instance you can type:
#' `print = ~ dplyr::glimpse(., width = 50)`.
#'
#' Sometimes you might want to print a specific type of object in a custom way,
#' in this case you can provide a named list, if you provide an unnamed element
#' it will be used as the default, and named elements will define how objects
#' of the given S3 class are printed. For instance `print = list(str, data.frame = tibble::as_tibble)`
#'
#' @export
#' @return `boom()` returns the output of the call. `rig()` returns the modified
#' input function. `rig_in_namespace()` returns `invisible(NULL)` and is called
#' for side effects. `rigger()` returns a list containing the arguments, with
#' the class "rigger" to enable `+.rigger` and `print.rigger`
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
#' # rig an existing function
#' rig(ave)(warpbreaks$breaks, warpbreaks$wool)
#'
#' # rig an anonymous function
#' fun1 <- rigger() + function(x) x + 1 + 2 # same as rig(function(x) x + 1 + 2))
#' fun1(1)
#' fun2 <- rigger(TRUE, typeof) + function(x) x + 1 + 2
#' fun2(1)
boom <- function(
  expr,
  clock = NULL,
  print = NULL) {

  # if we are in a pipe chain, explode the chain above
  scs <- sys.calls()
  l <- length(scs)
  call_is_piped <-
    identical(scs[[l]][[2]], quote(.)) &&
    identical(scs[[l-1]][[1]], quote(`%>%`))
  if(call_is_piped) {
    # change `code %>% boom(., ...)` into `boom(code, ...)`
    call <- do.call(substitute, list(scs[[l]], list(. = scs[[l-1]][[2]])))
    # call modified expression
    eval.parent(call)
  }

  fun <- as.function(list(substitute(expr)), envir = parent.frame())
  fun <- rig_impl(fun, clock, print, rigged_nm = NULL)
  fun()
}

#' @export
#' @rdname boom
rig <- function(
    fun,
    clock = NULL,
    print = NULL
) {
  fun_lng <- substitute(fun)
  if (!is.function(fun)) stop("`fun` should evaluate to a function")
  if (is.symbol(fun_lng) || rlang::is_call(fun_lng, "::") || rlang::is_call(fun_lng, "::"))  {
    fun_chr <- paste(deparse(fun_lng), collapse="")
  } else {
    stop("`fun` should be provided in one of these forms : `f`, `pkg::f` or `pkg:::f`")
  }
  rig_impl(fun, clock, print, rigged_nm = fun_chr)
}

#' @export
#' @rdname boom
rigger <- function(
  clock = NULL,
  print = NULL) {
  res <- list(clock = clock, print = print)
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
      print = e1$print)
}


#' @export
#' @rdname boom
rig_in_namespace <- function(
  ...,
  clock = NULL,
  print = NULL) {

  nms <- as.character(substitute(alist(...))[-1])
  vals <- list(...)

  # rig all functions in their own namespace
  # i.e. keep their binding in the namespace but insert a parent on top
  # of their enclosing env and fill it with wrapped shims
  for (i in seq_along(vals)) {

    nm <- nms[[i]]
    ns <- environment(vals[[i]])
    vals[[i]] <- rig_impl(vals[[i]], clock = clock, print = print, rigged_nm = nms[[i]])
    val <- vals[[i]]
    ub <- unlockBinding
    ub(nm, ns)
    assign(nm, val, ns)
    pkg <- paste0("package:", base::getNamespaceName(ns))
    ub(nm, as.environment(pkg))
    assign(nm, val, pkg)
  }

  # list of modified functions
  rigged_funs <- setNames(vals, nms)
  wrapped_funs <- mapply(
    wrap,
    rigged_funs,
    MoreArgs = list(clock = clock, print_fun = print))

  # add all modified functions to each function's environment
  for(fun in vals) {
    list2env(wrapped_funs, environment(fun))
  }

  invisible(NULL)
}



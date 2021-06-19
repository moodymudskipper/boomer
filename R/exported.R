
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
#' @param print_args whether to print the values of the arguments provided to a rigged
#'   function. They are printed when they are evaluated, if they are evaluated.
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
#'

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
    # change `code %>% boom(., ...)` into `boom(code, ...)`
    call <- do.call(substitute, list(scs[[l]], list(. = scs[[l-1]][[2]])))
    # call modified expression
    eval.parent(call)
  }

  fun <- as.function(list(substitute(expr)), envir = parent.frame())
  fun <- rig_impl(fun, clock, print, ignore, visible_only, nm = NULL)
  fun()
}

#' @export
#' @rdname boom
rig <- function(
  fun,
  clock = getOption("boom.clock"),
  print = getOption("boom.print"),
  ignore = getOption("boom.ignore"),
  visible_only = getOption("boom.visible_only"),
  print_args = getOption("boom.print_args")) {
  rig_impl(fun, clock, print, ignore, visible_only,
           nm = as.character(substitute(fun)),
           print_args = print_args)
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
  visible_only = getOption("boom.visible_only"),
  print_args = getOption("boom.print_args")) {
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
  visible_only = getOption("boom.visible_only"),
  print_args = getOption("boom.print_args")) {

  nms <- as.character(substitute(alist(...))[-1])
  vals <- list(...)

  ## rig all functions in their own namespace
  for (i in seq_along(vals)) {

    nm <- nms[[i]]
    ns <- environment(vals[[i]])
    vals[[i]] <- rig_impl(vals[[i]], clock = clock, print = print, ignore = ignore,
                     visible_only = visible_only, nm = nms[[i]], print_args = print_args)
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
    nm = nms,
    MoreArgs = list(clock = clock, print_fun = print, visible_only = visible_only,
                    print_args = print_args))

  # add all modified functions to each function's environment
  for(fun in vals) {
    list2env(wrapped_funs, environment(fun))
  }

  invisible(NULL)
}




#' Print the Output of Intermediate Steps of a Call
#'
#' - `boom()` prints the intermediate results of a call or a code chunk.
#' - `rig()` creates a copy of a function which will display the intermediate
#' results of all the calls of it body.
#' - `rig_in_place()` rigs a function in place, so its always
#' verbose even when called by other existing functions. It works on functions from packages
#' as well as on functions defined in a session.
#' To undo, call `load_all()` for the development package or
#' `pkgload::unload()` on other packages, or restart the session if you rigged a base package.
#' It works on S3 methods, and when given an S3 generic it rigs the generic's
#' registered methods instead.
#' - `rig_on_load()` can be used in `.onLoad()` to rig functions (values or names) stored in `getOption("boomer.rig_on_load")` 
#' - `rigger()` provides a convenient way to rig an
#' anonymous function by using the `rigger(...) + function(...) {...}` syntax.
#' 
#' @section Side effects of `rig_in_place()`:
#' 
#' When called on a packaged function `rig_in_place()` replaces the target function in the namespace but
#' to behave as expected it does a bit more:
#' 
#' * Replace the copy of the function in the package environment, where the functions called without `::` or `:::` are fetched from after calling `library()`.
#' * Replace the copy of the function in the S3 methods table if relevant. S3 dispatch calls copies of functions, not the function in the namespace directly, so those need to be replaced as well.
#' * Replace the copy of the function in the "imports" environments of already loaded packages. Indeed packages that import function load copies of those in an environment, these need to be replaced as well.
#' 
#' @param expr call to explode
#' @param fun function ro `rig()`, can be a symbol, an expression returning a function, or a string
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
#' `constructive::construct`, `str` or `dplyr::glimpse`.
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
#' input function. `rig_in_place()` returns `invisible(NULL)` and is called
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
  if (is.character(fun)) {
    fun_lng <- str2lang(fun)
    fun <- eval.parent(fun_lng)
  }
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
rig_in_place <- function(
  ...,
  clock = NULL,
  print = NULL) {

  expr <- substitute(alist(...))[-1]
  vals <- rlang::list2(...)
  char_lgl <- sapply(vals, is.character)
  expr[char_lgl] <- lapply(vals[char_lgl], str2lang)
  pf <- parent.frame()
  vals[char_lgl] <- lapply(expr[char_lgl], eval, pf)
  expr <- lapply(
    expr,
    function(x) {
      if (length(x) > 1 && list(x[[1]]) %in% c(as.symbol("::"), as.symbol(":::"))) {
        x <- x[[3]]
      }
      x
    }
  )
  
  nms <- as.character(expr)

  # Rigging an S3 generic on its own has no visible effect: the generic just
  # dispatches (`UseMethod()`) to a method, and it is the methods that we want
  # to explode. So whenever a generic is passed we rig all of its registered S3
  # methods instead. (#141)
  is_generic <- vapply(
    vals, function(v) isTRUE(unname(isS3stdGeneric(v))), logical(1)
  )
  if (any(is_generic)) {
    expanded_vals <- list()
    expanded_nms <- character()
    for (i in seq_along(vals)) {
      if (!is_generic[[i]]) {
        expanded_vals <- c(expanded_vals, vals[i])
        expanded_nms <- c(expanded_nms, nms[[i]])
        next
      }
      methods <- s3_methods_of(nms[[i]], vals[[i]])
      if (!length(methods)) {
        message("`", nms[[i]], "()` is an S3 generic with no registered methods to rig.")
        next
      }
      message(
        "`", nms[[i]], "()` is an S3 generic, rigging its ", length(methods),
        " registered method(s) instead: ",
        paste0(names(methods), "()", collapse = ", ")
      )
      expanded_vals <- c(expanded_vals, unname(methods))
      expanded_nms <- c(expanded_nms, names(methods))
    }
    vals <- expanded_vals
    nms <- expanded_nms
  }

  ub <- unlockBinding

  # rig all functions in their own namespace
  # i.e. keep their binding in the namespace but insert a parent on top
  # of their enclosing env and fill it with wrapped shims
  for (i in seq_along(vals)) {

    nm <- nms[[i]]
    env <- environment(vals[[i]])
    ns <- topenv(env)
    orig <- vals[[i]]
    vals[[i]] <- rig_impl(vals[[i]], clock = clock, print = print, rigged_nm = nms[[i]])
    val <- vals[[i]]


    # compare against `orig` (the function before rigging), since `val` is the
    # rigged copy and would never match the binding in the namespace
    if (isNamespace(ns) && identical(get0(nm, ns, inherits = FALSE), orig)) {
      # if the library is attached and the function is exported
      # we need to update the copy in the package env
      pkg <- paste0("package:", base::getNamespaceName(ns))
      if (pkg %in% search() && nm %in% getNamespaceExports(ns)) {
        ub(nm, as.environment(pkg))
        assign(nm, val, pkg)
      }

      # if the function is an S3 method we need to update its registered copy,
      # which is the one S3 dispatch actually calls. That copy lives in the S3
      # methods table of the namespace that owns the *generic* (often base, e.g.
      # for `print`), which is not necessarily `ns`. We locate it by identity
      # wherever it is registered. The methods table environment is not locked,
      # so no unlocking is needed.
      for (loaded_ns in loadedNamespaces()) {
        s3_table <- asNamespace(loaded_ns)[[".__S3MethodsTable__."]]
        if (!is.null(s3_table) &&
            !is.null(s3_table[[nm]]) &&
            identical(s3_table[[nm]], orig)) {
          assign(nm, val, s3_table)
        }
      }

      # if the is imported by other packages priot to calling rig_in_place()
      # we need to rig the copies in the import environments
      # (Namespace loaded later will get the rigged functions right away)
      for (loaded_ns in setdiff(loadedNamespaces(), "base")) {
        imports_env <- parent.env(asNamespace(loaded_ns))
        imported_funs <- as.list(imports_env)
        ind <- match(list(val), imported_funs)
        if (!is.na(ind)) {
          # nm_imp is 99.9% of the time nm but let's be conservative
          nm_imp <- names(imported_funs)[[ind]]
          ub(nm_imp, imports_env)
          assign(nm_imp, val, imports_env)
        }
      }

      # The main function in the namespace
      ub(nm, ns)
    }
    assign(nm, val, ns)
  }

  # list of modified functions
  rigged_funs <- setNames(vals, nms)
  
  # add all modified functions to each function's environment
  for(nm in nms) {
    mask <- environment(rigged_funs[[nm]])
    wrapped_funs <- mapply(
      wrap,
      fun_val = rigged_funs,
      wrapped_nm = nms,
      MoreArgs = list(
        clock = clock, 
        print_fun = print, 
        rigged_nm = nm,
        mask = mask
      ) 
    )
    list2env(wrapped_funs, mask)
  }

  invisible(NULL)
}

#' rig_in_namespace
#' 
#' `rig_in_namespace()` is a deprecated alias to `rig_in_place()`
#' 
#' @export
#' @keywords internal
rig_in_namespace <- rig_in_place

#' @rdname boom
#' @export
rig_on_load <- function() {
  do.call(rig_in_place, as.list(getOption("boomer.rig_on_load")), envir = parent.frame())
}
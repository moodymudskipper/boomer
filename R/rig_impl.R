rig_impl <- function(
  fun,
  clock = getOption("boom.clock"),
  print = getOption("boom.print"),
  ignore = getOption("boom.ignore"),
  visible_only = getOption("boom.visible_only"),
  nm = NULL,
  print_args = FALSE) {

  expr <- body(fun)
  reset_globals()
  rigged_fun_env   <- environment(fun)
  funs <- fetch_functions(expr, ignore)
  mask <- new.env(parent = rigged_fun_env)
  mask$"<-" <- build_shimmed_assign("<-", ignore, clock, print, visible_only)
  mask$"=" <- build_shimmed_assign("=", ignore, clock, print, visible_only)
  # go through every existing function detected above and create a wrapper
  # in the mask to override it
  for (fun_chr in funs) {
    # `funs` will include functions yet to be defined when calling `rig()`
    # these might or might nor be sorted out by shim_assign, but we can't know
    # yet so we don't want to fail here if the object doesn't exist
    if(!exists(fun_chr, rigged_fun_env)) {
      if(!is.null(nm))
        message("`", fun_chr, "()` is undefined outside of `", nm, "()` and its output might not be shown.")
      next
    }

    # fetch the env, primitives don't have one, but they're in the base package
    fun_val <- get(fun_chr, envir = rigged_fun_env)
    fun_env <- environment(fun_val)
    if(is.null(fun_env)) {
      fun_env <- asNamespace("base")
    }

    f <- wrap(fun_val, clock, print, visible_only, nm = nm, print_args = print_args)
    mask[[fun_chr]] <- f
  }
  mask$`::` <- double_colon(clock, print, visible_only, nm, print_args)
  mask$`:::` <- triple_colon(clock, print, visible_only, nm, print_args)
  mask$..FIRST_CALL.. <- TRUE
  arg_nms <- formalArgs(fun)
  mask$..EVALED_ARGS.. <- setNames(rep(FALSE, length(arg_nms)), arg_nms)
  environment(fun) <- mask
  fun
}

build_shimmed_assign <- function(symbol, ignore, clock, print_fun, visible_only) {
  # return a function that shims an assignment operator
  f <- eval(bquote(function(e1, e2) {
    E2 <- if (is.language(e2)) {
      substitute(quote(e2), list(e2 = e2))
    } else if (!is.function(e2)) {
      e2
    } else {
      wrap(e2, clock = .(clock), print_fun = .(print_fun), visible_only = .(visible_only))
    }
    invisible(eval.parent(substitute(.Primitive(.(symbol))(e1, E2))))
  }))

  if (!symbol %in% ignore) {
    f <- wrap(f, clock = clock, print_fun = print_fun, visible_only = visible_only)
  }
  f
}

double_colon <- function(clock, print_fun, visible_only, nm, print_args) {
  if(clock) {
    function(pkg, name) {
      # code borrowed from base::`::`
      pkg <- as.character(substitute(pkg))
      name <- as.character(substitute(name))
      fun_val <- getExportedValue(pkg, name)
      if(!is.function(fun_val)) return(fun_val)

      wrap(fun_val, TRUE, print_fun, visible_only, nm, print_args)
    }
  } else {
    function(pkg, name) {
      # code borrowed from base::`::`
      pkg <- as.character(substitute(pkg))
      name <- as.character(substitute(name))
      fun_val <- getExportedValue(pkg, name)
      if(!is.function(fun_val)) return(fun_val)

      wrap(fun_val, FALSE, print_fun, visible_only, nm, print_args)
    }
  }
}

triple_colon <- function(clock, print_fun, visible_only, nm, print_args) {
  if(clock) {
    function(pkg, name) {
      # code borrowed from base::`:::`
      pkg <- as.character(substitute(pkg))
      name <- as.character(substitute(name))
      fun_val <- get(name, envir = asNamespace(pkg), inherits = FALSE)
      if(!is.function(fun_val)) return(fun_val)

      wrap(fun_val, TRUE, print_fun, visible_only, nm, print_args)
    }
  } else {
    function(pkg, name) {
      # code borrowed from base::`:::`
      pkg <- as.character(substitute(pkg))
      name <- as.character(substitute(name))
      fun_val <- get(name, envir = asNamespace(pkg), inherits = FALSE)
      if(!is.function(fun_val)) return(fun_val)

      wrap(fun_val, FALSE, print_fun, visible_only, nm, print_args)
    }
  }
}


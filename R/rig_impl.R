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

    f <- wrap(fun_val, clock, print, visible_only, nm, print_args, mask)
    mask[[fun_chr]] <- f
  }
  mask$`::` <- double_colon(clock, print, visible_only, nm, print_args, mask)
  mask$`:::` <- triple_colon(clock, print, visible_only, nm, print_args, mask)
  mask$..FIRST_CALL.. <- TRUE
  arg_nms <- formalArgs(fun)
  mask$..EVALED_ARGS.. <- setNames(rep(FALSE, length(arg_nms)), arg_nms)
  environment(fun) <- mask
  fun
}

fetch_functions <- function(expr, ignore) {
  dismissed_token_types <-
    c("EQ_ASSIGN",      # dealt with through shim_assign() so ignored here
      "LEFT_ASSIGN",    # dealt with through shim_assign() so ignored here
      "expr",           #
      "expr_or_assign_or_help",
      "forcond",        # ignore token after `for`
      "SYMBOL",         # regular symbols not used before `(`
      "SYMBOL_SUB",     # argument names
      "NUM_CONST",      # numbers
      "STR_CONST",      # strings
      "EQ_SUB",         # `=` in argument definition
      "SYMBOL_PACKAGE", # rhs of `::` or `:::`
      "NS_GET_INT",     # `:::`
      "NS_GET",         # `::`
      "ELSE",           # not a function
      "IN",             # not a function
      "WHILE",          # always returns NULL
      "REPEAT",         # always returns NULL
      "FOR")            # always returns NULL
  parse_data <- getParseData(parse(text = deparse(expr), keep.source = TRUE))
  # remove rows which follow `:::` or `::`
  to_remove <- which(parse_data$token %in% c("NS_GET", "NS_GET_INT")) + 1
  if(length(to_remove)) parse_data <- parse_data[- to_remove,]
  # keep only eligible token types
  funs <- parse_data$text[! parse_data$token %in% dismissed_token_types]
  # remove tokens that are not functions
  funs <- setdiff(unique(funs), c(ignore, ")", "}", ",", "]"))
  funs
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

double_colon <- function(clock, print_fun, visible_only, nm, print_args, mask) {
  if(clock) {
    function(pkg, name) {
      # code borrowed from base::`::`
      pkg <- as.character(substitute(pkg))
      name <- as.character(substitute(name))
      fun_val <- getExportedValue(pkg, name)
      if(!is.function(fun_val)) return(fun_val)

      wrap(fun_val, TRUE, print_fun, visible_only, nm, print_args, mask)
    }
  } else {
    function(pkg, name) {
      # code borrowed from base::`::`
      pkg <- as.character(substitute(pkg))
      name <- as.character(substitute(name))
      fun_val <- getExportedValue(pkg, name)
      if(!is.function(fun_val)) return(fun_val)

      wrap(fun_val, FALSE, print_fun, visible_only, nm, print_args, mask)
    }
  }
}

triple_colon <- function(clock, print_fun, visible_only, nm, print_args, mask) {
  if(clock) {
    function(pkg, name) {
      # code borrowed from base::`:::`
      pkg <- as.character(substitute(pkg))
      name <- as.character(substitute(name))
      fun_val <- get(name, envir = asNamespace(pkg), inherits = FALSE)
      if(!is.function(fun_val)) return(fun_val)

      wrap(fun_val, TRUE, print_fun, visible_only, nm, print_args, mask)
    }
  } else {
    function(pkg, name) {
      # code borrowed from base::`:::`
      pkg <- as.character(substitute(pkg))
      name <- as.character(substitute(name))
      fun_val <- get(name, envir = asNamespace(pkg), inherits = FALSE)
      if(!is.function(fun_val)) return(fun_val)

      wrap(fun_val, FALSE, print_fun, visible_only, nm, print_args, mask)
    }
  }
}


rig_impl <- function(
  fun,
  clock = NULL,
  print = NULL,
  rigged_nm = NULL) {

  expr <- body(fun)
  reset_globals()
  rigged_fun_env   <- environment(fun)
  wrapped_nms <- fetch_functions(expr)
  mask <- new.env(parent = rigged_fun_env)
  mask$"<-" <- build_shimmed_assign("<-", clock, print, rigged_nm, mask)
  mask$"=" <- build_shimmed_assign("=", clock, print, rigged_nm, mask)
  # go through every existing function detected above and create a wrapper
  # in the mask to override it
  for (wrapped_nm in wrapped_nms) {
    # `wrapped_nms` will include functions yet to be defined when calling `rig()`
    # these might or might nor be sorted out by shim_assign, but we can't know
    # yet so we don't want to fail here if the object doesn't exist
    if(!exists(wrapped_nm, rigged_fun_env)) {
      if(!is.null(rigged_nm))
        message("`", wrapped_nm, "()` is undefined outside of `", rigged_nm, "()` and its output might not be shown.")
      next
    }

    # fetch the env, primitives don't have one, but they're in the base package
    fun_val <- get(wrapped_nm, envir = rigged_fun_env)
    fun_env <- environment(fun_val)
    if(is.null(fun_env)) {
      fun_env <- asNamespace("base")
    }

    f <- wrap(fun_val, clock, print, rigged_nm, wrapped_nm, mask)
    mask[[wrapped_nm]] <- f
  }
  mask$`::` <- double_colon(clock, print, rigged_nm, mask)
  mask$`:::` <- triple_colon(clock, print, rigged_nm, mask)
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
  wrapped_nms <- parse_data$text[! parse_data$token %in% dismissed_token_types]
  # remove tokens that are not functions
  wrapped_nms <- setdiff(unique(wrapped_nms), c(")", "}", ",", "]")) # ignore,
  wrapped_nms
}


build_shimmed_assign <- function(symbol, clock, print_fun, rigged_nm, mask) {
  assign_op <- eval(bquote(function(e1, e2) {
    E2 <- if (is.language(e2)) {
      # protect language object before it's passed to substitute
      substitute(quote(e2), list(e2 = e2))
    } else if (!is.function(e2)) {
      # don't shim non functions
      e2
    } else {
      wrap(e2, .(clock), .(print_fun), .(rigged_nm), NA, .(mask))
    }
    invisible(eval.parent(substitute(.Primitive(.(symbol))(e1, E2))))
  }))

  # wrap the assignment op itself
  wrap(assign_op, clock = clock, print_fun = print_fun, rigged_nm, symbol, mask)
}

double_colon <- function(clock, print_fun, rigged_nm, mask) {
  function(pkg, name) {
    # code borrowed from base::`::`
    pkg <- as.character(substitute(pkg))
    name <- as.character(substitute(name))
    fun_val <- getExportedValue(pkg, name)
    if (!is.function(fun_val)) {
      return(fun_val)
    }
    wrap(fun_val, clock, print_fun, rigged_nm, "::", mask)
  }
}

triple_colon <- function(clock, print_fun, rigged_nm, mask) {
  function(pkg, name) {
    # code borrowed from base::`:::`
    pkg <- as.character(substitute(pkg))
    name <- as.character(substitute(name))
    fun_val <- get(name, envir = asNamespace(pkg), inherits = FALSE)
    if (!is.function(fun_val)) {
      return(fun_val)
    }
    wrap(fun_val, clock, print_fun, rigged_nm, "::", mask)
  }
}


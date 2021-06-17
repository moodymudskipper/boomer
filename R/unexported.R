# will contain `times` data frame and `last_total_time_end` POSIXct
globals <- new.env()
globals$n_indent <- -1

wrap <- function(fun_val, clock, print_fun, visible_only, nm = NULL, print_args = FALSE) {
  as.function(envir = asNamespace("boomer"), c(alist(...=), bquote2({
    # set icons, this is slightly inefficient but easier and more readable than
    .IF(getOption("boom.safe_print"), {
      rig_open   <- crayon::bold(crayon::yellow("{ "))
      rig_close  <- crayon::bold(crayon::yellow("} "))
      wrap_open  <- crayon::bold(crayon::yellow("< "))
      wrap_close <- crayon::bold(crayon::yellow("> "))
      dot        <- crayon::yellow(". ")
    }, {
      rig_open  <- "\U0001f447 "
      rig_close <- "\U0001f446 "
      wrap_open <- "\U0001f4a3 "
      wrap_close <- "\U0001f4a5 "
      dot        <- crayon::yellow("\ub7 ")
    }
    )

    # start the clock
    .IF(clock, total_time_start <- Sys.time())

    globals <- getFromNamespace("globals", "boomer")
    # set indentation
    globals$n_indent <- globals$n_indent + 1
    print_fun <- getFromNamespace("fetch_print_fun", "boomer")(.(print_fun), res)
    dots <- strrep(dot, globals$n_indent)
    on.exit({
      globals$n_indent <- globals$n_indent - 1
      # update last_total_time_end on exit, we do it this way so our total
      # time doesn't leave out the updating of the times df with this value
      .IF(clock, globals$last_total_time_end <- Sys.time())
    })

    .IF(!is.null(nm), {
      mask <- parent.env(parent.frame())
      if(isTRUE(mask$..FIRST_CALL..)) {
        cat(dots, rig_open, crayon::yellow(.(nm)),"\n", sep = "")

        .IF(print_args, {
          pf <- parent.frame()
          args <- names(eval.parent(quote(match.call())))[-1]
          for (arg in args) {
            tryCatch(
              error = function(e) {
                writeLines(paste0(
                  dots, c(crayon::green(arg, ": Couldn't evaluate"))))
              }, {
                # to do : make it CRAN compatible
                capture.output(arg_val <- suppressMessages(suppressWarnings(
                  eval(pryr:::promise_code(arg, pf), pryr:::promise_env(arg, pf)))))
                output <- capture.output(print_fun(arg_val))
                writeLines(paste0(
                  dots, c(crayon::green(arg, ":"), output)))
              })
          }
        })

        withr::defer_parent({
          cat(dots, rig_close, crayon::yellow(.(nm)),"\n", sep = "")
          mask$..FIRST_CALL.. <- TRUE

        })
        mask$..FIRST_CALL.. <- FALSE
      }
    })

    # manipulate call to use original function
    sc  <- sys.call()
    sc_bkp <- sc
    sc[[1]] <- .(fun_val)
    cat(dots, wrap_open, crayon::cyan(deparse1(sc_bkp[[1]])), "\n", sep ="")

    # evaluate call with original function
    .IF(clock, evaluation_time_start <- Sys.time())
    success <- FALSE
    error <- tryCatch(
      {
        res <- withVisible(rlang::eval_bare(sc, parent.frame()))
        success <- TRUE
      },
      error = identity
    )
    .IF(clock, evaluation_time_end <- Sys.time())
    if(success && !res$visible && .(visible_only)) return(invisible(res$value))

    # always display function call
    call_txt <- deparse1(sc_bkp, collapse = paste0("\n", strrep(" ", globals$n_indent + 3)))
    call_txt <- styler::style_text(call_txt)
    cat(
      dots,
      wrap_close,
      crayon::cyan(call_txt),
      "\n",
      sep ="")

    # rethrow on failure
    if (!success) {
      writeLines(crayon::magenta("Error:", paste0(class(error), collapse = "/")))
      stop(error)
    }

    # update the global `times` data frame and compute the true time
    .IF(clock, true_time_msg <-
          getFromNamespace("update_times_df_and_get_true_time", "boomer")(
            call, total_time_start, evaluation_time_start, evaluation_time_end))

    # otherwise print result
    res <- res$value
    .IF(clock, writeLines(crayon::blue(true_time_msg)))

    writeLines(c(paste0(dots, capture.output(print_fun(res))), dots))

    res
  })))
}

update_times_df_and_get_true_time <- function(
  call, total_time_start, evaluation_time_start, evaluation_time_end) {

  evaluation_time <- difftime(evaluation_time_end, evaluation_time_start, units = "secs")
  n <- nrow(globals$times)
  # spot sub-steps, they were already computed, but start after the current step,
  # and aren't marked as counted yet
  ind <-
    globals$times$total_time_start >=  evaluation_time_start &
    ! globals$times$counted
  if(n) {
    # update last value of total time end
    globals$times$total_time_end[n] <- globals$last_total_time_end
    # deduce total last time
    globals$times$total_time[n] <- difftime(
      globals$times$total_time_end[n],
      globals$times$total_time_start[n],
      units = "secs")
    # mark sub-steps as counted
    globals$times$counted[ind] <- TRUE
  }
  # the true time is the evaluation time minus the total overlapping time of
  # sub-steps
  overlap_time <- sum(globals$times$total_time[ind])
  true_time <- evaluation_time - overlap_time

  # assemble everything in a row and bind it to the global times data.frame
  times_row <- data.frame(
    call = deparse1(call),
    total_time_start,
    evaluation_time_start,
    evaluation_time_end,
    evaluation_time,
    true_time,
    total_time_end = Sys.time()[NA],
    total_time = double(1),
    counted = FALSE)

  globals$times <- rbind(globals$times, times_row)

  # build message with appropriate unit
  if(true_time < 1e-6) {
    true_time_msg <- paste("time:", round(true_time*1e6, 3), "us")
  } else if(true_time < 1e-3) {
    true_time_msg <- paste("time:", round(true_time*1e3, 3), "ms")
  } else {
    true_time_msg <- paste("time:", round(true_time, 3), "s")
  }
  true_time_msg
}

fetch_print_fun <- function(print_fun, res) {
  if(is.list(print_fun)) {
    use_default <- TRUE
    nms <- allNames(print_fun)
    default <- print_fun[nms == ""]
    if(length(default)) {
      default <- rlang::as_function(default[[1]])
    } else {
      default <- print
    }
    for(cl in setdiff(nms, "")) {
      if(inherits(res, cl)) {
        print_fun <- rlang::as_function(print_fun[[cl]])
        use_default <- FALSE
        break
      }
    }
    if(use_default) print_fun <- default
  }
  print_fun
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


reset_globals <- function() {
  # reset the global times table
  globals$times <- data.frame(
    call = character(),
    total_time_start = Sys.time()[0],
    evaluation_time_start = Sys.time()[0],
    evaluation_time_end = Sys.time()[0],
    evaluation_time = double(),
    true_time = double(),
    total_time_end = Sys.time()[0],
    total_time = double(),
    counted = logical())

  globals$last_total_time_end <- NULL

  invisible(NULL)
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
  environment(fun) <- mask
  fun
}

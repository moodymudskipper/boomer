# will contain `times` data frame and `last_total_time_end` POSIXct
globals <- new.env()
globals$n_indent <- -1

# copied from method::allNames
allNames <- function (x)
{
  value <- names(x)
  if (is.null(value))
    character(length(x))
  else value
}


wrap_clocked <- function(fun_val, print_fun, visible_only) {
  as.function(c(alist(...=), bquote({

    # start the clock
    total_time_start <- Sys.time()

    globals <- getFromNamespace("globals", "boomer")
    # set indentation
    globals$n_indent <- globals$n_indent + 1
    on.exit({
      globals$n_indent <- globals$n_indent - 1
      # update last_total_time_end on exit, we do it this way so our total
      # time doesn't leave out the updating of the times df with this value
      globals$last_total_time_end <- Sys.time()
      })

    # manipulate call to use original function
    sc  <- sys.call()
    sc_bkp <- sc
    sc[[1]] <- .(fun_val)

    # evaluate call with original function
    pf <- parent.frame()
    evaluation_time_start <- Sys.time()
    success <- FALSE
    error <- tryCatch(
      {
        res <- withVisible(rlang::eval_bare(sc, parent.frame()))
        success <- TRUE
      },
      error = identity
    )
    evaluation_time_end <- Sys.time()
    if(success && !res$visible && .(visible_only)) return(invisible(res$value))

    # always display function call
    # (must happen after evaluation so that calls are shown in order)
    call_chr <- deparse(sc_bkp)
    call_chr <- paste0(strrep("\ub7 ", globals$n_indent), call_chr)
    call_chr <- crayon::cyan(call_chr)
    writeLines(call_chr)

    # rethrow on failure
    if (!success) {
      writeLines(crayon::magenta("Error:", paste0(class(error), collapse = "/")))
      stop(error)
    }

    # update the global `times` data frame and compute the true time
    true_time_msg <- getFromNamespace("update_times_df_and_get_true_time", "boomer")(
      call, total_time_start, evaluation_time_start, evaluation_time_end)

    # otherwise print result
    res <- res$value
    writeLines(crayon::blue(true_time_msg))
    print_fun <- getFromNamespace("fetch_print_fun", "boomer")(.(print_fun), res)
    writeLines(capture.output(print_fun(res)))

    res
  })))
}

wrap_unclocked <- function(fun_val, print_fun, visible_only) {
  as.function(c(alist(...=), bquote({
    globals <- getFromNamespace("globals", "boomer")

    # set indentation
    globals$n_indent <- globals$n_indent + 1
    on.exit(globals$n_indent <- globals$n_indent - 1)

    # manipulate call to use original function
    sc  <- sys.call()
    sc_bkp <- sc
    sc[[1]] <- .(fun_val)

    # evaluate call with original function
    success <- FALSE
    error <- tryCatch(
      {
        res <- withVisible(rlang::eval_bare(sc, parent.frame()))
        success <- TRUE
      },
      error = identity
    )
    if(success && !res$visible && .(visible_only)) return(invisible(res$value))

    # always display function call
    # (must happen after evaluation so that calls are shown in order)
    call_chr <- deparse(sc_bkp)
    call_chr <- paste0(strrep("\ub7 ", globals$n_indent), call_chr)
    call_chr <- crayon::cyan(call_chr)
    writeLines(call_chr)

    # rethrow on failure
    if (!success) {
      writeLines(crayon::magenta("Error:", paste0(class(error), collapse = "/")))
      stop(error)
    }

    # otherwise print result
    res <- res$value
    print_fun <- getFromNamespace("fetch_print_fun", "boomer")(.(print_fun), res)
    writeLines(capture.output(print_fun(res)))
    res
  })))
}

wrap <- function(fun_val, clock, print_fun, visible_only) {
  if(clock) {
    wrap_clocked(fun_val, print_fun, visible_only)
  } else {
    wrap_unclocked(fun_val, print_fun, visible_only)
  }
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


double_colon <- function(clock, print_fun, visible_only) {
  if(clock) {
    function(pkg, name) {
      # code borrowed from base::`::`
      pkg <- as.character(substitute(pkg))
      name <- as.character(substitute(name))
      fun_val <- getExportedValue(pkg, name)

      wrap_clocked(fun_val, print_fun, visible_only)
    }
  } else {
    function(pkg, name) {
      # code borrowed from base::`::`
      pkg <- as.character(substitute(pkg))
      name <- as.character(substitute(name))
      fun_val <- getExportedValue(pkg, name)

      wrap_unclocked(fun_val, print_fun, visible_only)
    }
  }
}

triple_colon <- function(clock, print_fun, visible_only) {
  if(clock) {
    function(pkg, name) {
      # code borrowed from base::`:::`
      pkg <- as.character(substitute(pkg))
      name <- as.character(substitute(name))
      fun_val <- get(name, envir = asNamespace(pkg), inherits = FALSE)

      wrap_clocked(fun_val, print_fun, visible_only)
    }
  } else {
    function(pkg, name) {
      # code borrowed from base::`:::`
      pkg <- as.character(substitute(pkg))
      name <- as.character(substitute(name))
      fun_val <- get(name, envir = asNamespace(pkg), inherits = FALSE)

      wrap_unclocked(fun_val, print_fun, visible_only)
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
  funs <- setdiff(unique(funs), c(ignore, ")", "}", ",", "[", "]"))
  funs
}



build_shimmed_assign <- function(symbol, ignore, clock, print_fun, visible_only) {
  # return a function that shims an assignment operator
  if (symbol == "<-") {
    f <- eval(bquote(function(e1, e2) {
      E2 <- if (is.language(e2)) {
        substitute(quote(e2), list(e2 = e2))
      } else if (!is.function(e2)) {
        e2
      } else {
        wrap(e2, clock = .(clock), print_fun = .(print_fun), visible_only = .(visible_only))
      }
      invisible(eval.parent(substitute(.Primitive("<-")(e1, E2))))
    }))
  } else if (symbol == "=") {
    f <- eval(bquote(function(e1, e2) {
      E2 <- if (is.language(e2)) {
        substitute(quote(e2), list(e2 = e2))
      } else if (!is.function(e2)) {
        e2
      } else {
        wrap(e2, clock = .(clock), print_fun = .(print_fun), visible_only = .(visible_only))
      }
      invisible(eval.parent(substitute(.Primitive("=")(e1, E2))))
    }))
  }

  if (!symbol %in% ignore) {
    f <- wrap(f, clock = clock, print_fun = print_fun, visible_only = visible_only)
  }

  # need to put `f` in namespace so `wrap` is accessible (CRAN doesn't like `:::`)
  environment(f) <- asNamespace("boomer")
  f
}

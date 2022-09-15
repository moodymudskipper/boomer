#' wrap
#'
#' `wrap()` curries a function to signal when a call is entered and exited, and print
#' its output with appropriate indentation.
#'
#' The original function is the wrapped function, the output of `wrap()` is the
#' wrapper function.
#'
#' The `wrap()`er function optionally prints the execution time of the call.
#'
#' Indentation is done through a global variable `globals$n_indent`.
#'
#' If `wrap()` is called from a function rigged using `rig` or `rig_in_namespace`,
#' the `wrap()`per function will check if we entered the first call of the rigged
#' function by checking in `mask` if `..FIRST_CALL..` is `TRUE`. If it is we
#' signal that we entered the rigged function, and use `withr::defer` to
#' signal when the rigged function will be exited. This complex mechanism is
#' used so that the rigged function's body stays unchanged and boomer's behavior
#' can be more robust.
#'
#' If called from such a rigged function, a `wrap()`er function optionally
#' checks after each call if the arguments of the rigged function were evaluated,
#' and prints them as soon as they are.
#'
#' @inheritParams rig
#' @param fun_val the function to wrap
#' @param clock whether to clock
#' @param print_fun A function, a formula or a list of functions or formulas.
#' @param rigged_nm The name of the rigged function containing the wrapper function calls
#' @param wrapped_nm The name of the wrapped function
#' @param mask The enclosing environment of the rigged function, where wrapper functions are stored
#' @noRd
wrap <- function(fun_val, clock, print_fun, rigged_nm = NULL, wrapped_nm = NA, mask = NULL) {
  # for CRAN notes
  . <- NULL
  as.function(envir = asNamespace("boomer"), c(alist(...=), bquote({
    # Since we set the enclosing env to {boomer}'s namespace
    # we use `bquote()` to get `wrap()`'s arguments in

    # return early if function is to be ignored
    wrapped_nm <- .(wrapped_nm)
    fun_val   <- .(fun_val)
    ignore <- getOption("boomer.ignore")
    sc  <- sys.call()
    if(wrapped_nm %in% ignore) {
      res <- rlang::eval_bare(as.call(c(fun_val, as.list(sc[-1]))), parent.frame())
      return(res)
    }

    # start the clock
    clock <- .(clock)
    if(is.null(clock)) clock <- getOption("boomer.clock")
    total_time_start <- if(clock) Sys.time()

    # fetch other args
    print_fun <- .(print_fun)
    rigged_nm <- .(rigged_nm)
    mask      <- .(mask)

    # gather other options at run time
    if(is.null(print_fun)) print_fun <- getOption("boomer.print")
    visible_only <- getOption("boomer.visible_only")
    print_args <- getOption("boomer.print_args")
    safe_print <- getOption("boomer.safe_print")


    wrapped_fun_caller_env <- parent.frame()
    # fetch rigged function's execution env, it's the wrapped_fun_caller_env
    # only at the top level
    rigged_fun_exec_env <- wrapped_fun_caller_env
    while (!identical(parent <- parent.env(rigged_fun_exec_env), mask)) {
      rigged_fun_exec_env <- parent
    }

    # set indentation
    globals$n_indent <- globals$n_indent + 1

    # set emojis
    ej <- set_emojis(safe_print, globals$n_indent)

    # reset indentation and update times
    on.exit(update_globals_on_exit(clock))

    # !!! this adds calls on.exit of caller (rigged) function !!!
    signal_rigged_function_and_args(rigged_nm, mask, ej, print_args, rigged_fun_exec_env)

    # build calls to be displayed on top and bottom of wrapped call
    deparsed_calls <- build_deparsed_calls(sc, ej, globals$n_indent)

    # display wrapped call at the top if relevant
    if(!is.null(deparsed_calls$open)) {
      cat(deparsed_calls$open, "\n")
    }

    # evaluate call with original wrapped function
    res <- try(eval_wrapped_call(sc, fun_val, clock, wrapped_fun_caller_env), silent = TRUE)
    success <- !inherits(res, "try-error")

    # if rigged fun args have been evaled, print them
    print_arguments(print_args, rigged_nm, mask, print_fun, ej, rigged_fun_exec_env)

    # display wrapped call at the bottom
    cat(deparsed_calls$close, "\n")

    # rethrow error on failure
    if (!success) {
      error <- attr(res, "condition")
      writeLines(crayon::magenta("Error:", paste0(class(error), collapse = "/")))
      stop(error)
    }

    # return invisible result early
    if(!res$visible && visible_only) {
      return(invisible(res$value))
    }

    # update the global `times` data frame and compute the true time
    if(clock) {
      true_time_msg <- update_times_df_and_get_true_time(
        call, total_time_start, res$evaluation_time_start, res$evaluation_time_end)
      writeLines(crayon::blue(true_time_msg))
    }

    # print output with appropriate print fun and indentation
    res <- res$value
    print_fun <- fetch_print_fun(print_fun, res)
    writeLines(c(paste0(ej$dots, capture.output(print_fun(res))), ej$dots))

    res
  })))
}

set_emojis <- function(safe_print, n_indent) {
  ej <- list()
  if (safe_print) {
    ej$rig_open   <- crayon::bold(crayon::yellow("{ "))
    ej$rig_close  <- crayon::bold(crayon::yellow("} "))
    ej$wrap_open  <- crayon::bold(crayon::yellow("<  "))
    ej$wrap_close <- crayon::bold(crayon::yellow(">  "))
    ej$dots       <- crayon::yellow(strrep(". ", n_indent))
  } else {
    # nocov start
    ej$rig_open   <- "\U0001f447 "
    ej$rig_close  <- "\U0001f446 "
    ej$wrap_open  <- "\U0001f4a3 "
    ej$wrap_close <- "\U0001f4a5 "
    ej$dots       <- crayon::yellow(strrep("\ub7 ", n_indent))
    # nocov end
  }
  ej
}

update_globals_on_exit <- function(clock) {
  globals$n_indent <- globals$n_indent - 1
  # update last_total_time_end on exit, we do it this way so our total
  # time doesn't leave out the updating of the times df with this value
  if(clock) globals$last_total_time_end <- Sys.time()
  invisible(NULL)
}

signal_rigged_function_and_args <- function(rigged_nm, mask, ej, print_args, rigged_fun_exec_env) {
  # is the wrapped function called by a rigged function?
  if(!is.null(rigged_nm)) {
    # is this wrapped function call the first of the body?
    if(mask$..FIRST_CALL..) {
      # load pryr early to print early "Registered S3 method overwritten..."
      if(print_args) loadNamespace("pryr")

      cat(ej$dots, ej$rig_open, crayon::yellow(rigged_nm),"\n", sep = "")

      # when exiting rigged function, inform and reset ..FIRST_CALL..
      withr::defer({
        cat(ej$dots, ej$rig_close, crayon::yellow(rigged_nm),"\n", sep = "")
        mask$..FIRST_CALL.. <- TRUE
        mask$..EVALED_ARGS..[] <- FALSE
      }, envir = rigged_fun_exec_env)

      mask$..FIRST_CALL.. <- FALSE
    }
  }
}

build_deparsed_calls <- function(sc, ej, n_indent) {
  # manipulate call to use original function
  sc <- sc

  deparsed_calls <- list()

  call_chr <- deparse(sc)
  call_chr <- styler::style_text(call_chr)

  # if all args are "atomic" (symbols or numbers) we can print open and close in one go
  all_args_are_atomic <- all(lengths(as.list(sc[-1])) == 1)
  # we need a workaround for magrittr here
  no_dot_in_args <- !any(sapply(sc[-1], identical, quote(.)))
  if(length(call_chr) == 1) {
    if(all_args_are_atomic && no_dot_in_args) {
      deparsed_calls$close <-
        paste0(ej$dots, ej$wrap_open, ej$wrap_close, crayon::cyan(call_chr))
    } else {
      deparsed_calls$close <- paste0(ej$dots, ej$wrap_close, crayon::cyan(call_chr))
      if(getOption("boomer.abbreviate")) {
        call_chr <- deparse1(sc[[1]])
      }
      deparsed_calls$open <- paste0(ej$dots, ej$wrap_open, crayon::cyan(call_chr))

      if(crayon::col_nchar(deparsed_calls$open) > 80) {
        deparsed_calls$open <- paste0(
          crayon::col_substr(deparsed_calls$open, 1, 77), crayon::cyan("..."))
      }
    }
  } else {
    if(all_args_are_atomic && no_dot_in_args) {
      line1 <- paste0(ej$dots, ej$wrap_open, ej$wrap_close, crayon::cyan(call_chr[1]))
      other_lines <-  paste0(ej$dots, "      ", crayon::cyan(call_chr[-1]))
      deparsed_calls$close <- paste(c(line1, other_lines), collapse = "\n")
    } else {
      line1 <- paste0(ej$dots, ej$wrap_close, crayon::cyan(call_chr[1]))
      other_lines <-  paste0(ej$dots, "   ", crayon::cyan(call_chr[-1]))
      deparsed_calls$close <-  paste(c(line1, other_lines), collapse = "\n")
      if(getOption("boomer.abbreviate")) {
        call_chr <- deparse1(sc[[1]])
      }
      if(length(call_chr) > 1) {
        call_chr <- paste0(call_chr[1], "...")
      }
      deparsed_calls$open <- paste0(ej$dots, ej$wrap_open, crayon::cyan(call_chr))

      if(crayon::col_nchar(deparsed_calls$open) > 80) {
        # couldn' find example to test this so using nocov, but it's he same as above
        # nocov start
        deparsed_calls$open <- paste0(
          crayon::col_substr(deparsed_calls$open, 1, 77), crayon::cyan("..."))
        # nocov end
      }
    }
  }
  deparsed_calls
}

eval_wrapped_call <- function(sc, fun_val, clock, rigged_fun_exec_env) {
  sc[[1]] <- fun_val
  if (clock) {
    evaluation_time_start <- Sys.time()
    res <- withVisible(rlang::eval_bare(sc, rigged_fun_exec_env))
    res$evaluation_time_end <- Sys.time()
    res$evaluation_time_start <- evaluation_time_start
  } else {
    res <- withVisible(rlang::eval_bare(sc, rigged_fun_exec_env))
  }
  res
}

print_arguments <- function(print_args, rigged_nm, mask, print_fun, ej, rigged_fun_exec_env) {
  rigged <- !is.null(rigged_nm)
  if(!print_args || ! rigged) return(invisible(NULL))
  for (arg in names(mask$..EVALED_ARGS..)) {
    if(!mask$..EVALED_ARGS..[[arg]]) {
      evaled <- promise_evaled_fixed(arg, rigged_fun_exec_env)
      if(evaled) {
        mask$..EVALED_ARGS..[[arg]] <- TRUE
        arg_val <- get(arg, envir = rigged_fun_exec_env)
        print_fun <- fetch_print_fun(print_fun, arg_val)
        output <- capture.output(print_fun(arg_val))
        writeLines(paste0(
          ej$dots, c(crayon::green(arg, ":"), output)))
      }
    }
  }
}

promise_evaled <- getFromNamespace("promise_evaled", "pryr")
# fixed so it returns FALSE if arg is missing
promise_evaled_fixed <- function (name, env) {
  expr <- substitute(missing(ARG), list(ARG = as.symbol(name)))
  expr[[1]] <- missing # so it does not collide with a local definition of `missing()`
  arg_is_missing <- eval(expr, env)
  if (arg_is_missing) return(FALSE)
  promise_evaled(name, env)
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
  #nocov start
  if(true_time < 1e-6) {
    true_time_msg <- paste("time:", round(true_time*1e6, 3), "us")
  } else if(true_time < 1e-3) {
    true_time_msg <- paste("time:", round(true_time*1e3, 3), "ms")
  } else {
    true_time_msg <- paste("time:", round(true_time, 3), "s")
  }
  #nocov end
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


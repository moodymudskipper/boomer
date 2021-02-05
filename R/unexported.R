# will contain `times` data frame and `last_total_time_end` POSIXct
globals <- new.env()

# copied from method::allNames
allNames <- function (x)
{
  value <- names(x)
  if (is.null(value))
    character(length(x))
  else value
}


wrap <- function(fun_val, clock, print_fun) {
  if(clock) {
    f <- as.function(c(alist(...=), bquote({
      # start the clock
      total_time_start <- Sys.time()

      # manipulate call to use original function
      sc  <- sys.call()
      sc_bkp <- sc
      sc[[1]] <- .(fun_val)

      # evaluate call with original function and clock it
      pf <- parent.frame()
      evaluation_time_start <- Sys.time()
      res <- rlang::eval_bare(sc, pf)
      evaluation_time_end <- Sys.time()

      # update the global `times` data frame and compute the true time
      true_time_msg <- getFromNamespace("update_times_df_and_get_true_time", "boomer")(
        call, total_time_start, evaluation_time_start, evaluation_time_end)

      # update last_total_time_end on exit, we do it this way so our total
      # time doesn't leave out the updating of the times df with this value
      globals <- getFromNamespace("globals", "boomer")
      on.exit(globals$last_total_time_end <- Sys.time())

      # display side effects
      writeLines(crayon::cyan(deparse(sc_bkp)))
      writeLines(crayon::blue(true_time_msg))
      print_fun <- getFromNamespace("fetch_print_fun", "boomer")(.(print_fun), res)
      writeLines(capture.output(print_fun(res)))

      res
    })))
  } else {
    f <- as.function(c(alist(...=), bquote({
      # manipulate call to use original function
      sc  <- sys.call()
      sc_bkp <- sc
      sc[[1]] <- .(fun_val)
      # evaluate call with original function
      res <- rlang::eval_bare(sc, parent.frame())
      # display side effects
      writeLines(crayon::cyan(deparse(sc_bkp)))
      print_fun <- getFromNamespace("fetch_print_fun", "boomer")(.(print_fun), res)
      writeLines(capture.output(print_fun(res)))
      res
    })))
  }
  f
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


double_colon <- function(clock, print_fun) {
  if(clock) {
    function(pkg, name) {
      # code borrowed from base::`::`
      pkg <- as.character(substitute(pkg))
      name <- as.character(substitute(name))
      fun_val <- getExportedValue(pkg, name)

      as.function(c(alist(...=), bquote({
        # start the clock
        total_time_start <- Sys.time()

        # manipulate call to use original function
        sc  <- sys.call()
        sc_bkp <- sc
        sc[[1]] <- .(fun_val)

        # evaluate call with original function and clock it
        pf <- parent.frame()
        evaluation_time_start <- Sys.time()
        res <- rlang::eval_bare(sc, pf)
        evaluation_time_end <- Sys.time()

        # update the global `times` data frame and compute the true time
        true_time_msg <- getFromNamespace("update_times_df_and_get_true_time", "boomer")(
          call, total_time_start, evaluation_time_start, evaluation_time_end)

        # update last_total_time_end on exit, we do it this way so our total
        # time doesn't leave out the updating of the times df with this value
        globals <- getFromNamespace("globals", "boomer")
        on.exit(globals$last_total_time_end <- Sys.time())

        # display side effects
        writeLines(crayon::cyan(deparse(sc_bkp)))
        writeLines(crayon::blue(true_time_msg))
        print_fun <- getFromNamespace("fetch_print_fun", "boomer")(.(print_fun), res)
        writeLines(capture.output(print_fun(res)))

        res
      })))
    }
  } else {
    function(pkg, name) {
      # code borrowed from base::`::`
      pkg <- as.character(substitute(pkg))
      name <- as.character(substitute(name))
      fun_val <- getExportedValue(pkg, name)

      as.function(c(alist(...=), bquote({
        # manipulate call to use original function
        sc  <- sys.call()
        sc_bkp <- sc
        sc[[1]] <- .(fun_val)
        # evaluate call with original function
        res <- rlang::eval_bare(sc, parent.frame())
        writeLines(crayon::cyan(deparse(sc_bkp)))
        print_fun <- getFromNamespace("fetch_print_fun", "boomer")(.(print_fun), res)
        writeLines(capture.output(print_fun(res)))
        res
      })))
    }
  }
}

triple_colon <- function(clock, print_fun) {
  if(clock) {
    function(pkg, name) {
      # code borrowed from base::`:::`
      pkg <- as.character(substitute(pkg))
      name <- as.character(substitute(name))
      fun_val <- get(name, envir = asNamespace(pkg), inherits = FALSE)
      as.function(c(alist(...=), bquote({
        # start the clock
        total_time_start <- Sys.time()

        # manipulate call to use original function
        sc  <- sys.call()
        sc_bkp <- sc
        sc[[1]] <- .(fun_val)

        # evaluate call with original function and clock it
        pf <- parent.frame()
        evaluation_time_start <- Sys.time()
        res <- rlang::eval_bare(sc, pf)
        evaluation_time_end <- Sys.time()

        # update the global `times` data frame and compute the true time
        true_time_msg <- getFromNamespace("update_times_df_and_get_true_time", "boomer")(
          call, total_time_start, evaluation_time_start, evaluation_time_end)

        # update last_total_time_end on exit, we do it this way so our total
        # time doesn't leave out the updating of the times df with this value
        globals <- getFromNamespace("globals", "boomer")
        on.exit(globals$last_total_time_end <- Sys.time())

        # display side effects
        writeLines(crayon::cyan(deparse(sc_bkp)))
        writeLines(crayon::blue(true_time_msg))
        print_fun <- getFromNamespace("fetch_print_fun", "boomer")(.(print_fun), res)
        writeLines(capture.output(print_fun(res)))

        res
      })))
    }
  } else {
    function(pkg, name) {
      # code borrowed from base::`:::`
      pkg <- as.character(substitute(pkg))
      name <- as.character(substitute(name))
      fun_val <- get(name, envir = asNamespace(pkg), inherits = FALSE)
      as.function(c(alist(...=), bquote({
        # manipulate call to use original function
        sc  <- sys.call()
        sc_bkp <- sc
        sc[[1]] <- .(fun_val)
        # evaluate call with original function
        res <- rlang::eval_bare(sc, parent.frame())
        writeLines(crayon::cyan(deparse(sc_bkp)))
        print_fun <- getFromNamespace("fetch_print_fun", "boomer")(.(print_fun), res)
        writeLines(capture.output(print_fun(res)))
        res
      })))
    }
  }
}

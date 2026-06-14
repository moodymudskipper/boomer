# will contain `times` data frame and `last_total_time_end` POSIXct
globals <- new.env()
globals$n_indent <- -1
globals$shiny_rigged <- FALSE
# `log_open` is TRUE while a file log entry is being buffered, `rigged_depth`
# counts the currently open rigged functions; together with `n_indent` they tell
# us when a top-level entry has fully finished (see `maybe_finalize_log_entry()`).
# `log_buffer`/`log_timestamp` accumulate the current entry before it is flushed.
globals$log_open <- FALSE
globals$rigged_depth <- 0
globals$log_buffer <- character()
globals$log_timestamp <- NULL

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
  globals$log_open <- FALSE
  globals$rigged_depth <- 0
  globals$log_buffer <- character()
  globals$log_timestamp <- NULL

  invisible(NULL)
}

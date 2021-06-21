# will contain `times` data frame and `last_total_time_end` POSIXct
globals <- new.env()
globals$n_indent <- -1

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

#' Switch "boom" debugging on and off
#'
#' While debugging a function, call `boom_on()` and all subsequent calls will be boomed,
#' call `boom_off()` to return to standard debugging.
#' @inheritParams boom
#' @export
#' @return Returns `NULL` invisibly, called for side effects.
boom_on <- function(clock = NULL, print = NULL) {
  fun <- sys.function(-1)
  rigged_fun <- rig_impl(fun, clock, print, rigged_nm = NULL)
  e <- parent.frame()
  parent.env(e) <- environment(rigged_fun)
  invisible(NULL)
}

#' @export
#' @rdname boom_on
boom_off <- function() {
  e <- parent.frame()
  parent.env(e) <- parent.env(parent.env(e))
  invisible(NULL)
}

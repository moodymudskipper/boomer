#' @importFrom stats setNames
#' @importFrom utils getParseData head capture.output
#' @importFrom methods allNames formalArgs
NULL

promise_evaled <- getFromNamespace("promise_evaled", "pryr")

.onLoad <- function(libname, pkgname) {
  #nocov start
  op <- options()
  op.boom <- list(
    boomer.clock = FALSE,
    boomer.print = print,
    boomer.ignore = c("~", "{", "(", "<-", "<<-", "="),
    boomer.visible_only = FALSE,
    boomer.print_args = TRUE,
    boomer.safe_print = FALSE,
    boomer.abbreviate = FALSE
  )
  toset <- !(names(op.boom) %in% names(op))
  if(any(toset)) options(op.boom[toset])

  invisible(NULL)
  #nocov end
}

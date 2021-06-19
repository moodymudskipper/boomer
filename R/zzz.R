#' @importFrom stats setNames
#' @importFrom utils getParseData head capture.output
#' @importFrom methods allNames
NULL

.onLoad <- function(libname, pkgname) {
  #nocov start
  op <- options()
  op.boom <- list(
    boom.clock = FALSE,
    boom.print = print,
    boom.ignore = c("~", "{", "(", "<-", "<<-", "="),
    boom.visible_only = FALSE,
    boom.print_args = FALSE,
    boom.safe_print = FALSE
  )
  toset <- !(names(op.boom) %in% names(op))
  if(any(toset)) options(op.boom[toset])

  invisible(NULL)
  #nocov end
}

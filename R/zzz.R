#' @importFrom stats setNames
#' @importFrom utils getParseData head capture.output
#' @importFrom methods allNames formalArgs
#' @importFrom pryr address
NULL

op.boom <- list(
  boomer.clock = FALSE,
  boomer.print = print,
  boomer.ignore = c("~", "{", "(", "<-", "<<-", "="),
  boomer.visible_only = FALSE,
  boomer.print_args = TRUE,
  boomer.safe_print = FALSE,
  boomer.abbreviate = FALSE
)

.onLoad <- function(libname, pkgname) {
  #nocov start
  op <- options()
  toset <- !(names(op.boom) %in% names(op))
  if(any(toset)) options(op.boom[toset])
  #nocov end
}

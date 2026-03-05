#' @importFrom stats setNames
#' @importFrom utils getParseData head capture.output
#' @importFrom methods allNames formalArgs
NULL

op.boom <- list(
  boomer.clock = FALSE,
  boomer.print = print,
  boomer.ignore = c("~", "{", "(", "<-", "<<-", "="),
  boomer.visible_only = FALSE,
  boomer.print_args = TRUE,
  boomer.safe_print = FALSE,
  boomer.abbreviate = FALSE,
  boomer.max_indent = 10L,
  boomer.theme.args = "green",
  boomer.theme.rigged_fun = "yellow",
  boomer.theme.code = "cyan"
)

.onLoad <- function(libname, pkgname) {
  #nocov start
  op <- options()
  toset <- !(names(op.boom) %in% names(op))
  if(any(toset)) options(op.boom[toset])
  #nocov end
}

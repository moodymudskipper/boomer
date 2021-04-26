.onLoad <- function(libname, pkgname) {
  #nocov start
  op <- options()
  op.boom <- list(
    boom.clock = FALSE,
    boom.print = print,
    boom.ignore = c("~", "{", "(", "<-", "<<-", "=")
  )
  toset <- !(names(op.boom) %in% names(op))
  if(any(toset)) options(op.boom[toset])

  invisible(NULL)
  #nocov end
}

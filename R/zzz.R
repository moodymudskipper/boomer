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
    boomer.abbreviate = FALSE,
    boomer.rig_shiny = TRUE
  )
  toset <- !(names(op.boom) %in% names(op))
  if(any(toset)) options(op.boom[toset])

  # setHook(
  #   packageEvent("shiny", "onLoad"),
  #   function(...) {
  #     # create modified ("rigged") copies of shiny functions
  #     shiny_reactive_funs <-
  #       c("reactive", "renderCachedPlot", "renderDataTable", "renderImage",
  #         "renderPlot", "renderPrint", "renderTable", "renderText", "renderUI")
  #     globals$shiny_rigged_copies <- sapply(shiny_reactive_funs, rig_shiny_fun)
  #
  #     # "trace" shiny's functions in the namespace, so they fire the modified
  #     # copies if the "rig_shiny" option is TRUE
  #     for (fun in shiny_reactive_funs) {
  #       trace_shiny_reactive_fun(fun)
  #     }
  #     trace_shinyApp()
  #   }
  # )

  invisible(NULL)
  #nocov end
}

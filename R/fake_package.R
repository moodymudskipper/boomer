#' simulate a package
#'
#' simulate loading a namespace or attaching a package. designed to make reprexes
#' for `rig_in_namespace()` easy. The input function's closer env is set to
#' the namespace and no fancy feature such as registering S3 methods is supported
#'
#' @param name string, name of fake package
#' @param exported named list of exported functions
#' @param unexported named list of unexported functions
#' @param attach whether to attach the fake package
#'
#' @noRd
fake_package <- function(name, exported = NULL, unexported = NULL, attach = TRUE) {
  # for CRAN notes
  makeNamespace <- NULL
  # fetch and eval call to create `makeNamespace`
  eval(body(loadNamespace)[[c(8, 4, 4)]])
  # create an empty namespace
  ns <- makeNamespace(name)
  # makethis namespace the closure env of our input functions
  exported <- lapply(exported, `environment<-`, ns)
  unexported <- lapply(unexported, `environment<-`, ns)
  # place these in the namespace
  list2env(exported, ns)
  list2env(unexported, ns)
  # export relevant functions
  namespaceExport(ns, names(exported))
  if(attach) {
    # copy exported funs to "package:pkg" envir of the search path
    match.fun("attach")(exported, name = paste0("package:", name))
  }
  invisible()
}

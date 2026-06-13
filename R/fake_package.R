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
#' @param s3 list of `c(generic, class)` character pairs to register as S3
#'   methods. The method function must be present in `exported` or `unexported`
#'   under the name `paste(generic, class, sep = ".")`.
#'
#' @noRd
fake_package <- function(name, exported = NULL, unexported = NULL, attach = TRUE, s3 = NULL) {
  # for CRAN notes
  makeNamespace <- NULL
  # fetch and eval the call that defines `makeNamespace` inside `loadNamespace()`
  makeNamespace_call <- Find(
    function(e) is.call(e) &&
      identical(e[[1]], as.symbol("<-")) &&
      identical(e[[2]], as.symbol("makeNamespace")),
    as.list(body(loadNamespace)[[c(8, 4)]])
  )
  eval(makeNamespace_call)
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
  # register S3 methods so they end up in the relevant S3 methods table, as a
  # real package would
  for (m in s3) {
    method_nm <- paste(m[[1]], m[[2]], sep = ".")
    registerS3method(m[[1]], m[[2]], get(method_nm, ns), envir = ns)
  }
  if(attach) {
    # copy exported funs to "package:pkg" envir of the search path
    match.fun("attach")(exported, name = paste0("package:", name))
  }
  invisible()
}

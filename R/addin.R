boom_addin <- function() {
  # nocov start

  ## fetch context and selection
  context <- rstudioapi::getSourceEditorContext()
  selection <- rstudioapi::primary_selection(context)[["text"]]
  eval.parent(str2lang(paste0("boomer::boom(",selection, ")")))
  invisible()
  # nocov end
}

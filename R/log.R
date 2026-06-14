# Central output routing for boomer.
#
# `getOption("boomer.log")` is a character vector that may contain "console"
# (write to the console, keeping colors) and/or paths to files (typically
# ".log" or ".txt", written without ANSI styling). It defaults to "console",
# i.e. boomer's historical behaviour of printing to the console only.
boom_log_destinations <- function() {
  log <- getOption("boomer.log", "console")
  if (is.null(log) || !length(log)) {
    return("console")
  }
  log
}

# Emit already-assembled `text` (which may contain ANSI styling and newlines)
# to every configured destination: to the console as-is, and to each file with
# ANSI styling stripped.
boom_emit <- function(text) {
  log <- boom_log_destinations()
  if ("console" %in% log) {
    cat(text)
  }
  files <- setdiff(log, "console")
  if (length(files)) {
    plain <- cli::ansi_strip(text)
    for (path in files) {
      cat(plain, file = path, append = TRUE)
    }
  }
  invisible(NULL)
}

# `cat()`- and `writeLines()`-like helpers that route their output through
# `boom_emit()`. They assemble the exact same text the base functions would
# print, so swapping `cat()`/`writeLines()` for these is display-neutral.
boom_cat <- function(..., sep = " ") {
  boom_emit(paste(c(...), collapse = sep))
}

boom_writelines <- function(text) {
  boom_emit(paste0(paste(text, collapse = "\n"), "\n"))
}

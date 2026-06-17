# Central output routing for boomer.
#
# `getOption("boomer.log")` is a character vector that may contain "console"
# (write to the console, keeping colors) and/or paths to files (typically
# ".log" or ".txt", written without ANSI styling). It defaults to "console",
# i.e. boomer's historical behaviour of printing to the console only.
#
# In files, each top-level call produces one entry: a commented line with a
# timestamp, the (ANSI-stripped) exploded output, then a blank line, so that
# successive entries don't pile on top of each other.
boom_log_files <- function() {
  log <- getOption("boomer.log", "console")
  if (is.null(log) || !length(log)) {
    return(character())
  }
  setdiff(log, "console")
}

boom_logs_to_console <- function() {
  log <- getOption("boomer.log", "console")
  is.null(log) || !length(log) || "console" %in% log
}

# Emit already-assembled `text` (which may contain ANSI styling and newlines)
# to the console as-is (live), and buffer an ANSI-stripped copy for the files.
# The buffered entry is flushed as one unit when the top-level call completes,
# so each entry is cleanly framed regardless of how the trace happens to end.
boom_emit <- function(text) {
  if (boom_logs_to_console()) {
    cat(text)
  }
  if (length(boom_log_files())) {
    if (!isTRUE(globals$log_open)) {
      globals$log_open <- TRUE
      globals$log_buffer <- character()
      globals$log_timestamp <- format(Sys.time())
    }
    globals$log_buffer <- c(globals$log_buffer, cli::ansi_strip(text))
  }
  invisible(NULL)
}

# Header written once at the top of a freshly created log file, so that a reader
# (human or AI) encountering the file can tell what it is and how to read it.
boom_log_header <- paste0(
  "# Built by the boomer R package (https://github.com/moodymudskipper/boomer)\n",
  "# This is a boomer execution log, it displays the intermediate values computed inside a function.\n",
  "# Each expression is shown entering, then its successful return value.\n",
  "#   \U0001f4a3 <expr>   = expression about to be evaluated\n",
  "#   \U0001f4a5 <expr>   = same expression, followed by the value it returned\n",
  "#   ·····       = call-nesting depth (more dots = deeper in the stack)\n",
  "#   time: ...   = wall-clock time for the evaluation it precedes\n",
  "#   value blocks describe the values\n",
  "# Logged errors are errors in the inspected code, not boomer\n",
  "# The final depth-0 \U0001f4a5 is the overall return value.\n\n"
)

# Flush the buffered file log entry once the whole top-level call has unwound:
# all wrapped calls have exited (`n_indent < 0`) and all rigged functions have
# signalled their exit (`rigged_depth <= 0`). Called from both unwind points
# since either may happen last. The entry is written as a timestamp header, the
# trace (trailing whitespace trimmed), and a blank separator line. Files created
# by this write get the descriptive `boom_log_header` prepended first.
maybe_finalize_log_entry <- function() {
  if (isTRUE(globals$log_open) && globals$n_indent < 0 && globals$rigged_depth <= 0) {
    body <- trimws(paste0(globals$log_buffer, collapse = ""), which = "right")
    entry <- paste0("# ", globals$log_timestamp, "\n", body, "\n\n")
    for (path in boom_log_files()) {
      if (!file.exists(path)) {
        cat(boom_log_header, file = path)
      }
      cat(entry, file = path, append = TRUE)
    }
    globals$log_buffer <- character()
    globals$log_open <- FALSE
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

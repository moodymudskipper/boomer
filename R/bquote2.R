#' Partial substitution in expressions
#'
#' Modified `bquote()` to interpolate code conditionally in a readable way.
#' Use `.IF(cond, yes, no)` for conditional interpolation, `cond` will be evaluated
#' and `yes` and `no` will be quoted. `no` is optional.
#'
#' @inheritParams base::bquote
#' @keywords internal
#' @examples
#' fun <- function(x, y, cond) {
#'   boomer:::bquote2({
#'     some_long_code_chunk_1(.(x))
#'     .IF(cond, do_something())
#'     some_long_code_chunk_2(.(y))
#'   })
#' }
#' fun(1, 2, TRUE)
#' fun(1, 2, FALSE)
bquote2 <- function (expr, where = parent.frame(), splice = FALSE)
{
  if (!is.environment(where))
    # nocov start
    where <- as.environment(where)
    # nocov end
  unquote <- function(e) {
    if (is.pairlist(e))
      # nocov start
      as.pairlist(lapply(e, unquote))
      # nocov end
    else if (is.call(e)) {
      if (is.name(e[[1L]]) && as.character(e[[1]]) == ".")
        eval(e[[2L]], where)
      # EDIT STARTS HERE
      else if (is.name(e[[1L]]) && as.character(e[[1]]) == ".IF") {
        cond <- eval(e[[2L]], where)
        if(cond) {
          unquote(e[[3L]])
        } else {
          if (length(e) == 4) {
            unquote(e[[4L]])
          } else {
            NULL # quote(expr =)
          }
        }
        # EDIT ENDS HERE
      } else if (splice) {
        # nocov start
        if (is.name(e[[1L]]) && as.character(e[[1L]]) ==
            "..")
          stop("can only splice inside a call",
               call. = FALSE)
        else as.call(unquote.list(e))
        # nocov end
      }
      else {
        e <- as.call(lapply(e, unquote))
        if(is.name(e[[1L]]) && as.character(e[[1]]) == "{") {
          for(i in rev(seq_along(e)[-1])) {
            if(is.null(e[[i]])) e[[i]] <- NULL
          }
        }
        e
      }
    }
    else e
  }
  is.splice.macro <- function(e) is.call(e) && is.name(e[[1L]]) &&
    as.character(e[[1L]]) == ".."
  unquote.list <- function(e) {
    # nocov start
    p <- Position(is.splice.macro, e, nomatch = NULL)
    if (is.null(p))
      lapply(e, unquote)
    else {
      n <- length(e)
      head <- if (p == 1)
        NULL
      else e[1:(p - 1)]
      tail <- if (p == n)
        NULL
      else e[(p + 1):n]
      macro <- e[[p]]
      mexp <- eval(macro[[2L]], where)
      if (!is.vector(mexp))
        stop("can only splice vectors")
      c(lapply(head, unquote), mexp, as.list(unquote.list(tail)))
    }
    # nocov end
  }
  unquote(substitute(expr))
}

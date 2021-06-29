#' @importFrom stats setNames
#' @importFrom utils getParseData head capture.output
#' @importFrom methods allNames formalArgs
#' @aliases boomer boomer-package
#' @details
#'
#' * [boom()] displays the intermediate results of a call or a code chunk.
#' * [rig()] creates a copy of a function which will display the intermediate
#' results of all the calls of it body.
#' * [rig_in_namespace()] rigs a namespaced function in place, so its always
#' verbose even when called by other existing functions. It is especially handy
#' for package development.
#' * [rigger()] provides a convenient way to rig an
#' anonymous function by using the `rigger(...) + function(...) {...}` syntax.
#' * The addin "Explode a call with 'boom()'" provides a way to `boom()` a call
#'   with a keyboard shortcut.
#'
#' @section Package options:
#'
#' Several options impact the display of exploded calls :
#'
#' - `boomer.print`: If the `print` argument is not provided, this option will
#'   replace it at run time. Defaults to the `base::print` function.
#' - `boomer.clock`: If the `clock` argument is not provided, this option will
#'   replace it at run time. Defaults to `FALSE`.
#' - `boomer.print_args`: Whether to print the arguments of rigged functions
#'   and their values when they are evaluated. Defaults to `TRUE`.
#' - `boomer.visible_only`: Whether to hide the output of functions which return
#'   invisibly. Defaults to `FALSE`.
#' - `boomer.ignore`: Vector of functions for which we don't want the result
#'   printed (usually because it's redundant). Defaults to
#'   `c("~", "{", "(", "<-", "<<-", "=")`
#' - `boomer.safe_print`: Whether to replace emoticons by characters compatible
#'   with all systems. This is useful for reprexes (see {reprex} package) and
#'   for knitted report in case the ouput of those doesn't look good on your system.
#' - `boomer.abbreviate`: Whether to show only the function's name rather than the
#'   call when it's entered.
#'
"_PACKAGE"

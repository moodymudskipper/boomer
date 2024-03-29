% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/exported.R
\name{boom}
\alias{boom}
\alias{rig}
\alias{rigger}
\alias{rig_in_namespace}
\title{Print the Output of Intermediate Steps of a Call}
\usage{
boom(expr, clock = NULL, print = NULL)

rig(fun, clock = NULL, print = NULL)

rigger(clock = NULL, print = NULL)

rig_in_namespace(..., clock = NULL, print = NULL)
}
\arguments{
\item{expr}{call to explode}

\item{clock}{whether to time intermediate steps. Defaults to \code{getOption("boomer.clock")}
evaluated at run time (\code{FALSE} unless you change it). The execution time of
a step doesn't include the execution time of its previously printed sub-steps.}

\item{print}{A function, a formula or a list of functions or formulas, used to
modify the way the output is printed. Defaults to \code{getOption("boomer.print")}
evaluated at run time (\code{base::print} unless you change it)'.}

\item{fun}{function ro \code{rig()}}

\item{...}{Functions to rig in their namespace

If the \code{print} argument is a function, it will be used to print, or to transform the output
before it's printed. Use \code{invisible} to display nothing, useful possibilities are
\code{str} or \code{dplyr::glimpse}.

\emph{{rlang}}'s formula notation is supported, so for instance you can type:
\code{print = ~ dplyr::glimpse(., width = 50)}.

Sometimes you might want to print a specific type of object in a custom way,
in this case you can provide a named list, if you provide an unnamed element
it will be used as the default, and named elements will define how objects
of the given S3 class are printed. For instance \code{print = list(str, data.frame = tibble::as_tibble)}}
}
\value{
\code{boom()} returns the output of the call. \code{rig()} returns the modified
input function. \code{rig_in_namespace()} returns \code{invisible(NULL)} and is called
for side effects. \code{rigger()} returns a list containing the arguments, with
the class "rigger" to enable \code{+.rigger} and \code{print.rigger}
}
\description{
\itemize{
\item \code{boom()} prints the intermediate results of a call or a code chunk.
\item \code{rig()} creates a copy of a function which will display the intermediate
results of all the calls of it body.
\item \code{rig_in_namespace()} rigs a namespaced function in place, so its always
verbose even when called by other existing functions. It is especially handy
for package development.
\item \code{rigger()} provides a convenient way to rig an
anonymous function by using the \code{rigger(...) + function(...) {...}} syntax.
}
}
\examples{
# explode a simple call
boom(subset(head(mtcars, 2), qsec > 17))

# clock calls and customize how to print output
boom(subset(head(mtcars, 2), qsec > 17), clock = TRUE, print = str)

# print str only for data frames
boom(subset(head(mtcars, 2), qsec > 17), print = list(data.frame = str))

# rig an existing function
rig(ave)(warpbreaks$breaks, warpbreaks$wool)

# rig an anonymous function
fun1 <- rigger() + function(x) x + 1 + 2 # same as rig(function(x) x + 1 + 2))
fun1(1)
fun2 <- rigger(TRUE, typeof) + function(x) x + 1 + 2
fun2(1)
}

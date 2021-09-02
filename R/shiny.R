#' Debug a shiny app using boomer
#'
#' `rig_shiny()` should be called after your `library(shiny)` call,
#' it will change your global environment so you might want to refresh your session
#' after using it. Note that `renderPrint()` captures the printed output and in
#' doing so diverts boomer's ouput from the console to the app. We might find a
#' way to solve it at some point.
#'
#' @param funs The reactive functions to be affected, by default `reactive()` and
#'  all `render*()` functions from the *{shiny}* package.
#'
#' @param select Whether to debug all reacives (`TRUE` by default)
#'
#' @export
rig_shiny <- function(funs = c(
  "reactive", "renderCachedPlot", "renderDataTable", "renderImage", "renderPlot",
  "renderPrint", "renderTable", "renderText", "renderUI"), select = TRUE) {

  if(!requireNamespace("shiny", quietly =  TRUE)) {
    stop("You must install the {shiny} package to use `boomer::rig_shiny()`")
  }
  # backups
  globals$shiny_backups <- list()
  globals$shiny_backups$shinyApp <- shiny::shinyApp
  # globals$shiny_backups[funs] <- mget(funs, asNamespace("shiny"))

  boomer_shims <- sapply(funs, rig_shiny_fun)
  attach(boomer_shims)
  # for (fun_nm in funs)
  #   ge <- .GlobalEnv
  #   assign(fun_nm, rig_shiny_fun(fun_nm), ge)
  # }
  #
  #   reactives <- extract_shiny_reactives(funs)
  #   selected <- if(select) reactives
  #
  #   new_shinyApp <- function(ui, server, ...) {
  #     ui <- shiny::fluidPage(
  #       shiny::tabsetPanel(
  #         shiny::tabPanel("app", ui),
  #         shiny::tabPanel("boomer log options", shiny::checkboxGroupInput("boomer_checkboxes", "reactives", reactives, selected)))
  #     )
  #     on.exit({
  #       globals <- getFromNamespace("globals", "boomer")
  #       assign("shinyApp", globals$shiny_backups$shinyApp, asNamespace("shiny"))
  #       assign("shinyApp", globals$shiny_backups$shinyApp, as.environment("package:shiny"))
  #       # for(fun_nm in funs) {
  #       #   assign(fun_nm, boomer:::globals$shiny_backups[[fun_nm]], asNamespace("shiny"))
  #       #   assign(fun_nm, boomer:::globals$shiny_backups[[fun_nm]], as.environment("package:shiny"))
  #       # }
  #     })
  #     globals$shiny_backups$shinyApp(ui, server, ...)
  #   }
  #
  #
  #   ub <- unlockBinding
  #   ub("shinyApp", asNamespace("shiny"))
  #   ub("shinyApp", as.environment("package:shiny"))
  #   assign("shinyApp", new_shinyApp, asNamespace("shiny"))
  #   assign("shinyApp", new_shinyApp, as.environment("package:shiny"))
  #   # for(fun_nm in funs) {
  #   #   print(fun_nm)
  #   #   unlockBinding(fun_nm, asNamespace("shiny"))
  #   #   unlockBinding(fun_nm, as.environment("package:shiny"))
  #   #   rigged_fun <- rig_shiny_fun(fun_nm)
  #   #   assign(fun_nm, rigged_fun, asNamespace("shiny"))
  #   #   assign(fun_nm, rigged_fun, asNamespace("shiny"))
  #   # }

  trace_shinyApp()
}


#' This should be run on the existing ui if we use runApp rather than shinyApp
#' @export
rig_shiny_ui <- function(ui, funs = c(
  "reactive", "renderCachedPlot", "renderDataTable", "renderImage", "renderPlot",
  "renderPrint", "renderTable", "renderText", "renderUI"), select = TRUE) {

  if(!requireNamespace("shiny", quietly =  TRUE)) {
    stop("You must install the {shiny} package to use `boomer::rig_shiny_ui()`")
  }

  reactives <- extract_shiny_reactives(funs)
  selected <- if(select) reactives

  shiny::fluidPage(
    shiny::tabsetPanel(
      shiny::tabPanel("app", ui),
      shiny::tabPanel("boomer log options", shiny::checkboxGroupInput("boomer_checkboxes", "reactives", reactives, selected)))
  )
}

# returns a rigged shiny reactive function
# rig() would rig the function's body while `rig_shiny_fun` rigs the `expr` argument of
# render* functions or the x arg of `reactive`
rig_shiny_fun <- function(shiny_fun_nm) {
  # the shiny function, such as `reactive`, `renderPlot` etc
  shiny_fun <- getFromNamespace(shiny_fun_nm, "shiny")
  new_body <-  bquote({

    # if(!getOption("boomer.rig_shiny")) {
    #   sc <- sys.call()
    #   # replace function by original shiny function
    #   sc[[1]] <- quote(.(str2lang(paste0("shiny::", shiny_fun_nm))))
    #   return(eval.parent(sc))
    # }

    # explore the call stack to fetch the variable names and modules

    # NOTE : if this fun is called from inside of the original reactive fun
    # we need to go a bit further with the sys calls
    sc <- sys.call(-1)
    #sc <- sys.call(-4)
    module_server_fun_nm <- deparse1(sc[[1]])
    # this is a dirty fix to deal with the new recommended way of writing modules
    # it's brittle as it will break if shiny's code changes
    if(module_server_fun_nm == "module") {
      sc <- sys.call(-8)
      #sc <- sys.call(-11)
      module_server_fun_nm <- deparse1(sc[[1]])
      if (module_server_fun_nm == "..stacktraceon..") {
        sc <- sys.call(-6)
        #sc <- sys.call(-9)
        module_server_fun_nm <- deparse1(sc[[2]])
      }
    }
    sc <- sys.call()
    #sc <- sys.call(-3)
    # sc prints as `{var_nm} <- {shiny_fun_nm}(...)` but is not really this
    # expression, so we need to cheat to build it as an expression
    call <- parse(text=capture.output(sc))[[1]]
    # the var name is the lhs, which is the 2nd term
    var_nm <- deparse1(call[[2]])
    rigged_nm <- paste0(module_server_fun_nm, "/", var_nm)

    # use match.call so call is put together in canonical form with expr as first arg
    sc <- match.call(eval(sc[[1]]), sc)
    # replace function by original shiny function
    sc[[1]] <- quote(.(str2lang(paste0("shiny::", shiny_fun_nm))))
    # edit 2nd arg so it's rigged
    fun <- as.function(list(sc[[2]]), envir = parent.frame())
    sc[[2]] <- substitute({
      # here we must (?) do horrible things to get the "boomer_checkboxes",
      # ignoring namespaces
      selected_boomer_checkboxes <-
        .subset2(input, "impl")$.values$get("boomer_checkboxes")
      if(RIGGED_NM_1 %in% selected_boomer_checkboxes) {
        rig_impl <- getFromNamespace("rig_impl", "boomer")
        rig_impl(FUN, rigged_nm = RIGGED_NM_2)()
      } else {
        SC_2
      }
    }, list(
      FUN = fun,
      RIGGED_NM_1 = rigged_nm,
      RIGGED_NM_2 = paste(rigged_nm, .(sprintf("<- %s(...)", shiny_fun_nm))), SC_2 = sc[[2]]))
    eval.parent(sc)
  })
  as.function(c(formals(shiny_fun), new_body))
  # body(shiny_fun) <- new_body
  # shiny_fun
}

extract_shiny_reactives <- function(funs = c(
  "reactive", "renderCachedPlot", "renderDataTable", "renderImage", "renderPlot",
  "renderPrint", "renderTable", "renderText", "renderUI")) {

  rec_react <- function(code) {
    # message("rec_react")
    # print(code)
    if(!is.call(code)) return(invisible(NULL))
    if(identical(code[[1]], quote(`<-`)) &&
       is.call(code[[3]]) &&
       deparse1(code[[c(3, 1)]]) %in% funs) {
      return(deparse1(code[[2]]))
    }

    unlist(lapply(code, rec_react))
  }

  rec_mods <- function(code) {
    if(!is.call(code)) return(invisible(NULL))

    if(identical(code[[1]], quote(`<-`)) &&
       is.call(code[[3]]) &&
       identical(code[[c(3, 1)]], quote(`function`))) {
      # extract reactive calls
      mod_nm <- deparse1(code[[2]])
      fun_code <- code[[3]]
      body <- fun_code[[3]]

      react_nms <- rec_react(body)
      full_nms <- if(length(react_nms)) paste0(mod_nm, "/", react_nms)

      return(full_nms)
    }
    unlist(lapply(code[-1], rec_mods))
  }

  files <- list.files(full.names = TRUE, pattern = "\\.r|R$")

  unlist(lapply(files, function(x) {
    x <- parse(x)
    x <- as.call(c(quote(`{`), x))
    rec_mods(x)
  }))
}

# to do :
# * on shiny's load, populate `globals$rigged_funs`
# * Have `rig_shiny` and `unrig_shiny` toggle `globals$rig_shiny`
# * Have the log tab in the ui split by function type
# * .doTrace cannot return (due to its code using eval.parent(substitute(...)), so we need to implement our own trace analog,
#   Maybe (?) it would be nice if it could return a trace function, printing as a traced function and that we can disable with `untrace()`

# trace_shiny_funs <- function() {
#   for(fun in c(
#     "reactive", "renderCachedPlot", "renderDataTable", "renderImage", "renderPlot",
#     "renderPrint", "renderTable", "renderText", "renderUI", "shinyApp", "runApp")) {
#     tracer <- quote({
# if(getOption("boomer.shiny_rig")) {
#   rigged_fun <- getFromNamespace("globals", "boomer")$rigged_funs[[fun]]
#   call <- sys.call()
#   call[[1]] <- rigged_fun
#   return(eval.parent(call))
# }
#     })
#     suppressMessages(eval(bquote(trace(.(as.name(fun)), tracer, print = FALSE))))
#   }
# }

# trace function in attached package
trace2 <- function(fun_nm, tracer, pkg = "shiny") {
  ns  <- asNamespace(pkg)
  sp_env <- as.environment(paste0("package:", pkg))
  ub <- unlockBinding
  ub(fun_nm, ns)
  ub(fun_nm, sp_env)
  fun <- ns[[fun_nm]]
  # prepend body of copy of function
  body(fun) <- bquote({..(list(tracer, body(fun)))}, splice = TRUE)

  assign(fun_nm, fun, ns)
  assign(fun_nm, fun, sp_env)
  lb <- lockBinding
  lb(fun_nm, ns)
  lb(fun_nm, sp_env)
}

trace_shiny_reactive_fun <- function(fun_nm) {
  tracer <- bquote(
    if(getOption("boomer.rig_shiny")) {
      boomer_ns <- asNamespace("boomer")
      new_fun <- boomer_ns[["globals"]]$shiny_rigged_copies[[.(fun_nm)]]
      sc <- sys.call()
      sc[[1]] <- new_fun
      res <- eval(sc, parent.frame())
      return(res)
    }
  )
  trace2(fun_nm, tracer)
}


trace_shinyApp <- function(funs = c(
  "reactive", "renderCachedPlot", "renderDataTable", "renderImage", "renderPlot",
  "renderPrint", "renderTable", "renderText", "renderUI"), select = TRUE) {

  reactives <- extract_shiny_reactives(funs)
  selected <- if(select) reactives

  tracer <- bquote(
    if(getOption("boomer.rig_shiny")) {
      # nest app in "app" tabl and add "boomer log options" tab
      ui <- shiny::fluidPage(
        shiny::tabsetPanel(
          shiny::tabPanel("app", ui),
          shiny::tabPanel("boomer log options", shiny::checkboxGroupInput(
            "boomer_checkboxes", "reactives", .(reactives), .(selected)))))
    }
  )
  trace2("shinyApp", tracer)
}


# ça ne peut pas marcher avec des traces car `reactive` est appelé dans le code lui
# même d'ou des pb de recursion, on va devoir utiliser attach


prepend_fun <- function(fun_nm, tracer, pkg = "shiny") {
  fun <- getFromNamespace(fun_nm, pkg)
  # prepend body of copy of function
  body(fun) <- bquote({..(list(tracer, body(fun)))}, splice = TRUE)
  fun
}

attach_shiny_shims <- function(select = TRUE) {
  fun_nms <- c(
    "reactive", "renderCachedPlot", "renderDataTable", "renderImage", "renderPlot",
    "renderPrint", "renderTable", "renderText", "renderUI")
  boomer_shiny_shim <- list()
  for (fun_nm in fun_nms) {
    tracer <- bquote(
      if(getOption("boomer.rig_shiny")) {
        boomer_ns <- asNamespace("boomer")
        new_fun <- boomer_ns[["rig_shiny_fun"]](.(fun_nm))
        sc <- sys.call()
        sc[[1]] <- new_fun
        res <- eval(sc, parent.frame())
        return(res)
      }
    )
    boomer_shiny_shim[[fun_nm]] <- prepend_fun(fun_nm, tracer)
  }
  reactives <- extract_shiny_reactives(fun_nms)
  selected <- if(select) reactives

  tracer <- bquote(
    if(getOption("boomer.rig_shiny")) {
      # nest app in "app" tabl and add "boomer log options" tab
      ui <- shiny::fluidPage(
        shiny::tabsetPanel(
          shiny::tabPanel("app", ui),
          shiny::tabPanel("boomer log options", shiny::checkboxGroupInput(
            "boomer_checkboxes", "reactives", .(reactives), .(selected)))))
    }
  )
  boomer_shiny_shim[["shinyApp"]] <- prepend_fun("shinyApp", tracer)
  attach(boomer_shiny_shim)
}


# we must first have modified ("rigged") copies of shiny functions stored in globals
# at load time of shiny

# then we must "trace" shiny's functions in the namespace, so they fire the
# modified copies if the "rig_shiny" option is TRUE



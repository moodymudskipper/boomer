reactive_funs <- c(
  "reactive", "renderCachedPlot", "renderDataTable", "renderImage", "renderPlot",
  "renderPrint", "renderTable", "renderText", "renderUI")

ub <- unlockBinding
lb <- lockBinding

#' Debug a shiny app using boomer
#'
#' `rig_shiny()` modifies the way shiny works so that a debugging tab will be
#'  added to the shiny app and selected reactive functions will log their
#'  refreshed boomed ouput to the console. `unrig_shiny()` undoes his action.
#'
#' @export
rig_shiny <- function() {
  unrig_shiny()
  globals$shinyApp_bkp <- shiny::shinyApp

  if(!requireNamespace("shiny", quietly =  TRUE)) {
    stop("You must install the {shiny} package to use `boomer::rig_shiny()`")
  }

  boomer_shims <- sapply(reactive_funs, rig_shiny_fun)
  attach(boomer_shims)
  trace_shinyApp()
  globals$shiny_rigged <- TRUE
  invisible(NULL)
}

#' @rdname rig_shiny
#' @export
unrig_shiny <- function() {
  if(globals$shiny_rigged) {
    detach("boomer_shims")
    ns  <- asNamespace("shiny")
    sp_env <- as.environment("package:shiny")
    fun_nm <- "shinyApp"
    ub(fun_nm, ns)
    ub(fun_nm, sp_env)
    assign("shinyApp", globals$shinyApp_bkp, ns)
    assign("shinyApp", globals$shinyApp_bkp, sp_env)
    lb(fun_nm, ns)
    lb(fun_nm, sp_env)
    globals$shiny_rigged <- FALSE
  }
}




# returns a rigged shiny reactive function
# rig() would rig the function's body while `rig_shiny_fun` rigs the `expr` argument of
# render* functions or the x arg of `reactive`
rig_shiny_fun <- function(shiny_fun_nm) {
  # the shiny function, such as `reactive`, `renderPlot` etc
  shiny_fun <- getFromNamespace(shiny_fun_nm, "shiny")
  new_body <-  bquote({
    # explore the call stack to fetch the variable names and modules
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
}

extract_shiny_reactives <- function() {

  rec_react <- function(code) {
    # message("rec_react")
    # print(code)
    if(!is.call(code)) return(invisible(NULL))
    if(identical(code[[1]], quote(`<-`)) &&
       is.call(code[[3]]) &&
       deparse1(code[[c(3, 1)]]) %in% reactive_funs) {
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

# trace function in attached package
trace2 <- function(fun_nm, tracer, pkg = "shiny") {
  ns  <- asNamespace(pkg)
  sp_env <- as.environment(paste0("package:", pkg))
  ub(fun_nm, ns)
  ub(fun_nm, sp_env)
  fun <- ns[[fun_nm]]
  # prepend body of copy of function
  body(fun) <- bquote({..(list(tracer, body(fun)))}, splice = TRUE)
  assign(fun_nm, fun, ns)
  assign(fun_nm, fun, sp_env)
  lb(fun_nm, ns)
  lb(fun_nm, sp_env)
}


trace_shinyApp <- function(select = TRUE) {
  tracer <- bquote({
      reactives <- getFromNamespace("extract_shiny_reactives", "boomer")()
      select    <- .(select)
      selected  <- if(select) reactives
      # nest app in "app" tabl and add "boomer log options" tab
      ui <- shiny::fluidPage(
        shiny::tabsetPanel(
          shiny::tabPanel("app", ui),
          shiny::tabPanel("boomer log options", shiny::checkboxGroupInput(
            "boomer_checkboxes", "reactives", reactives, NULL))))
  })
  trace2("shinyApp", tracer)
}

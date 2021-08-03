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

  for (fun_nm in funs) {
    ge <- .GlobalEnv
    assign(fun_nm, rig_shiny_fun(fun_nm), ge)
  }

  reactives <- extract_shiny_reactives(funs)
  selected <- if(select) reactives

  new_shinyApp <- function(ui, server, ...) {
    ui <- shiny::fluidPage(
      shiny::tabsetPanel(
        shiny::tabPanel("app", ui),
        shiny::tabPanel("boomer log options", shiny::checkboxGroupInput("boomer_checkboxes", "reactives", reactives, selected)))
    )
    on.exit({
      globals <- getFromNamespace("globals", "boomer")
      assign("shinyApp", globals$shiny_backups$shinyApp, asNamespace("shiny"))
      assign("shinyApp", globals$shiny_backups$shinyApp, as.environment("package:shiny"))
      # for(fun_nm in funs) {
      #   assign(fun_nm, boomer:::globals$shiny_backups[[fun_nm]], asNamespace("shiny"))
      #   assign(fun_nm, boomer:::globals$shiny_backups[[fun_nm]], as.environment("package:shiny"))
      # }
    })
    globals$shiny_backups$shinyApp(ui, server, ...)
  }
  ub <- unlockBinding
  ub("shinyApp", asNamespace("shiny"))
  ub("shinyApp", as.environment("package:shiny"))
  assign("shinyApp", new_shinyApp, asNamespace("shiny"))
  assign("shinyApp", new_shinyApp, as.environment("package:shiny"))
  # for(fun_nm in funs) {
  #   print(fun_nm)
  #   unlockBinding(fun_nm, asNamespace("shiny"))
  #   unlockBinding(fun_nm, as.environment("package:shiny"))
  #   rigged_fun <- rig_shiny_fun(fun_nm)
  #   assign(fun_nm, rigged_fun, asNamespace("shiny"))
  #   assign(fun_nm, rigged_fun, asNamespace("shiny"))
  # }
}

rig_shiny_fun <- function(shiny_fun_nm) {
  # the shiny function, such as `reactive`, `renderPlot` etc
  shiny_fun <- get(shiny_fun_nm, mode = "function")
  as.function(c(formals(shiny_fun), bquote({

    # explore the call stack to fetch the variable names and modules

    sc <- sys.call(-1)
    fun_nm <- deparse1(sc[[1]])
    # this is a dirty fix to deal with the new recommended way of writing modules
    # it's brittle as it will break if shiny's code changes
    if(fun_nm == "module") {
      sc <- sys.call(-8)
      fun_nm <- deparse1(sc[[1]])
      if (fun_nm == "..stacktraceon..") {
        sc <- sys.call(-6)
        fun_nm <- deparse1(sc[[2]])
      }
    }
    sc <- sys.call()
    call <- parse(text=capture.output(sc))[[1]]
    var_nm <- deparse1(call[[2]])
    rigged_nm <- paste0(fun_nm, "/", var_nm)

    # use match.call so call is put together in canonical form with expr as first arg
    sc <- match.call(eval(sc[[1]]), sc)
    # replace function by original shiny function
    sc[[1]] <- str2lang(paste0("shiny::", shiny_fun_nm))
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
      RIGGED_NM_2 = sprintf("%s <- %s(...)", rigged_nm, shiny_fun_nm), SC_2 = sc[[2]]))
    eval.parent(sc)
  })))
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
      # extrac reactive calls
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

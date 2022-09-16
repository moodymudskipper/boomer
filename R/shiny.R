reactive_funs <- c(
  "reactive", "renderCachedPlot", "renderDataTable", "renderImage", "renderPlot",
  "renderPrint", "renderTable", "renderText", "renderUI")

# returns a rigged shiny reactive function
# rig() would rig the function's body while `rig_shiny_fun` rigs the `expr` argument of
# render* functions or the x arg of `reactive`
rig_shiny_fun <- function(shiny_fun_nm) {
  # the shiny function, such as `reactive`, `renderPlot` etc
  shiny_fun <- getFromNamespace(shiny_fun_nm, "shiny")
  new_body <-  bquote({
    scs <- sys.calls()

    for (i in rev(seq_along(scs))) {
      call <- scs[[i]]
      found_callModule <- "callModule" %in% all.names(call)
      if(found_callModule) {
        called_by_moduleServed <- deparse1(scs[[i-1]][[1]]) == "moduleServer"
        if(called_by_moduleServed) {
          module_server_fun_nm <- deparse1(scs[[i-2]][[1]])
        } else {
          module_server_fun_nm <- deparse1(call[[2]])
        }
        break
      }
    }
    if(!found_callModule) {
      module_server_fun_nm <- deparse1(scs[[length(scs)-1]][[1]])
    }

    sc <- sys.call()
    # sc prints as `{var_nm} <- {shiny_fun_nm}(...)` but is not really this
    # expression, so we need to cheat to build it as an expression
    call <- parse(text=capture.output(sc))[[1]]
    if(identical(call[[1]], quote(`<-`))) {
      # the var name is the lhs, which is the 2nd term
      var_nm <- deparse1(call[[2]])
      reactive_fun_nm <- deparse1(call[[c(3, 1)]])
    } else {
      call <- sc # to avoid corner cases like `return(reactive(...))`
      var_nm <- "<return value>"
      reactive_fun_nm <- deparse1(call[[1]])
    }
    rigged_nm <- paste0(module_server_fun_nm, "/", var_nm, " <- ", reactive_fun_nm, "(...)")

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
      RIGGED_NM_2 = rigged_nm, #paste(rigged_nm, .(sprintf("<- %s(...)", shiny_fun_nm))),
      SC_2 = sc[[2]]))
    eval.parent(sc)
  })
  as.function(c(formals(shiny_fun), new_body))
}

extract_shiny_reactives <- function() {

  rec_react <- function(code) {
    # is applied on the body of a funcion to fetch reactive calls
    # message("rec_react")
    # print(code)
    if(!is.call(code)) return(invisible(NULL))
    code_is_assigning_a_reactive <-
      identical(code[[1]], quote(`<-`)) &&
      is.call(code[[3]]) &&
      deparse1(code[[c(3, 1)]]) %in% reactive_funs
    code_is_a_call_to_a_reactive <-
      deparse1(code[[1]]) %in% reactive_funs
    if(code_is_assigning_a_reactive) {
      res <- paste0(deparse1(code[[2]]), " <- ", deparse1(code[[c(3, 1)]]), "(...)")
      return(res)
    } else if (code_is_a_call_to_a_reactive) {
      # we're not testing that it's the last call but it wouldn't make sense otherwise
      # and is not trivial to test
      res <- paste0("<return value> <- ", deparse1(code[[1]]), "(...)")
      return(res)
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

  files <- list.files(full.names = TRUE, pattern = "\\.r|R$", recursive = TRUE)

  unlist(lapply(files, function(x) {
    x <- parse(x)
    x <- as.call(c(quote(`{`), x))
    rec_mods(x)
  }))
}



#' boom the reactive calls of a shiny app
#'
#' These works just like `shiny::shinyApp` and `shiny::runApp` and have the exact same parameters,
#' except they create/run a modified app that allows for easier debugging.
#'
#' For this function to work properly the main server function should always be
#'  assigned to an object (usually you'd name it `server`).
#'
#' For instance :
#' * if you have a `server.R` script, make sure to assign your function to `server`
#' * if you use `shinyServer`, create a `server` function separately and use it
#' in your `shinyServer` call.
#'
#' It also assumes you follow standard practice in your use of `callModule()` or
#' `moduleServer()`.
#'
#' @inheritParams shiny::shinyApp
#' @inheritParams shiny::runApp
#' @export
#' @return See `?shiny::shinyApp` and `?shiny::runApp`
boom_shinyApp <- function (
  ui,
  server,
  onStart = NULL,
  options = list(),
  uiPattern = "/",
  enableBookmarking = NULL) {
  if(!requireNamespace("shiny", quietly = TRUE)) {
    stop("`boomApp` requires the 'shiny' package to be installed")
  }
  reactives <- getFromNamespace("extract_shiny_reactives", "boomer")()
  ui <- shiny::fluidPage(
    shiny::tabsetPanel(
      shiny::tabPanel("app", ui),
      shiny::tabPanel("boomer log options", shiny::checkboxGroupInput(
        "boomer_checkboxes", "reactives", reactives, NULL))))

  res <- shiny::shinyApp(
    ui,
    server,
    onStart,
    options,
    uiPattern,
    enableBookmarking)

  class(res) <- c("shiny.boomed_appobj", class(res))
  res
}

#' @export
print.shiny.boomed_appobj <- function(x, ...) {
  opts <- if (is.null(x$options) || isTRUE(is.na(x$options))) {
    list()
  } else {
    x$options
  }
  opts <- opts[names(opts) %in% c("port", "launch.browser",
                                  "host", "quiet", "display.mode", "test.mode")]
  args <- c(list(quote(x)), opts)

  boomer_shims <- sapply(reactive_funs, rig_shiny_fun)
  # to avoid note, attaching is OK since we detach right away on exit
  attach_ <- attach
  suppressMessages(attach_(boomer_shims))
  on.exit({
    detach(boomer_shims)
  })
  do.call(shiny::runApp, args)
}


#' @rdname boom_shinyApp
#' @export
boom_runApp <- function (
  appDir = getwd(),
  port = getOption("shiny.port"),
  launch.browser = getOption("shiny.launch.browser", interactive()),
  host = getOption("shiny.host",  "127.0.0.1"),
  workerId = "",
  quiet = FALSE,
  display.mode = c("auto", "normal", "showcase"),
  test.mode = getOption("shiny.testmode", FALSE)) {
  if(!requireNamespace("shiny", quietly = TRUE)) {
    stop("`boomApp` requires the 'shiny' package to be installed")
  }

  # to avoid note, library is OK since runApp attaches shiny anyway,
  # but we need to attach it first to make sure ou shims are visible
  library_ <- library
  library_("shiny")
  ns <- asNamespace("shiny")

  with(ns, suppressMessages(trace(
    print = FALSE,
    what = uiHttpHandler,
    tracer = bquote({
      reactives <- getFromNamespace("extract_shiny_reactives", "boomer")()
      ui <- shiny::fluidPage(
        shiny::tabsetPanel(
          shiny::tabPanel("app", ui),
          shiny::tabPanel("boomer log options", shiny::checkboxGroupInput(
            "boomer_checkboxes", "reactives", reactives, NULL))))
    }))))

  sc <- sys.call()
  sc[[1]] <- quote(shiny::runApp)
  boomer_shims <- sapply(reactive_funs, rig_shiny_fun)
  # to avoid note, attaching is OK since we detach right away on exit
  attach_ <- attach
  suppressMessages(attach_(boomer_shims))
  on.exit({
    with(ns, suppressMessages(untrace(uiHttpHandler)))
    detach(boomer_shims)
  })
  eval.parent(sc)
}


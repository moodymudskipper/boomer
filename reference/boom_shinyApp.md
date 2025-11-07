# boom the reactive calls of a shiny app

These works just like
[`shiny::shinyApp`](https://rdrr.io/pkg/shiny/man/shinyApp.html) and
[`shiny::runApp`](https://rdrr.io/pkg/shiny/man/runApp.html) and have
the exact same parameters, except they create/run a modified app that
allows for easier debugging.

## Usage

``` r
boom_shinyApp(
  ui,
  server,
  onStart = NULL,
  options = list(),
  uiPattern = "/",
  enableBookmarking = NULL
)

boom_runApp(
  appDir = getwd(),
  port = getOption("shiny.port"),
  launch.browser = getOption("shiny.launch.browser", interactive()),
  host = getOption("shiny.host", "127.0.0.1"),
  workerId = "",
  quiet = FALSE,
  display.mode = c("auto", "normal", "showcase"),
  test.mode = getOption("shiny.testmode", FALSE)
)
```

## Arguments

- ui:

  The UI definition of the app (for example, a call to `fluidPage()`
  with nested controls).

  If bookmarking is enabled (see `enableBookmarking`), this must be a
  single argument function that returns the UI definition.

- server:

  A function with three parameters: `input`, `output`, and `session`.
  The function is called once for each session ensuring that each app is
  independent.

- onStart:

  A function that will be called before the app is actually run. This is
  only needed for `shinyAppObj`, since in the `shinyAppDir` case, a
  `global.R` file can be used for this purpose.

- options:

  Named options that should be passed to the `runApp` call (these can be
  any of the following: "port", "launch.browser", "host", "quiet",
  "display.mode" and "test.mode"). You can also specify `width` and
  `height` parameters which provide a hint to the embedding environment
  about the ideal height/width for the app.

- uiPattern:

  A regular expression that will be applied to each `GET` request to
  determine whether the `ui` should be used to handle the request. Note
  that the entire request path must match the regular expression in
  order for the match to be considered successful.

- enableBookmarking:

  Can be one of `"url"`, `"server"`, or `"disable"`. The default value,
  `NULL`, will respect the setting from any previous calls to
  [`enableBookmarking()`](https://rdrr.io/pkg/shiny/man/enableBookmarking.html).
  See
  [`enableBookmarking()`](https://rdrr.io/pkg/shiny/man/enableBookmarking.html)
  for more information on bookmarking your app.

- appDir:

  Path to directory that contains a Shiny app (i.e. a server.R file and
  either ui.R or www/index.html)

- port:

  The TCP port that the application should listen on. If the `port` is
  not specified, and the `shiny.port` option is set (with
  `options(shiny.port = XX)`), then that port will be used. Otherwise,
  use a random port between 3000:8000, excluding ports that are blocked
  by Google Chrome for being considered unsafe: 3659, 4045, 5060, 5061,
  6000, 6566, 6665:6669 and 6697. Up to twenty random ports will be
  tried.

- launch.browser:

  If true, the system's default web browser will be launched
  automatically after the app is started. Defaults to true in
  interactive sessions only. The value of this parameter can also be a
  function to call with the application's URL.

- host:

  The IPv4 address that the application should listen on. Defaults to
  the `shiny.host` option, if set, or `"127.0.0.1"` if not. See Details.

- workerId:

  Can generally be ignored. Exists to help some editions of Shiny Server
  Pro route requests to the correct process.

- quiet:

  Should Shiny status messages be shown? Defaults to FALSE.

- display.mode:

  The mode in which to display the application. If set to the value
  `"showcase"`, shows application code and metadata from a `DESCRIPTION`
  file in the application directory alongside the application. If set to
  `"normal"`, displays the application normally. Defaults to `"auto"`,
  which displays the application in the mode given in its `DESCRIPTION`
  file, if any.

- test.mode:

  Should the application be launched in test mode? This is only used for
  recording or running automated tests. Defaults to the `shiny.testmode`
  option, or FALSE if the option is not set.

## Value

See [`?shiny::shinyApp`](https://rdrr.io/pkg/shiny/man/shinyApp.html)
and [`?shiny::runApp`](https://rdrr.io/pkg/shiny/man/runApp.html)

## Details

For this function to work properly the main server function should
always be assigned to an object (usually you'd name it `server`).

For instance :

- if you have a `server.R` script, make sure to assign your function to
  `server`

- if you use `shinyServer`, create a `server` function separately and
  use it in your `shinyServer` call.

It also assumes you follow standard practice in your use of
`callModule()` or `moduleServer()`.

#' MVU Module UI
#'
#' Creates the UI for an MVU component that can be embedded as a Shiny module.
#' Handles namespace prefixing so that multiple MVU components on the same
#' page do not collide on message channels.
#'
#' @param id The module ID string.
#' @param ... UI elements to include inside the Alpine.js `x-data` container.
#' @param component Character string naming the Alpine.js component. Defaults
#'   to `"mvu"`.
#' @param debug Logical. When `TRUE`, includes the built-in time-travel
#'   debugger overlay. The corresponding [mvu_module_server()] call must
#'   also set `debug = TRUE`.
#' @param theme A [bslib::bs_theme()] object for Bootstrap theming.
#' @param extra_js Optional JavaScript string for additional Alpine.js
#'   component properties.
#' @param extra_channels A named list of additional Shiny custom message
#'   handlers.
#'
#' @return A [htmltools::tagList()] containing the bridge script and Alpine.js
#'   container.
#'
#' @examples
#' \dontrun{
#' ui <- shiny::fluidPage(
#'   mvu_module_ui("counter",
#'     shiny::tags$h1("Counter Module")
#'   )
#' )
#' }
#'
#' @export
mvu_module_ui <- function(id, ..., component = "mvu", debug = FALSE,
                          theme = bslib::bs_theme(),
                          extra_js = NULL, extra_channels = NULL) {
  ns <- NS(id)
  comp_id <- ns(component)
  js_name <- gsub("-", "_", comp_id)
  tagList(
    tags$style(HTML("[x-cloak]{display:none!important}")),
    mvu_bridge_js(comp_id, extra_js = extra_js, extra_channels = extra_channels),
    tags$script(
      src = "https://cdn.jsdelivr.net/npm/alpinejs@3/dist/cdn.min.js",
      defer = NA
    ),
    div(`x-data` = js_name, `x-cloak` = NA, ...),
    if (debug) debugger_ui(ns("__dbg__"))
  )
}

#' MVU Module Server
#'
#' Server-side counterpart to [mvu_module_ui()]. Wraps [mvu_server()] inside
#' [shiny::moduleServer()] with correct namespace handling for message
#' channels.
#'
#' @param id The module ID string (must match the `id` in [mvu_module_ui()]).
#' @param init A function returning the initial model.
#' @param update A function with signature `function(model, msg, value)` that
#'   returns a new model or an [mvu_result()].
#' @param msg An optional enum factory created by [mvu_enum()]. See
#'   [mvu_server()] for details.
#' @param to_frontend A function with signature `function(model)` that
#'   projects the model into a JSON-serializable list for the client.
#'   Defaults to [identity()].
#' @param component Character string naming the Alpine.js component. Must
#'   match the `component` argument in [mvu_module_ui()].
#' @param on_msg `r lifecycle::badge("deprecated")` Optional message hook.
#'   See [mvu_server()] for details.
#' @param debug Logical. When `TRUE`, records transitions and starts the
#'   built-in time-travel debugger. The corresponding [mvu_module_ui()]
#'   call must also set `debug = TRUE`. See [mvu_server()] for details
#'   on the changed return type.
#'
#' @return See [mvu_server()] — the model `reactiveVal` (default) or a list
#'   with `$model` and `$log` when `debug = TRUE`.
#'
#' @examples
#' \dontrun{
#' server <- function(input, output, session) {
#'   mvu_module_server("counter",
#'     init = function() list(count = 0),
#'     update = function(model, msg, value) model
#'   )
#' }
#' }
#'
#' @export
mvu_module_server <- function(id, init, update, msg = NULL,
                              to_frontend = identity,
                              component = "mvu", on_msg = NULL,
                              debug = FALSE) {
  moduleServer(id, function(input, output, session) {
    comp_id <- session$ns(component)
    mvu_server(
      init = init, update = update, msg = msg,
      to_frontend = to_frontend,
      component = component, on_msg = on_msg,
      debug = debug,
      input = input, output = output, session = session,
      .channel_component = comp_id
    )
  })
}

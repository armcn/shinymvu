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
#'   mvu_module_ui(
#'     "counter",
#'     shiny::tags$h1("Counter Module")
#'   )
#' )
#' }
#'
#' @export
mvu_module_ui <- function(id, ..., component = "mvu",
                          extra_js = NULL, extra_channels = NULL) {
  ns <- NS(id)
  comp_id <- ns(component)
  js_name <- gsub("-", "_", comp_id)
  tagList(
    shinymvu_dep(),
    alpine_dep(),
    mvu_bridge_js(comp_id, extra_js = extra_js, extra_channels = extra_channels),
    div(`x-data` = js_name, `x-cloak` = NA, ...)
  )
}

#' MVU Module Server
#'
#' Server-side counterpart to [mvu_module_ui()]. Runs the Model-View-Update
#' reactive loop inside [shiny::moduleServer()].
#'
#' @param id The module ID string (must match the `id` in [mvu_module_ui()]).
#' @param init A function returning the initial model (a list).
#' @param update A function with signature `function(model, msg, value)` that
#'   takes the current model, a message type (string or `mvu_enum`), and an
#'   optional value, and returns a new model or an [mvu_result()].
#' @param msg An optional enum factory created by [mvu_enum()]. When
#'   provided, incoming message type strings are automatically validated
#'   and converted to `mvu_enum` objects before being passed to `update`.
#' @param to_frontend A function with signature `function(model)` that
#'   projects the server-side model into a JSON-serializable list for the
#'   client. May include derived display values. Defaults to [identity()],
#'   which sends the model as-is.
#' @param component Character string naming the Alpine.js component. Must
#'   match the `component` argument in [mvu_module_ui()].
#' @param debug Logical. When `TRUE`, every transition is recorded and the
#'   built-in time-travel debugger overlay is injected into the page. The
#'   return value changes to a list with `$model` (reactiveVal), `$log`
#'   (reactiveVal of [mvu_log()]), `$travel_to` (function), `$resume`
#'   (function), and `$is_traveling` (reactiveVal).
#' @param subscriptions A function returning a named list of reactive
#'   expressions. Names are message types; when a reactive changes, the
#'   runtime dispatches the corresponding message through `update`. This
#'   is the Elm-style subscriptions pattern for connecting external event
#'   sources (other modules, timers, Shiny inputs) into the MVU loop.
#'
#' @return When `debug = FALSE` (default): the model `reactiveVal`.
#'   When `debug = TRUE`: a list with `$model`, `$log`, `$travel_to`,
#'   `$resume`, and `$is_traveling`.
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
                              component = "mvu",
                              debug = FALSE, subscriptions = NULL) {
  moduleServer(id, function(input, output, session) {
    comp_id <- session$ns(component)
    mvu_server(
      init = init, update = update, msg = msg,
      to_frontend = to_frontend,
      component = component,
      debug = debug, subscriptions = subscriptions,
      input = input, output = output, session = session,
      .channel_component = comp_id
    )
  })
}

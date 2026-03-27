#' MVU Server Runtime Loop
#'
#' The core reactive runtime that connects the Model-View-Update cycle.
#' Listens for messages from the Alpine.js client, passes them through the
#' user-defined `update` function, and pushes the resulting frontend model
#' back to the browser via `sendCustomMessage`.
#'
#' @param init A function returning the initial model (a list).
#' @param update A function with signature `function(model, msg, value)` that
#'   takes the current model, a message type (string or `mvu_enum`), and an
#'   optional value, and returns a new model. Must be a pure function.
#' @param msg An optional enum factory created by [mvu_enum()]. When
#'   provided, incoming message type strings are automatically validated
#'   and converted to `mvu_enum` objects before being passed to `update`.
#' @param to_frontend A function with signature `function(model)` that
#'   projects the server-side model into a JSON-serializable list for the
#'   client. May include derived display values. Defaults to [identity()],
#'   which sends the model as-is.
#' @param component Character string naming the Alpine.js component. Must
#'   match the `component` argument used in [mvu_page()] or
#'   [mvu_module_ui()].
#' @param on_msg An optional callback with signature
#'   `function(model, type, value, session)` called before `update`. Receives
#'   the `reactiveVal` itself (not its value) for direct read/write. Return
#'   `FALSE` to skip the default `update` call; any other return value
#'   (including `NULL`) proceeds normally. Useful for side-effect-only
#'   messages.
#' @param input,output,session The Shiny session objects, passed from the
#'   server function.
#' @param .channel_component Internal parameter used by [mvu_module_server()]
#'   to set the correct channel name for `sendCustomMessage`. Do not use
#'   directly.
#'
#' @return The model `reactiveVal`, allowing further reactive integration.
#'
#' @examples
#' \dontrun{
#' server <- function(input, output, session) {
#'   mvu_server(
#'     init = function() list(count = 0),
#'     update = function(model, msg, value) model,
#'     input = input, output = output, session = session
#'   )
#' }
#' }
#'
#' @export
mvu_server <- function(init, update, msg = NULL, to_frontend = identity,
                       component = "mvu", on_msg = NULL, input, output, session,
                       .channel_component = component) {
  model_channel <- paste0(.channel_component, "__model")
  msg_input <- paste0(component, "__msg")

  model <- reactiveVal(init())

  push <- function() {
    session$sendCustomMessage(model_channel, to_frontend(model()))
  }

  session$onFlushed(function() {
    session$sendCustomMessage(model_channel, to_frontend(isolate(model())))
  })

  observe({
    push()
  })

  msg_factory <- msg

  observeEvent(input[[msg_input]], {
    raw <- input[[msg_input]]
    type <- raw$type
    value <- raw$value

    if (!is.null(msg_factory)) {
      type <- msg_factory(type)
    }

    if (!is.null(on_msg)) {
      proceed <- on_msg(model, type, value, session)
      if (isFALSE(proceed)) return()
    }

    new_model <- update(model(), type, value)
    model(new_model)
  })

  model
}

#' MVU Server Runtime Loop
#'
#' The core reactive runtime that connects the Model-View-Update cycle.
#' Listens for messages from the Alpine.js client, passes them through the
#' user-defined `update` function, and pushes the resulting frontend model
#' back to the browser via `sendCustomMessage`.
#'
#' `update` may return either a plain model list (backward compatible) or
#' an [mvu_result()] containing both a model and effects. When effects are
#' present, the runtime executes them after updating the model.
#'
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
#'   match the `component` argument used in [mvu_page()] or
#'   [mvu_module_ui()].
#' @param on_msg `r lifecycle::badge("deprecated")` An optional callback
#'   called before `update`. Use [mvu_result()] with effect constructors
#'   instead. When provided, receives `(model, type, value, session)` where
#'   `model` is the `reactiveVal` itself. Return `FALSE` to skip `update`.
#' @param debug Logical. When `TRUE`, every transition is recorded, a
#'   built-in time-travel debugger overlay is shown, and the return value
#'   changes to a list with `$model` (reactiveVal), `$log` (reactiveVal
#'   of [mvu_log()]), `$travel_to` (function), `$resume` (function), and
#'   `$is_traveling` (reactiveVal). The corresponding UI function
#'   ([mvu_page()] or [mvu_module_ui()]) must also set `debug = TRUE`.
#' @param input,output,session The Shiny session objects, passed from the
#'   server function.
#' @param .channel_component Internal parameter used by [mvu_module_server()]
#'   to set the correct channel name for `sendCustomMessage`. Do not use
#'   directly.
#'
#' @section Time-travel debugging:
#' When `debug = TRUE`, the returned list includes:
#' \describe{
#'   \item{`travel_to(step)`}{Jump the UI to historical step `step`
#'     (0 = initial state, 1 = after first message, ...). While traveling,
#'     incoming messages are silently dropped.}
#'   \item{`resume()`}{Exit time-travel mode and restore the live model.}
#'   \item{`is_traveling`}{A `reactiveVal` that is `TRUE` while the
#'     runtime is in time-travel mode.}
#' }
#'
#' @return When `debug = FALSE` (default): the model `reactiveVal`.
#'   When `debug = TRUE`: a list (see Time-travel debugging section).
#'
#' @examples
#' \dontrun{
#' server <- function(input, output, session) {
#'   mvu_server(
#'     init = function() list(count = 0),
#'     update = function(model, msg, value) {
#'       switch(msg,
#'         increment = list_set(model, count = model$count + 1),
#'         save = mvu_result(model, effect_notify("Saved!")),
#'         model
#'       )
#'     },
#'     input = input, output = output, session = session
#'   )
#' }
#' }
#'
#' @export
mvu_server <- function(init, update, msg = NULL, to_frontend = identity,
                       component = "mvu", on_msg = NULL, debug = FALSE,
                       input, output, session,
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

  log_rv <- if (debug) reactiveVal(mvu_log()) else NULL
  is_traveling <- if (debug) reactiveVal(FALSE) else NULL
  seq_counter <- 0L
  cached_models <- NULL

  msg_factory <- msg

  observeEvent(input[[msg_input]], {
    if (debug && isTRUE(isolate(is_traveling()))) return()

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

    model_before <- if (debug) model() else NULL

    result <- update(model(), type, value)

    if (is_mvu_result(result)) {
      model(result$model)
      run_effects(result$effects, session)
      result_effects <- result$effects
    } else {
      model(result)
      result_effects <- list()
    }

    if (debug) {
      seq_counter <<- seq_counter + 1L
      trans <- mvu_transition(
        seq = seq_counter,
        type = type,
        value = value,
        model_before = model_before,
        model_after = model(),
        effects = result_effects
      )
      current_log <- isolate(log_rv())
      current_log$transitions <- c(current_log$transitions, list(trans))
      log_rv(mvu_log(current_log$transitions))
      cached_models <<- NULL
    }
  })

  if (debug) {
    travel_to_fn <- function(step) {
      log <- isolate(log_rv())
      n <- length(log$transitions)
      step <- max(0L, min(as.integer(step), n))

      if (is.null(cached_models) || length(cached_models) != n + 1L) {
        cached_models <<- mvu_replay(init, update, log)
      }

      is_traveling(TRUE)
      session$sendCustomMessage(
        model_channel, to_frontend(cached_models[[step + 1L]])
      )
    }

    resume_fn <- function() {
      is_traveling(FALSE)
      cached_models <<- NULL
      session$sendCustomMessage(
        model_channel, to_frontend(isolate(model()))
      )
    }

    model_at_fn <- function(step) {
      log <- isolate(log_rv())
      n <- length(log$transitions)
      step <- max(0L, min(as.integer(step), n))
      if (step == 0L) return(init())
      log$transitions[[step]]$model_after
    }

    import_fn <- function(messages) {
      is_traveling(FALSE)
      model(init())
      seq_counter <<- 0L
      new_transitions <- list()
      for (m in messages) {
        seq_counter <<- seq_counter + 1L
        model_before <- model()
        type <- m$type
        value <- m$value
        if (!is.null(msg_factory)) type <- msg_factory(type)
        result <- update(model(), type, value)
        if (is_mvu_result(result)) {
          model(result$model)
          effs <- result$effects
        } else {
          model(result)
          effs <- list()
        }
        new_transitions <- c(new_transitions, list(mvu_transition(
          seq = seq_counter, type = type, value = value,
          model_before = model_before, model_after = model(),
          effects = effs
        )))
      }
      log_rv(mvu_log(new_transitions))
      cached_models <<- NULL
    }

    runtime <- list(
      model = model,
      log = log_rv,
      travel_to = travel_to_fn,
      resume = resume_fn,
      is_traveling = is_traveling,
      model_at = model_at_fn,
      import_log = import_fn
    )

    debugger_server("__dbg__", runtime, init)

    runtime
  } else {
    model
  }
}

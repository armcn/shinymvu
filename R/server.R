#' MVU Server Runtime Loop
#'
#' The core reactive runtime that connects the Model-View-Update cycle.
#' Called internally by [mvu_module_server()].
#'
#' @param init A function returning the initial model (a list).
#' @param update A function with signature `function(model, msg, value)`.
#' @param msg An optional enum factory created by [mvu_enum()].
#' @param to_frontend A function projecting the model for the client.
#' @param component Alpine.js component name.
#' @param on_msg Deprecated message hook.
#' @param debug Logical. When `TRUE`, records transitions and injects the
#'   time-travel debugger.
#' @param input,output,session Shiny session objects.
#' @param .channel_component Internal channel name override.
#'
#' @return When `debug = FALSE`: the model `reactiveVal`.
#'   When `debug = TRUE`: a runtime list.
#'
#' @keywords internal
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

    shiny::insertUI(
      selector = "body",
      where = "beforeEnd",
      ui = debugger_ui(session$ns("__dbg__")),
      immediate = TRUE,
      session = session
    )
    debugger_server("__dbg__", runtime, init)

    runtime
  } else {
    model
  }
}

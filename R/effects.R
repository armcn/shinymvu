#' Return Model and Effects from Update
#'
#' Wraps a new model together with side effects that the runtime should
#' execute after the state transition. This is the Elm-style "command"
#' pattern: `update` stays pure by *describing* effects as data rather
#' than performing them.
#'
#' When `update` returns a plain list, the runtime treats it as a model
#' with no effects (backward compatible). When it returns an `mvu_result`,
#' the runtime updates the model and then runs the effects.
#'
#' @param model The new model (a list).
#' @param ... Zero or more effect objects created by [effect_notify()],
#'   [effect_send()], [effect_custom()], or grouped with [effects()].
#'
#' @return An `mvu_result` S3 object with `$model` and `$effects` fields.
#'
#' @examples
#' # Model-only (equivalent to returning the model directly)
#' mvu_result(list(count = 1))
#'
#' # Model with a notification
#' mvu_result(
#'   list(count = 1),
#'   effect_notify("Saved!", type = "message")
#' )
#'
#' # Model with multiple effects
#' mvu_result(
#'   list(count = 0),
#'   effect_notify("Reset complete"),
#'   effect_send("analytics", list(event = "reset"))
#' )
#'
#' @export
mvu_result <- function(model, ...) {
  raw <- list(...)
  flat <- list()
  for (e in raw) {
    if (inherits(e, "mvu_effects")) {
      flat <- c(flat, unclass(e))
    } else if (inherits(e, "mvu_effect")) {
      flat <- c(flat, list(e))
    } else if (!is.null(e)) {
      stop("mvu_result() expects effect objects, got: ", class(e)[[1]])
    }
  }
  structure(list(model = model, effects = flat), class = "mvu_result")
}

#' Test Whether an Object is an MVU Result
#'
#' @param x An object to test.
#' @return `TRUE` if `x` inherits from `"mvu_result"`.
#' @export
is_mvu_result <- function(x) inherits(x, "mvu_result")

#' @export
print.mvu_result <- function(x, ...) {
  n <- length(x$effects)
  cat(sprintf("<mvu_result>\n  effects: %d\n", n))
  for (eff in x$effects) {
    cat(sprintf("    - %s\n", format_effect(eff)))
  }
  invisible(x)
}

# --- Effect Constructors ----------------------------------------------------

#' Create a Notification Effect
#'
#' Describes a [shiny::showNotification()] call to be executed by the
#' runtime after the model update.
#'
#' @param text Notification text.
#' @param type One of `"default"`, `"message"`, `"warning"`, `"error"`.
#' @param duration Seconds to display. Use `NULL` for persistent.
#'
#' @return An `mvu_effect` object.
#'
#' @examples
#' effect_notify("File saved", type = "message", duration = 3)
#'
#' @export
effect_notify <- function(text, type = "default", duration = 5) {
  structure(
    list(kind = "notify", text = text, type = type, duration = duration),
    class = "mvu_effect"
  )
}

#' Create a Custom Message Effect
#'
#' Describes a [shiny::session]`$sendCustomMessage()` call. Use this to
#' send data to client-side JavaScript handlers registered via
#' `extra_channels` in [mvu_module_ui()].
#'
#' @param channel The custom message channel name.
#' @param data The data to send (will be serialized to JSON).
#'
#' @return An `mvu_effect` object.
#'
#' @examples
#' effect_send("clipboard", "text to copy")
#'
#' @export
effect_send <- function(channel, data) {
  structure(
    list(kind = "send", channel = channel, data = data),
    class = "mvu_effect"
  )
}

#' Create a Custom Side-Effect
#'
#' Escape hatch for effects not covered by the built-in constructors.
#' The function receives the Shiny `session` object and can perform
#' arbitrary operations.
#'
#' @param fn A function with signature `function(session)`.
#'
#' @return An `mvu_effect` object.
#'
#' @examples
#' effect_custom(function(session) {
#'   shiny::showModal(shiny::modalDialog("Hello"))
#' })
#'
#' @export
effect_custom <- function(fn) {
  if (!is.function(fn)) stop("effect_custom() requires a function")
  structure(list(kind = "custom", fn = fn), class = "mvu_effect")
}

#' Combine Multiple Effects
#'
#' Groups several effects into a single object that [mvu_result()] will
#' flatten. This is purely syntactic convenience.
#'
#' @param ... Effect objects.
#'
#' @return An `mvu_effects` list.
#'
#' @examples
#' combined <- effects(
#'   effect_notify("Done"),
#'   effect_send("log", list(event = "done"))
#' )
#' mvu_result(list(status = "done"), combined)
#'
#' @export
effects <- function(...) {
  structure(list(...), class = "mvu_effects")
}

#' @export
print.mvu_effect <- function(x, ...) {
  cat(sprintf("<mvu_effect: %s>\n", format_effect(x)))
  invisible(x)
}

# --- Internal ---------------------------------------------------------------

format_effect <- function(eff) {
  switch(eff$kind,
    "notify" = sprintf("notify(%s, type=%s)", eff$text, eff$type),
    "send"   = sprintf("send(%s)", eff$channel),
    "custom" = "custom(fn)",
    sprintf("unknown(%s)", eff$kind)
  )
}

run_effects <- function(effs, session) {
  for (eff in effs) {
    if (!inherits(eff, "mvu_effect")) next
    switch(eff$kind,
      "notify" = shiny::showNotification(
        eff$text, type = eff$type, duration = eff$duration
      ),
      "send" = session$sendCustomMessage(eff$channel, eff$data),
      "custom" = eff$fn(session),
      warning(sprintf("Unknown effect kind: '%s'", eff$kind), call. = FALSE)
    )
  }
}

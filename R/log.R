#' Create a Transition Record
#'
#' Captures a single state transition for debugging. Used internally by
#' the runtime when `debug = TRUE` and as input to [mvu_replay()].
#'
#' @param seq Integer sequence number.
#' @param type The message type (character or `mvu_enum`).
#' @param value The message payload.
#' @param model_before Model before the transition.
#' @param model_after Model after the transition.
#' @param effects List of effects emitted (may be empty).
#'
#' @return An `mvu_transition` S3 object.
#'
#' @export
mvu_transition <- function(seq, type, value, model_before, model_after,
                           effects = list()) {
  structure(
    list(
      seq = seq,
      timestamp = Sys.time(),
      type = if (inherits(type, "mvu_enum")) as.character(type) else type,
      value = value,
      model_before = model_before,
      model_after = model_after,
      effects = effects
    ),
    class = "mvu_transition"
  )
}

#' @export
print.mvu_transition <- function(x, ...) {
  effs <- if (length(x$effects) > 0) {
    sprintf(" [%d effect(s)]", length(x$effects))
  } else {
    ""
  }
  cat(sprintf(
    "<mvu_transition #%d> %s '%s'%s\n",
    x$seq, format(x$timestamp, "%H:%M:%OS3"), x$type, effs
  ))
  invisible(x)
}

#' Create or Wrap a Transition Log
#'
#' A log is an ordered collection of [mvu_transition()] records. Pass an
#' existing list of transitions to wrap them, or call with no arguments
#' for an empty log.
#'
#' @param transitions A list of `mvu_transition` objects. Defaults to an
#'   empty list.
#'
#' @return An `mvu_log` S3 object.
#'
#' @export
mvu_log <- function(transitions = list()) {
  structure(list(transitions = transitions), class = "mvu_log")
}

#' @export
print.mvu_log <- function(x, ...) {
  n <- length(x$transitions)
  cat(sprintf("MVU Log: %d transition%s\n", n, if (n == 1L) "" else "s"))
  for (t in x$transitions) {
    effs <- if (length(t$effects) > 0) {
      sprintf(" [%d effect(s)]", length(t$effects))
    } else {
      ""
    }
    cat(sprintf(
      "  #%d %s '%s'%s\n",
      t$seq, format(t$timestamp, "%H:%M:%OS3"), t$type, effs
    ))
  }
  invisible(x)
}

#' @export
summary.mvu_log <- function(object, ...) {
  n <- length(object$transitions)
  if (n == 0L) {
    cat("MVU Log: empty\n")
    return(invisible(object))
  }
  types <- vapply(
    object$transitions,
    function(t) as.character(t$type),
    character(1)
  )
  type_counts <- sort(table(types), decreasing = TRUE)
  cat(sprintf("MVU Log: %d transition%s\n", n, if (n == 1L) "" else "s"))
  cat(sprintf(
    "  Time span: %s to %s\n",
    format(object$transitions[[1L]]$timestamp, "%H:%M:%OS3"),
    format(object$transitions[[n]]$timestamp, "%H:%M:%OS3")
  ))
  cat("  Message counts:\n")
  for (i in seq_along(type_counts)) {
    cat(sprintf("    %s: %d\n", names(type_counts)[[i]], type_counts[[i]]))
  }
  n_effects <- sum(vapply(
    object$transitions,
    function(t) length(t$effects),
    integer(1)
  ))
  if (n_effects > 0L) cat(sprintf("  Total effects: %d\n", n_effects))
  invisible(object)
}

#' Replay a Transition Log Through an Update Function
#'
#' Re-runs every message from `log` through `update`, starting from
#' `init`. Returns a list of models — one per step plus the initial
#' state — so you can inspect the model at any point in the history.
#'
#' Effects are collected but not executed; inspect them on the returned
#' `mvu_result` objects if needed.
#'
#' @param init A function returning the initial model, or a model list.
#' @param update The update function.
#' @param log An `mvu_log` object, or a plain list of transitions.
#'
#' @return A list of models of length `n + 1` where `n` is the number
#'   of transitions. Element 1 is the initial model, element `n + 1` is
#'   the final model.
#'
#' @examples
#' \dontrun{
#' models <- mvu_replay(my_init, my_update, saved_log)
#' models[[1]] # initial state
#' models[[length(models)]] # final state
#' }
#'
#' @export
mvu_replay <- function(init, update, log) {
  model <- if (is.function(init)) init() else init
  transitions <- if (inherits(log, "mvu_log")) log$transitions else log

  n <- length(transitions)
  models <- vector("list", n + 1L)
  models[[1L]] <- model

  for (i in seq_along(transitions)) {
    t <- transitions[[i]]
    result <- update(model, t$type, t$value)
    model <- if (is_mvu_result(result)) result$model else result
    models[[i + 1L]] <- model
  }

  models
}

#' Dispatch Messages Through an Update Function
#'
#' A test helper that simulates the MVU runtime loop by sequentially applying
#' messages to a model via the `update` function, without needing a running
#' Shiny session. Since `update` is pure, this enables fast, deterministic
#' unit testing of application logic.
#'
#' When `.collect_effects` is `TRUE`, the return value changes to a list
#' with `$model` and `$effects` fields, allowing tests to inspect which
#' effects would have been executed.
#'
#' @param init Either a function returning the initial model, or a model list
#'   directly.
#' @param update A function with signature `function(model, msg, value)` that
#'   returns a new model or an [mvu_result()].
#' @param messages A list of messages to apply in order. Each element can be:
#'
#'   - A character string (used as the message type, value is `NULL`)
#'   - A list with `$type` and optional `$value` fields
#'
#' @param .collect_effects Logical. When `TRUE`, returns a list with
#'   `$model` and `$effects` instead of the model alone. Defaults to
#'   `FALSE` for backward compatibility.
#'
#' @return When `.collect_effects = FALSE` (default): the final model.
#'   When `.collect_effects = TRUE`: a list with `$model` (final model)
#'   and `$effects` (all effects collected across all messages).
#'
#' @examples
#' update_fn <- function(model, msg, value) {
#'   switch(msg,
#'     increment = list_set(model, count = model$count + 1),
#'     save = mvu_result(model, effect_notify("Saved!")),
#'     model
#'   )
#' }
#'
#' # Just the model (backward compatible)
#' mvu_dispatch(
#'   init = function() list(count = 0),
#'   update = update_fn,
#'   messages = list("increment", "save")
#' )
#'
#' # Model + effects
#' mvu_dispatch(
#'   init = function() list(count = 0),
#'   update = update_fn,
#'   messages = list("increment", "save"),
#'   .collect_effects = TRUE
#' )
#'
#' @export
mvu_dispatch <- function(init, update, messages, .collect_effects = FALSE) {
  model <- if (is.function(init)) init() else init
  all_effects <- list()
  for (msg in messages) {
    type <- if (is.list(msg)) msg$type else msg
    value <- if (is.list(msg)) msg$value else NULL
    result <- update(model, type, value)
    if (is_mvu_result(result)) {
      model <- result$model
      if (.collect_effects) {
        all_effects <- c(all_effects, result$effects)
      }
    } else {
      model <- result
    }
  }
  if (.collect_effects) {
    list(model = model, effects = all_effects)
  } else {
    model
  }
}

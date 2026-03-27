#' Dispatch Messages Through an Update Function
#'
#' A test helper that simulates the MVU runtime loop by sequentially applying
#' messages to a model via the `update` function, without needing a running
#' Shiny session. Since `update` is pure, this enables fast, deterministic
#' unit testing of application logic.
#'
#' @param init Either a function returning the initial model, or a model list
#'   directly.
#' @param update A function with signature `function(model, msg, value)` that
#'   returns a new model.
#' @param messages A list of messages to apply in order. Each element can be:
#'
#'   - A character string (used as the message type, value is `NULL`)
#'   - A list with `$type` and optional `$value` fields
#'
#' @return The final model after all messages have been applied.
#'
#' @examples
#' update_fn <- function(model, msg, value) {
#'   switch(msg,
#'     increment = list_set(model, count = model$count + 1),
#'     decrement = list_set(model, count = model$count - 1),
#'     model
#'   )
#' }
#'
#' mvu_dispatch(
#'   init = function() list(count = 0),
#'   update = update_fn,
#'   messages = list("increment", "increment", "decrement")
#' )
#'
#' mvu_dispatch(
#'   init = list(count = 10),
#'   update = update_fn,
#'   messages = list(
#'     list(type = "increment", value = NULL),
#'     "decrement"
#'   )
#' )
#'
#' @export
mvu_dispatch <- function(init, update, messages) {
  model <- if (is.function(init)) init() else init
  for (msg in messages) {
    type <- if (is.list(msg)) msg$type else msg
    value <- if (is.list(msg)) msg$value else NULL
    model <- update(model, type, value)
  }
  model
}

#' Immutable List Update
#'
#' Creates a modified copy of a list with updated or added elements. The
#' original list is not modified, supporting the immutable update pattern
#' central to the Model-View-Update architecture.
#'
#' @param .l A list to update.
#' @param ... Named arguments specifying elements to update or add.
#'
#' @return A new list with the specified elements updated.
#'
#' @examples
#' model <- list(count = 0, label = "hello")
#' list_set(model, count = 1)
#' list_set(model, count = 5, label = "world")
#'
#' @export
list_set <- function(.l, ...) {
  updates <- list(...)
  for (nm in names(updates)) .l[[nm]] <- updates[[nm]]
  .l
}

#' Get a Value at a Nested Path
#'
#' Retrieves a value from a nested list structure using a character vector
#' path. Returns `NULL` if any intermediate key is missing or not a list.
#'
#' @param .l A list.
#' @param .path A character vector of keys forming the path.
#'
#' @return The value at the path, or `NULL` if not found.
#'
#' @examples
#' model <- list(user = list(profile = list(name = "Alice")))
#' list_get_in(model, c("user", "profile", "name"))
#' list_get_in(model, c("user", "missing"))
#'
#' @export
list_get_in <- function(.l, .path) {
  for (key in .path) {
    if (!is.list(.l) || is.null(.l[[key]])) {
      return(NULL)
    }
    .l <- .l[[key]]
  }
  .l
}

#' Set a Value at a Nested Path
#'
#' Returns a modified copy of a list with `.value` placed at the location
#' specified by `.path`. Missing intermediate keys are created as empty
#' lists. Setting a value to `NULL` removes that key (standard R list
#' semantics).
#'
#' @param .l A list.
#' @param .path A character vector of keys forming the path.
#' @param .value The value to set.
#'
#' @return A new list with the value set at the given path.
#'
#' @examples
#' model <- list(a = list(b = 1))
#' list_set_in(model, c("a", "b"), 99)
#' list_set_in(model, c("a", "c", "d"), "new")
#' list_set_in(list(), c("x", "y"), 42)
#'
#' @export
list_set_in <- function(.l, .path, .value) {
  if (length(.path) == 0L) {
    return(.value)
  }
  if (length(.path) == 1L) {
    .l[[.path]] <- .value
    return(.l)
  }
  key <- .path[[1L]]
  rest <- .path[-1L]
  child <- .l[[key]]
  if (!is.list(child)) child <- list()
  .l[[key]] <- list_set_in(child, rest, .value)
  .l
}

#' Update a Value at a Nested Path with a Function
#'
#' Reads the current value at `.path`, applies `.fn` to it, and writes
#' the result back. If the path does not exist, `.fn` receives `NULL`.
#'
#' @param .l A list.
#' @param .path A character vector of keys forming the path.
#' @param .fn A function taking the current value and returning the new
#'   value.
#'
#' @return A new list with the value at the path transformed by `.fn`.
#'
#' @examples
#' model <- list(counters = list(a = 1, b = 5))
#' list_update_in(model, c("counters", "a"), function(x) x + 1)
#' list_update_in(model, c("counters", "c"), function(x) if (is.null(x)) 0 else x + 1)
#'
#' @export
list_update_in <- function(.l, .path, .fn) {
  current <- list_get_in(.l, .path)
  list_set_in(.l, .path, .fn(current))
}

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

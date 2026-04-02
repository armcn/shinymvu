#' @export
bind_text <- function(tag, js_value) {
  htmltools::tagAppendAttributes(tag, `x-text` = js_value)
}

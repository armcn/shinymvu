on_click <- function(tag, msg) {
  htmltools::tagAppendAttributes(tag, `@click` = send_msg(msg))
}

send_msg <- function(msg) {
  paste0("send('", msg, "')")
}

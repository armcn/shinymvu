mvu_action_button <- function(label = NULL, icon = NULL, width = NULL, ...) {
  icon <- if (!is.null(icon)) {
    tags$span(
      validate_icon(icon),
      class = "action-icon",
      .noWS = c("outside", "inside")
    )
  }

  label <- if (!is.null(label)) {
    tags$span(
      label,
      class = "action-label",
      .noWS = c("outside", "inside")
    )
  }

  tags$button(
    style = htmltools::css(width = htmltools::validateCssUnit(width)),
    type = "button",
    class = "btn btn-default",
    icon,
    label,
    .noWS = "inside",
    ...
  )
}

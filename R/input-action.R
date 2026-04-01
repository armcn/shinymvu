mvu_action_button <- function(label = NULL, icon = NULL, width = NULL, ...) {
  icon <- if (!is.null(icon)) {
    shiny::tags$span(
      icon,
      class = "action-icon",
      .noWS = c("outside", "inside")
    )
  }

  label <- if (!is.null(label)) {
    shiny::tags$span(
      label,
      class = "action-label",
      .noWS = c("outside", "inside")
    )
  }

  shiny::tags$button(
    style = htmltools::css(width = htmltools::validateCssUnit(width)),
    type = "button",
    class = "btn btn-default",
    icon,
    label,
    .noWS = "inside",
    ...
  )
}

validateIcon <- function(icon) {
  if (length(icon) == 0) {
    return(icon)
  }
  if (!isTagLike(icon)) {
    rlang::warn(
      c(
        "It appears that a non-HTML value was provided to `icon`.",
        i = "Try using a `shiny::icon()` (or an equivalent) to get an icon."
      ),
      class = "shiny-validate-icon"
    )
  }
  icon
}

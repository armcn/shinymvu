#' Create a Message-Dispatching Button
#'
#' Generates a `<button>` element wired to dispatch an MVU message via
#' Alpine.js `send()` on click.
#'
#' @param label The button label text.
#' @param msg Character string specifying the message type to dispatch.
#' @param value An optional static value sent with the message. Character
#'   values are quoted; other types are serialized to JSON.
#' @param class CSS class string for the button element.
#' @param ... Additional HTML attributes passed to [shiny::tags]`$button`.
#'
#' @return A `<button>` [shiny::tags] element.
#'
#' @examples
#' mvu_button("Click me", msg = "clicked")
#' mvu_button("+1", msg = "add", value = 1)
#' mvu_button("Save", msg = "save", value = "draft")
#'
#' @export
mvu_button <- function(label, msg, value = NULL, class = "btn btn-primary",
                       ...) {
  value_js <- if (is.null(value)) {
    ""
  } else if (is.character(value)) {
    sprintf(", '%s'", value)
  } else {
    sprintf(", %s", jsonlite::toJSON(value, auto_unbox = TRUE))
  }
  tags$button(
    class = class,
    `@click` = sprintf("send('%s'%s)", msg, value_js),
    ...,
    label
  )
}

#' Create a Button with a JavaScript Value Expression
#'
#' Like [mvu_button()], but accepts a JavaScript expression string for the
#' value instead of a static R value. Useful when the value depends on
#' Alpine.js client-side state.
#'
#' @param label The button label text.
#' @param msg Character string specifying the message type to dispatch.
#' @param value_expr A JavaScript expression string evaluated at click time
#'   to produce the message value.
#' @param class CSS class string for the button element.
#' @param ... Additional HTML attributes passed to [shiny::tags]`$button`.
#'
#' @return A `<button>` [shiny::tags] element.
#'
#' @examples
#' mvu_button_expr("Submit", msg = "submit", value_expr = "model.formData")
#'
#' @export
mvu_button_expr <- function(label, msg, value_expr,
                            class = "btn btn-primary", ...) {
  tags$button(
    class = class,
    `@click` = sprintf("send('%s', %s)", msg, value_expr),
    ...,
    label
  )
}

#' Create a Message-Dispatching Select Input
#'
#' Generates a `<select>` element that dispatches an MVU message on change,
#' sending the selected value.
#'
#' @param label Label text displayed above the select element.
#' @param choices A character vector of option values.
#' @param msg Character string specifying the message type to dispatch.
#' @param value_expr A JavaScript expression string for Alpine.js
#'   `x-bind:value`, typically a path into the model like
#'   `"model.selected"`.
#' @param ... Additional HTML attributes passed to [shiny::tags]`$select`.
#'
#' @return A [htmltools::tagList()] with label and select elements.
#'
#' @examples
#' mvu_select(
#'   label = "Color",
#'   choices = c("red", "green", "blue"),
#'   msg = "set_color",
#'   value_expr = "model.color"
#' )
#'
#' @export
mvu_select <- function(label, choices, msg, value_expr, ...) {
  tagList(
    tags$label(class = "form-label", label),
    tags$select(
      class = "form-select",
      `x-bind:value` = value_expr,
      `@change` = sprintf("send('%s', $event.target.value)", msg),
      ...,
      tags$option(value = "", "Select..."),
      lapply(choices, function(ch) tags$option(value = ch, ch))
    )
  )
}

#' Create a Message-Dispatching Checkbox
#'
#' Generates a Bootstrap-styled checkbox that dispatches an MVU message on
#' change, sending the checked state as a boolean.
#'
#' @param label Label text displayed next to the checkbox.
#' @param msg Character string specifying the message type to dispatch.
#' @param checked_expr A JavaScript expression string for Alpine.js
#'   `x-bind:checked`, typically `"model.isChecked"`.
#' @param ... Additional HTML attributes passed to [shiny::tags]`$input`.
#'
#' @return A `<div>` containing the checkbox input and label.
#'
#' @examples
#' mvu_checkbox(
#'   label = "Enable notifications",
#'   msg = "toggle_notifications",
#'   checked_expr = "model.notifications"
#' )
#'
#' @export
mvu_checkbox <- function(label, msg, checked_expr, ...) {
  div(
    class = "form-check",
    tags$input(
      type = "checkbox", class = "form-check-input",
      `x-bind:checked` = checked_expr,
      `@change` = sprintf("send('%s', $event.target.checked)", msg),
      ...
    ),
    tags$label(class = "form-check-label", label)
  )
}

#' Create a Message-Dispatching Slider
#'
#' Generates a Bootstrap-styled range slider that dispatches an MVU message
#' on change, sending the numeric value.
#'
#' @param label Label text displayed above the slider.
#' @param min Minimum slider value.
#' @param max Maximum slider value.
#' @param msg Character string specifying the message type to dispatch.
#' @param value_expr A JavaScript expression string for Alpine.js
#'   `x-bind:value`, typically `"model.sliderValue"`.
#' @param step Numeric step increment. Defaults to `1`.
#' @param ... Additional HTML attributes passed to [shiny::tags]`$input`.
#'
#' @return A [htmltools::tagList()] with label and range input elements.
#'
#' @examples
#' mvu_slider(
#'   label = "Volume",
#'   min = 0, max = 100,
#'   msg = "set_volume",
#'   value_expr = "model.volume",
#'   step = 5
#' )
#'
#' @export
mvu_slider <- function(label, min, max, msg, value_expr, step = 1, ...) {
  tagList(
    tags$label(class = "form-label", label),
    tags$input(
      type = "range", class = "form-range",
      min = min, max = max, step = step,
      `x-bind:value` = value_expr,
      `@change` = sprintf("send('%s', Number($event.target.value))", msg),
      ...
    )
  )
}

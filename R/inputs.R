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

#' Create a Local Text Input with Debounced Send
#'
#' Generates a text `<input>` bound to Alpine-local state via `x-model`.
#' The value lives in the browser and is never overwritten by the server.
#' When `msg` is provided, the current value is sent to R after the user
#' stops typing for `debounce` milliseconds. This is the recommended
#' approach for text inputs in MVU apps -- it avoids the round-trip
#' latency that makes Elm-style controlled inputs unreliable over a
#' network.
#'
#' For inputs that should only send on explicit action (e.g. a Save
#' button), omit `msg` and read the value from `local` in a
#' [mvu_button_expr()] click handler.
#'
#' @param label Label text displayed above the input.
#' @param local A JavaScript property name for the Alpine-local state,
#'   e.g. `"firstName"`. This must be declared in `extra_js` when
#'   creating the page or module UI.
#' @param msg Optional message type. When provided, sends the value to
#'   R after the debounce delay. When `NULL` (default), the value stays
#'   local until explicitly sent.
#' @param debounce Milliseconds to wait after the last keystroke before
#'   sending. Only used when `msg` is not `NULL`. Defaults to `300`.
#' @param ... Additional HTML attributes passed to [shiny::tags]`$input`.
#'
#' @return A [htmltools::tagList()] with label and input elements.
#'
#' @examples
#' # Debounced send: value dispatched 300ms after typing stops
#' mvu_text_local("Search", local = "search", msg = "search")
#'
#' # No auto-send: use a button to send explicitly
#' mvu_text_local("Name", local = "name")
#'
#' @export
mvu_text_local <- function(label, local, msg = NULL, debounce = 300, ...) {
  if (!is.null(msg)) {
    input_attr <- sprintf(
      "@input.debounce.%dms=\"send('%s', %s)\"",
      as.integer(debounce), msg, local
    )
    input_tag <- tags$input(
      type = "text", class = "form-control",
      `x-model` = local,
      ...
    )
    input_tag$attribs[[sprintf("@input.debounce.%dms", as.integer(debounce))]] <-
      sprintf("send('%s', %s)", msg, local)
  } else {
    input_tag <- tags$input(
      type = "text", class = "form-control",
      `x-model` = local,
      ...
    )
  }
  tagList(
    tags$label(class = "form-label", label),
    input_tag
  )
}

#' Create a Local Textarea with Debounced Send
#'
#' Like [mvu_text_local()] but renders a `<textarea>`. The value lives
#' in Alpine-local state and is optionally sent to R after debounce.
#'
#' @inheritParams mvu_text_local
#' @param rows Number of visible text rows. Defaults to 3.
#'
#' @return A [htmltools::tagList()] with label and textarea elements.
#'
#' @examples
#' mvu_textarea_local("Notes", local = "notes", msg = "set_notes", debounce = 500)
#'
#' @export
mvu_textarea_local <- function(label, local, msg = NULL, debounce = 300,
                               rows = 3, ...) {
  textarea_tag <- tags$textarea(
    class = "form-control", rows = rows,
    `x-model` = local,
    ...
  )
  if (!is.null(msg)) {
    textarea_tag$attribs[[sprintf("@input.debounce.%dms", as.integer(debounce))]] <-
      sprintf("send('%s', %s)", msg, local)
  }
  tagList(
    tags$label(class = "form-label", label),
    textarea_tag
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

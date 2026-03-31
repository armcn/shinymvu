# --------------------------------------------------------------------------
# Pipe-based view helpers for shinymvu
#
# Bind helpers:
#   bind_text(tag, expr)          x-text
#   bind_value(tag, expr)         x-bind:value
#   bind_checked(tag, expr)       x-bind:checked
#   bind_show(tag, expr)          x-show
#   bind_if(tag, expr)            x-if
#   bind_for(tag, var, items)     x-for
#   bind_key(tag, expr)           x-bind:key
#   bind_class(tag, expr)         x-bind:class
#   bind_style(tag, expr)         x-bind:style
#   bind_disabled(tag, expr)      x-bind:disabled
#   bind_attr(tag, name, expr)    x-bind:{name}
#
# Event helpers:
#   on_click(tag, msg, value)     @click
#   on_input(tag, msg, debounce)  @change / @input.debounce
#   on_input_num(tag, msg, ...)   @change with Number()
#   on_check(tag, msg)            @change with checked
#   on_change(tag, msg, value)    @change with static value
#   on_key(tag, key, msg)         @keydown.{key}
#   on_file(tag, msg)             @change with file metadata
#
# All helpers take a tag as the first argument and return a
# modified tag, enabling |> pipe composition.
#
# Bind helpers that target display properties (text, show, class,
# style, if, for, key) modify the tag directly.
#
# Bind helpers that target form state (value, checked, disabled)
# and all event helpers use add_to_input() to find the first
# <input>, <select>, <textarea>, or <button> inside a wrapper
# and modify it.
# --------------------------------------------------------------------------

# -- Bind helpers (exported) ------------------------------------------------

#' Bind Text Content
#'
#' Sets the element's text content from a JavaScript expression.
#'
#' @param tag An htmltools tag.
#' @param expr A JavaScript expression string referencing the model
#'   (e.g. `"model.count"`).
#'
#' @return The modified tag.
#'
#' @examples
#' shiny::tags$span() |> bind_text("model.count")
#'
#' @export
bind_text <- function(tag, expr) {
  tagAppendAttributes(tag, `x-text` = expr)
}

#' Bind Input Value
#'
#' Keeps an input element's value in sync with a model field. When
#' used on a wrapper element (e.g. from [mvu_text_input()]), the
#' binding is added to the form element inside.
#'
#' @inheritParams bind_text
#'
#' @return The modified tag.
#'
#' @examples
#' shiny::tags$input(type = "text") |>
#'   bind_value("model.name")
#'
#' @export
bind_value <- function(tag, expr) {
  add_to_input(tag, `x-bind:value` = expr)
}

#' Bind Checked State
#'
#' Keeps a checkbox or radio input's checked state in sync with a
#' JavaScript expression.
#'
#' @inheritParams bind_text
#'
#' @return The modified tag.
#'
#' @examples
#' shiny::tags$input(type = "checkbox") |>
#'   bind_checked("model.agree")
#'
#' @export
bind_checked <- function(tag, expr) {
  add_to_input(tag, `x-bind:checked` = expr)
}

#' Conditional Show / Hide
#'
#' Toggles an element's visibility via CSS `display`. The element
#' stays in the DOM. For full DOM removal, see [bind_if()].
#'
#' @inheritParams bind_text
#'
#' @return The modified tag.
#'
#' @examples
#' shiny::tags$div("Loading...") |>
#'   bind_show("model.loading")
#'
#' @export
bind_show <- function(tag, expr) {
  tagAppendAttributes(tag, `x-show` = expr)
}

#' Conditional Render / Remove
#'
#' Adds or removes an element from the DOM based on a condition.
#' Must be used on a `<template>` tag. For CSS-only toggling, see
#' [bind_show()].
#'
#' @inheritParams bind_text
#'
#' @return The modified tag.
#'
#' @examples
#' shiny::tags$template(
#'   shiny::tags$p("No items.")
#' ) |>
#'   bind_if("model.items.length === 0")
#'
#' @export
bind_if <- function(tag, expr) {
  tagAppendAttributes(tag, `x-if` = expr)
}

#' List Rendering
#'
#' Iterates over a list in the model. Must be used on a `<template>`
#' tag. Inside the template, use the loop variable name to refer to
#' each item (e.g. `"item.name"`).
#'
#' @param tag An htmltools `<template>` tag.
#' @param var The loop variable name (e.g. `"item"`).
#' @param items A JavaScript expression for the list
#'   (e.g. `"model.items"`).
#'
#' @return The modified tag.
#'
#' @examples
#' shiny::tags$template(
#'   shiny::tags$li() |> bind_text("item")
#' ) |>
#'   bind_for("item", "model.items")
#'
#' @export
bind_for <- function(tag, var, items) {
  tagAppendAttributes(tag, `x-for` = paste(var, "in", items))
}

#' Bind Loop Key
#'
#' Sets a unique key for efficient DOM updates inside a loop.
#'
#' @inheritParams bind_text
#'
#' @return The modified tag.
#'
#' @examples
#' shiny::tags$li() |> bind_key("item.id")
#'
#' @export
bind_key <- function(tag, expr) {
  tagAppendAttributes(tag, `x-bind:key` = expr)
}

#' Bind Dynamic CSS Class
#'
#' Dynamically sets CSS classes from a JavaScript expression.
#'
#' @inheritParams bind_text
#'
#' @return The modified tag.
#'
#' @examples
#' shiny::tags$div() |>
#'   bind_class("model.active ? 'active' : ''")
#'
#' shiny::tags$li() |>
#'   bind_class("{ 'completed': item.done }")
#'
#' @export
bind_class <- function(tag, expr) {
  tagAppendAttributes(tag, `x-bind:class` = expr)
}

#' Bind Dynamic Style
#'
#' Dynamically sets inline styles from a JavaScript expression.
#'
#' @inheritParams bind_text
#'
#' @return The modified tag.
#'
#' @examples
#' shiny::tags$div(class = "progress-bar") |>
#'   bind_style("'width: ' + model.progress + '%'")
#'
#' @export
bind_style <- function(tag, expr) {
  tagAppendAttributes(tag, `x-bind:style` = expr)
}

#' Bind Disabled State
#'
#' Dynamically disables a form element based on a JavaScript
#' expression.
#'
#' @inheritParams bind_text
#'
#' @return The modified tag.
#'
#' @examples
#' shiny::tags$button("Submit") |>
#'   bind_disabled("model.loading")
#'
#' @export
bind_disabled <- function(tag, expr) {
  add_to_input(tag, `x-bind:disabled` = expr)
}

#' Bind Any Attribute
#'
#' Dynamically binds any HTML attribute to a JavaScript expression.
#'
#' @param tag An htmltools tag.
#' @param name The attribute name (e.g. `"href"`, `"src"`).
#' @param expr A JavaScript expression string.
#'
#' @return The modified tag.
#'
#' @examples
#' shiny::tags$a() |>
#'   bind_attr("href", "model.url") |>
#'   bind_text("model.linkText")
#'
#' @export
bind_attr <- function(tag, name, expr) {
  attrs <- setNames(list(expr), paste0("x-bind:", name))
  do.call(tagAppendAttributes, c(list(tag), attrs))
}

# -- Event helpers (exported) -----------------------------------------------

#' Click Event Handler
#'
#' Dispatches a message when the element is clicked.
#'
#' @param tag An htmltools tag.
#' @param msg Character string specifying the message type.
#' @param value Optional payload. An R value (serialized to JSON) or
#'   a JavaScript expression string for a dynamic value.
#'
#' @return The modified tag.
#'
#' @examples
#' shiny::tags$button("+") |> on_click("increment")
#' shiny::tags$button("Page 2") |> on_click("set_page", 2)
#'
#' @export
on_click <- function(tag, msg, value = NULL) {
  add_to_input(tag, `@click` = send_js(msg, static_value_js(value)))
}

#' Input Event Handler (String Value)
#'
#' Dispatches the element's string value as a message. Use for text
#' inputs, selects, date inputs, and color inputs.
#'
#' @param tag An htmltools tag.
#' @param msg Character string specifying the message type.
#' @param debounce Optional debounce delay in milliseconds. When set,
#'   listens on `input` (every keystroke, debounced) instead of
#'   `change` (on blur).
#'
#' @return The modified tag.
#'
#' @examples
#' shiny::tags$input(type = "text") |>
#'   on_input("set_name")
#'
#' shiny::tags$input(type = "text") |>
#'   on_input("set_search", debounce = 300)
#'
#' @export
on_input <- function(tag, msg, debounce = NULL) {
  event <- if (!is.null(debounce)) {
    sprintf("@input.debounce.%dms", as.integer(debounce))
  } else {
    "@change"
  }
  add_input_event(tag, event, send_js(msg, "$event.target.value"))
}

#' Input Event Handler (Numeric Value)
#'
#' Like [on_input()] but coerces the value to a number before
#' sending. Use for number and range inputs.
#'
#' @inheritParams on_input
#'
#' @return The modified tag.
#'
#' @examples
#' shiny::tags$input(type = "number") |>
#'   on_input_num("set_age")
#'
#' @export
on_input_num <- function(tag, msg, debounce = NULL) {
  event <- if (!is.null(debounce)) {
    sprintf("@input.debounce.%dms", as.integer(debounce))
  } else {
    "@change"
  }
  add_input_event(tag, event, send_js(msg, "Number($event.target.value)"))
}

#' Checkbox Change Event Handler
#'
#' Dispatches the element's checked state (boolean) as a message.
#'
#' @param tag An htmltools tag.
#' @param msg Character string specifying the message type.
#'
#' @return The modified tag.
#'
#' @examples
#' shiny::tags$input(type = "checkbox") |>
#'   on_check("toggle_agree")
#'
#' @export
on_check <- function(tag, msg) {
  add_to_input(tag, `@change` = send_js(msg, "$event.target.checked"))
}

#' Change Event Handler (Static Value)
#'
#' Dispatches a message with a static value on change. Useful for
#' checkbox group items where each checkbox sends a fixed value.
#'
#' @param tag An htmltools tag.
#' @param msg Character string specifying the message type.
#' @param value A static R value sent with the message.
#'
#' @return The modified tag.
#'
#' @examples
#' shiny::tags$input(type = "checkbox", value = "cheese") |>
#'   on_change("toggle_topping", "cheese")
#'
#' @export
on_change <- function(tag, msg, value) {
  add_to_input(tag, `@change` = send_js(msg, static_value_js(value)))
}

#' Keyboard Event Handler
#'
#' Dispatches a message when a specific key is pressed.
#'
#' @param tag An htmltools tag.
#' @param key The key name (e.g. `"enter"`, `"escape"`, `"space"`).
#' @param msg Character string specifying the message type.
#'
#' @return The modified tag.
#'
#' @examples
#' shiny::tags$input(type = "text") |>
#'   on_key("enter", "submit") |>
#'   on_key("escape", "cancel")
#'
#' @export
on_key <- function(tag, key, msg) {
  add_input_event(tag, paste0("@keydown.", key), send_js(msg))
}

#' File Input Event Handler
#'
#' Dispatches file metadata (name, size, type) as a message when
#' files are selected.
#'
#' @param tag An htmltools tag.
#' @param msg Character string specifying the message type.
#'
#' @return The modified tag.
#'
#' @examples
#' shiny::tags$input(type = "file") |> on_file("upload")
#'
#' @export
on_file <- function(tag, msg) {
  file_js <- paste0(
    "Array.from($event.target.files).map(function(f){",
    "return {name:f.name,size:f.size,type:f.type};})"
  )
  add_to_input(tag, `@change` = send_js(msg, file_js))
}

# -- Internal helpers -------------------------------------------------------

add_to_input <- function(tag, ...) {
  form_names <- c("input", "select", "textarea", "button")
  if (!is.null(tag$name) && tag$name %in% form_names) {
    return(tagAppendAttributes(tag, ...))
  }
  for (i in seq_along(tag$children)) {
    child <- tag$children[[i]]
    if (inherits(child, "shiny.tag")) {
      if (!is.null(child$name) && child$name %in% form_names) {
        tag$children[[i]] <- tagAppendAttributes(child, ...)
        return(tag)
      }
      modified <- add_to_input(child, ...)
      if (!identical(modified, child)) {
        tag$children[[i]] <- modified
        return(tag)
      }
    }
  }
  tagAppendAttributes(tag, ...)
}

add_input_event <- function(tag, event, js) {
  attrs <- setNames(list(js), event)
  do.call(add_to_input, c(list(tag), attrs))
}

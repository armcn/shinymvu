# -- Internal helpers ----------------------------------------------------------

send_js <- function(msg, value_expr = NULL) {
  if (is.null(value_expr)) sprintf("send('%s')", msg)
  else sprintf("send('%s', %s)", msg, value_expr)
}

static_value_js <- function(value) {
  if (is.null(value)) return(NULL)
  if (is.character(value)) return(sprintf("'%s'", value))
  as.character(jsonlite::toJSON(value, auto_unbox = TRUE))
}

random_id <- function(prefix = "mvu") {
  paste0(prefix, "-", as.hexmode(sample.int(.Machine$integer.max, 1)))
}

make_options <- function(choices) {
  nms <- if (!is.null(names(choices))) names(choices) else unname(choices)
  vals <- unname(choices)
  mapply(function(label, value) {
    tags$option(value = value, label)
  }, nms, vals, SIMPLIFY = FALSE, USE.NAMES = FALSE)
}

# -- Event handlers (exported) ------------------------------------------------

#' Alpine.js Click Event Handler
#'
#' Returns a named list with an `@click` attribute that dispatches an MVU
#' message. Splice into any tag with `!!!`.
#'
#' @param msg Character string specifying the message type.
#' @param value An optional static R value sent with the message.
#'
#' @return A named list with one `@click` element.
#'
#' @examples
#' shiny::tags$button(!!!on_click("increment"), "Click me")
#' shiny::tags$button(!!!on_click("set_page", value = 2), "Page 2")
#'
#' @export
on_click <- function(msg, value = NULL) {
  setNames(
    list(send_js(msg, static_value_js(value))),
    "@click"
  )
}

#' Alpine.js Click Event Handler with JavaScript Expression
#'
#' Like [on_click()] but accepts a JavaScript expression for the value,
#' evaluated at click time.
#'
#' @param msg Character string specifying the message type.
#' @param value_expr A JavaScript expression string.
#'
#' @return A named list with one `@click` element.
#'
#' @examples
#' shiny::tags$button(!!!on_click_expr("submit", "model.formData"), "Submit")
#'
#' @export
on_click_expr <- function(msg, value_expr) {
  setNames(list(send_js(msg, value_expr)), "@click")
}

#' Alpine.js Input/Change Event Handler (String Value)
#'
#' Returns a named list with a `@change` (or debounced `@input`) attribute
#' that dispatches the element's string value as an MVU message.
#'
#' @param msg Character string specifying the message type.
#' @param debounce Optional debounce delay in milliseconds. When set, uses
#'   `@input.debounce` instead of `@change`, suitable for text inputs.
#'
#' @return A named list with one event attribute.
#'
#' @examples
#' shiny::tags$select(!!!on_input("set_color"),
#'   shiny::tags$option(value = "red", "Red"),
#'   shiny::tags$option(value = "blue", "Blue")
#' )
#'
#' @export
on_input <- function(msg, debounce = NULL) {
  event <- if (!is.null(debounce)) {
    sprintf("@input.debounce.%dms", as.integer(debounce))
  } else {
    "@change"
  }
  setNames(list(send_js(msg, "$event.target.value")), event)
}

#' Alpine.js Change Event Handler (Boolean Value)
#'
#' Returns a named list with a `@change` attribute that dispatches the
#' element's checked state as an MVU message. Use with checkboxes.
#'
#' @param msg Character string specifying the message type.
#'
#' @return A named list with one `@change` element.
#'
#' @examples
#' shiny::tags$input(type = "checkbox", !!!on_check("toggle_dark_mode"))
#'
#' @export
on_check <- function(msg) {
  setNames(
    list(send_js(msg, "$event.target.checked")),
    "@change"
  )
}

#' Alpine.js Change Event Handler (Numeric Value)
#'
#' Returns a named list with a `@change` (or debounced `@input`) attribute
#' that dispatches the element's value coerced to a number.
#'
#' @param msg Character string specifying the message type.
#' @param debounce Optional debounce delay in milliseconds.
#'
#' @return A named list with one event attribute.
#'
#' @examples
#' shiny::tags$input(type = "range", min = "0", max = "100",
#'   !!!on_input_num("set_volume"))
#'
#' @export
on_input_num <- function(msg, debounce = NULL) {
  event <- if (!is.null(debounce)) {
    sprintf("@input.debounce.%dms", as.integer(debounce))
  } else {
    "@change"
  }
  setNames(list(send_js(msg, "Number($event.target.value)")), event)
}

# -- Input wrappers (exported) ------------------------------------------------

#' Action Button
#'
#' A `<button>` that dispatches an MVU message on click.
#'
#' @param label The button label.
#' @param msg Message type to dispatch.
#' @param value Optional static value sent with the message.
#' @param class CSS classes. Defaults to Bootstrap primary button.
#' @param ... Additional HTML attributes.
#'
#' @return A `<button>` tag.
#'
#' @examples
#' mvu_button("Click me", msg = "clicked")
#' mvu_button("+1", msg = "add", value = 1)
#'
#' @export
mvu_button <- function(label, msg, value = NULL, class = "btn btn-primary",
                       ...) {
  do.call(tags$button, c(
    list(class = class),
    on_click(msg, value),
    list(...),
    list(label)
  ))
}

#' Text Input
#'
#' A text `<input>` that dispatches its value as an MVU message on change.
#'
#' @param label Label text.
#' @param msg Message type to dispatch.
#' @param bind Alpine.js expression for the displayed value (e.g.
#'   `"model.name"`). Optional.
#' @param placeholder Placeholder text.
#' @param debounce Debounce delay in milliseconds. When set, sends on
#'   every keystroke (debounced) instead of on blur.
#' @param class CSS classes for the input.
#' @param ... Additional HTML attributes.
#'
#' @return A [htmltools::tagList()] with label and input.
#'
#' @examples
#' mvu_text_input("Name", msg = "set_name", bind = "model.name")
#'
#' @export
mvu_text_input <- function(label, msg, bind = NULL, placeholder = NULL,
                           debounce = NULL, class = "form-control", ...) {
  input_id <- random_id()
  attrs <- c(
    list(type = "text", class = class, id = input_id),
    on_input(msg, debounce = debounce),
    list(...)
  )
  if (!is.null(bind)) attrs[["x-bind:value"]] <- bind
  if (!is.null(placeholder)) attrs[["placeholder"]] <- placeholder
  tagList(
    tags$label(class = "form-label", `for` = input_id, label),
    do.call(tags$input, attrs)
  )
}

#' Numeric Input
#'
#' A number `<input>` that dispatches its numeric value as an MVU message.
#'
#' @inheritParams mvu_text_input
#' @param min,max,step Numeric constraints for the input.
#'
#' @return A [htmltools::tagList()] with label and input.
#'
#' @examples
#' mvu_numeric_input("Age", msg = "set_age", bind = "model.age",
#'   min = 0, max = 120)
#'
#' @export
mvu_numeric_input <- function(label, msg, bind = NULL,
                              min = NULL, max = NULL, step = NULL,
                              debounce = NULL, class = "form-control", ...) {
  input_id <- random_id()
  attrs <- c(
    list(type = "number", class = class, id = input_id),
    on_input_num(msg, debounce = debounce),
    list(...)
  )
  if (!is.null(bind)) attrs[["x-bind:value"]] <- bind
  if (!is.null(min)) attrs[["min"]] <- min
  if (!is.null(max)) attrs[["max"]] <- max
  if (!is.null(step)) attrs[["step"]] <- step
  tagList(
    tags$label(class = "form-label", `for` = input_id, label),
    do.call(tags$input, attrs)
  )
}

#' Password Input
#'
#' A password `<input>` that dispatches its value as an MVU message.
#'
#' @inheritParams mvu_text_input
#'
#' @return A [htmltools::tagList()] with label and input.
#'
#' @examples
#' mvu_password_input("Password", msg = "set_password")
#'
#' @export
mvu_password_input <- function(label, msg, bind = NULL, placeholder = NULL,
                               debounce = NULL, class = "form-control", ...) {
  input_id <- random_id()
  attrs <- c(
    list(type = "password", class = class, id = input_id),
    on_input(msg, debounce = debounce),
    list(...)
  )
  if (!is.null(bind)) attrs[["x-bind:value"]] <- bind
  if (!is.null(placeholder)) attrs[["placeholder"]] <- placeholder
  tagList(
    tags$label(class = "form-label", `for` = input_id, label),
    do.call(tags$input, attrs)
  )
}

#' Textarea Input
#'
#' A `<textarea>` that dispatches its value as an MVU message.
#'
#' @inheritParams mvu_text_input
#' @param rows Number of visible text rows.
#'
#' @return A [htmltools::tagList()] with label and textarea.
#'
#' @examples
#' mvu_textarea_input("Bio", msg = "set_bio", bind = "model.bio", rows = 4)
#'
#' @export
mvu_textarea_input <- function(label, msg, bind = NULL, rows = 3,
                               placeholder = NULL, debounce = NULL,
                               class = "form-control", ...) {
  input_id <- random_id()
  attrs <- c(
    list(class = class, id = input_id, rows = rows),
    on_input(msg, debounce = debounce),
    list(...)
  )
  if (!is.null(bind)) attrs[["x-bind:value"]] <- bind
  if (!is.null(placeholder)) attrs[["placeholder"]] <- placeholder
  tagList(
    tags$label(class = "form-label", `for` = input_id, label),
    do.call(tags$textarea, attrs)
  )
}

#' Select Input
#'
#' A `<select>` dropdown that dispatches the selected value as an MVU message.
#'
#' @param label Label text.
#' @param choices A character vector of option values. If named, names are
#'   used as display labels.
#' @param msg Message type to dispatch.
#' @param bind Alpine.js expression for the selected value.
#' @param class CSS classes for the select.
#' @param ... Additional HTML attributes.
#'
#' @return A [htmltools::tagList()] with label and select.
#'
#' @examples
#' mvu_select_input("Color",
#'   choices = c("Red" = "red", "Green" = "green", "Blue" = "blue"),
#'   msg = "set_color", bind = "model.color")
#'
#' @export
mvu_select_input <- function(label, choices, msg, bind = NULL,
                             class = "form-select", ...) {
  input_id <- random_id()
  attrs <- c(
    list(class = class, id = input_id),
    on_input(msg),
    list(...)
  )
  if (!is.null(bind)) attrs[["x-bind:value"]] <- bind
  tagList(
    tags$label(class = "form-label", `for` = input_id, label),
    do.call(tags$select, c(attrs, make_options(choices)))
  )
}

#' Checkbox Input
#'
#' A Bootstrap checkbox that dispatches its checked state as an MVU message.
#'
#' @param label Label text.
#' @param msg Message type to dispatch.
#' @param bind Alpine.js expression for the checked state.
#' @param class CSS classes for the wrapper div.
#' @param ... Additional HTML attributes on the `<input>`.
#'
#' @return A `<div>` containing the checkbox and label.
#'
#' @examples
#' mvu_checkbox_input("I agree", msg = "toggle_agree", bind = "model.agree")
#'
#' @export
mvu_checkbox_input <- function(label, msg, bind = NULL,
                               class = "form-check", ...) {
  input_id <- random_id()
  attrs <- c(
    list(type = "checkbox", class = "form-check-input", id = input_id),
    on_check(msg),
    list(...)
  )
  if (!is.null(bind)) attrs[["x-bind:checked"]] <- bind
  div(class = class,
    do.call(tags$input, attrs),
    tags$label(class = "form-check-label", `for` = input_id, label)
  )
}

#' Checkbox Group Input
#'
#' A group of checkboxes where each toggle dispatches the same message type
#' with the checkbox's value. Your `update` function should toggle the value
#' in/out of a list.
#'
#' @param label Group label text.
#' @param choices A character vector of values. If named, names are used as
#'   display labels.
#' @param msg Message type dispatched for each toggle.
#' @param bind Alpine.js expression for the array of selected values (e.g.
#'   `"model.toppings"`).
#' @param inline If `TRUE`, render checkboxes horizontally.
#' @param ... Additional HTML attributes on each `<input>`.
#'
#' @return A [htmltools::tagList()] with group label and checkboxes.
#'
#' @examples
#' mvu_checkbox_group_input("Toppings",
#'   choices = c("Cheese", "Peppers", "Onions"),
#'   msg = "toggle_topping", bind = "model.toppings")
#'
#' @export
mvu_checkbox_group_input <- function(label, choices, msg, bind = NULL,
                                     inline = FALSE, ...) {
  nms <- if (!is.null(names(choices))) names(choices) else unname(choices)
  vals <- unname(choices)
  check_class <- if (inline) "form-check form-check-inline" else "form-check"
  boxes <- mapply(function(lbl, val) {
    input_id <- random_id()
    attrs <- c(
      list(
        type = "checkbox", class = "form-check-input", id = input_id,
        value = val
      ),
      setNames(list(send_js(msg, sprintf("'%s'", val))), "@change"),
      list(...)
    )
    if (!is.null(bind)) {
      attrs[["x-bind:checked"]] <- sprintf("%s.includes('%s')", bind, val)
    }
    div(class = check_class,
      do.call(tags$input, attrs),
      tags$label(class = "form-check-label", `for` = input_id, lbl)
    )
  }, nms, vals, SIMPLIFY = FALSE, USE.NAMES = FALSE)
  tagList(
    tags$label(class = "form-label", label),
    tagList(boxes)
  )
}

#' Radio Button Input
#'
#' A group of radio buttons that dispatch the selected value as an MVU
#' message.
#'
#' @param label Group label text.
#' @param choices A character vector of values. If named, names are used as
#'   display labels.
#' @param msg Message type to dispatch.
#' @param bind Alpine.js expression for the selected value.
#' @param inline If `TRUE`, render radio buttons horizontally.
#' @param ... Additional HTML attributes on each `<input>`.
#'
#' @return A [htmltools::tagList()] with group label and radio buttons.
#'
#' @examples
#' mvu_radio_input("Pet",
#'   choices = c("Dog" = "dog", "Cat" = "cat", "Fish" = "fish"),
#'   msg = "set_pet", bind = "model.pet")
#'
#' @export
mvu_radio_input <- function(label, choices, msg, bind = NULL,
                            inline = FALSE, ...) {
  nms <- if (!is.null(names(choices))) names(choices) else unname(choices)
  vals <- unname(choices)
  group_name <- random_id("radio")
  radio_class <- if (inline) "form-check form-check-inline" else "form-check"
  radios <- mapply(function(lbl, val) {
    input_id <- random_id()
    attrs <- c(
      list(
        type = "radio", class = "form-check-input", id = input_id,
        name = group_name, value = val
      ),
      on_input(msg),
      list(...)
    )
    if (!is.null(bind)) {
      attrs[["x-bind:checked"]] <- sprintf("%s === '%s'", bind, val)
    }
    div(class = radio_class,
      do.call(tags$input, attrs),
      tags$label(class = "form-check-label", `for` = input_id, lbl)
    )
  }, nms, vals, SIMPLIFY = FALSE, USE.NAMES = FALSE)
  tagList(
    tags$label(class = "form-label", label),
    tagList(radios)
  )
}

#' Slider Input
#'
#' A range `<input>` that dispatches its numeric value as an MVU message.
#'
#' @param label Label text.
#' @param min,max Numeric range bounds.
#' @param msg Message type to dispatch.
#' @param bind Alpine.js expression for the current value.
#' @param step Numeric step increment.
#' @param class CSS classes for the input.
#' @param ... Additional HTML attributes.
#'
#' @return A [htmltools::tagList()] with label and range input.
#'
#' @examples
#' mvu_slider_input("Volume", min = 0, max = 100, msg = "set_volume",
#'   bind = "model.volume")
#'
#' @export
mvu_slider_input <- function(label, min, max, msg, bind = NULL, step = 1,
                             class = "form-range", ...) {
  input_id <- random_id()
  attrs <- c(
    list(
      type = "range", class = class, id = input_id,
      min = min, max = max, step = step
    ),
    on_input_num(msg),
    list(...)
  )
  if (!is.null(bind)) attrs[["x-bind:value"]] <- bind
  tagList(
    tags$label(class = "form-label", `for` = input_id, label),
    do.call(tags$input, attrs)
  )
}

#' Date Input
#'
#' A date `<input>` that dispatches the selected date string as an MVU
#' message.
#'
#' @param label Label text.
#' @param msg Message type to dispatch.
#' @param bind Alpine.js expression for the current date value.
#' @param min,max Date range constraints (character strings in
#'   `"YYYY-MM-DD"` format).
#' @param class CSS classes for the input.
#' @param ... Additional HTML attributes.
#'
#' @return A [htmltools::tagList()] with label and date input.
#'
#' @examples
#' mvu_date_input("Birthday", msg = "set_birthday", bind = "model.birthday")
#'
#' @export
mvu_date_input <- function(label, msg, bind = NULL, min = NULL, max = NULL,
                           class = "form-control", ...) {
  input_id <- random_id()
  attrs <- c(
    list(type = "date", class = class, id = input_id),
    on_input(msg),
    list(...)
  )
  if (!is.null(bind)) attrs[["x-bind:value"]] <- bind
  if (!is.null(min)) attrs[["min"]] <- as.character(min)
  if (!is.null(max)) attrs[["max"]] <- as.character(max)
  tagList(
    tags$label(class = "form-label", `for` = input_id, label),
    do.call(tags$input, attrs)
  )
}

#' Date Range Input
#'
#' Two date `<input>` elements for selecting a start and end date. Each
#' dispatches a separate MVU message.
#'
#' @param label Group label text.
#' @param msg_start,msg_end Message types for the start and end dates.
#' @param bind_start,bind_end Alpine.js expressions for the date values.
#' @param min,max Date constraints (character strings in `"YYYY-MM-DD"`
#'   format).
#' @param label_start,label_end Labels for the individual date inputs.
#' @param class CSS classes for each input.
#' @param ... Additional HTML attributes on each input.
#'
#' @return A [htmltools::tagList()] with group label and two date inputs.
#'
#' @examples
#' mvu_date_range_input("Trip dates",
#'   msg_start = "set_depart", msg_end = "set_return",
#'   bind_start = "model.depart", bind_end = "model.return")
#'
#' @export
mvu_date_range_input <- function(label, msg_start, msg_end,
                                 bind_start = NULL, bind_end = NULL,
                                 min = NULL, max = NULL,
                                 label_start = "Start", label_end = "End",
                                 class = "form-control", ...) {
  start_id <- random_id()
  end_id <- random_id()
  start_attrs <- c(
    list(type = "date", class = class, id = start_id),
    on_input(msg_start),
    list(...)
  )
  end_attrs <- c(
    list(type = "date", class = class, id = end_id),
    on_input(msg_end),
    list(...)
  )
  if (!is.null(bind_start)) start_attrs[["x-bind:value"]] <- bind_start
  if (!is.null(bind_end)) end_attrs[["x-bind:value"]] <- bind_end
  if (!is.null(min)) {
    start_attrs[["min"]] <- as.character(min)
    end_attrs[["min"]] <- as.character(min)
  }
  if (!is.null(max)) {
    start_attrs[["max"]] <- as.character(max)
    end_attrs[["max"]] <- as.character(max)
  }
  tagList(
    tags$label(class = "form-label", label),
    div(class = "d-flex gap-2",
      div(class = "flex-fill",
        tags$label(
          class = "form-label small text-muted", `for` = start_id,
          label_start
        ),
        do.call(tags$input, start_attrs)
      ),
      div(class = "flex-fill",
        tags$label(
          class = "form-label small text-muted", `for` = end_id,
          label_end
        ),
        do.call(tags$input, end_attrs)
      )
    )
  )
}

#' File Input
#'
#' A file `<input>` that dispatches file metadata (name, size, type) as an
#' MVU message when files are selected. File contents are not sent
#' automatically; for content access, use Shiny's native file upload or
#' custom JavaScript with `on_click_expr`.
#'
#' @param label Label text.
#' @param msg Message type to dispatch.
#' @param accept A character vector of accepted MIME types or file extensions
#'   (e.g. `c(".csv", ".xlsx")` or `"image/*"`).
#' @param multiple If `TRUE`, allow selecting multiple files.
#' @param class CSS classes for the input.
#' @param ... Additional HTML attributes.
#'
#' @return A [htmltools::tagList()] with label and file input.
#'
#' @examples
#' mvu_file_input("Upload CSV", msg = "upload", accept = ".csv")
#' mvu_file_input("Photos", msg = "upload_photos",
#'   accept = "image/*", multiple = TRUE)
#'
#' @export
mvu_file_input <- function(label, msg, accept = NULL, multiple = FALSE,
                           class = "form-control", ...) {
  input_id <- random_id()
  file_js <- paste0(
    "Array.from($event.target.files).map(function(f){",
    "return {name:f.name,size:f.size,type:f.type};})"
  )
  attrs <- c(
    list(type = "file", class = class, id = input_id),
    setNames(list(send_js(msg, file_js)), "@change"),
    list(...)
  )
  if (!is.null(accept)) attrs[["accept"]] <- paste(accept, collapse = ",")
  if (multiple) attrs[["multiple"]] <- NA
  tagList(
    tags$label(class = "form-label", `for` = input_id, label),
    do.call(tags$input, attrs)
  )
}

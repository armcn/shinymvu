# --------------------------------------------------------------------------
# Structural input helpers for shinymvu
#
# These functions generate Bootstrap 5 form markup matching the
# style and structure of Shiny's native inputs. They handle labels,
# CSS classes, IDs, and option generation.
#
# MVU wiring (bind_value, on_input, etc.) is added via the pipe
# API from R/view.R. Exceptions are checkbox groups and radio
# buttons, where per-choice wiring requires the msg and selected
# parameters.
# --------------------------------------------------------------------------

# -- Action Button ----------------------------------------------------------

#' Action Button
#'
#' Creates a Bootstrap button. Add click handling with [on_click()].
#'
#' @param label The button label.
#' @param class CSS classes. Defaults to `"btn btn-default"` to match
#'   [shiny::actionButton()].
#' @param width The width of the button (e.g. `"100\%"`).
#' @param ... Additional HTML attributes.
#'
#' @return A `<button>` tag.
#'
#' @examples
#' mvu_button("Click me") |> on_click("clicked")
#' mvu_button("Delete", class = "btn btn-danger")
#'
#' @export
mvu_button <- function(label, class = "btn btn-default",
                       width = NULL, ...) {
  tags$button(
    style = width_style(width),
    type = "button",
    class = class,
    ...,
    label
  )
}

# -- Text Input -------------------------------------------------------------

#' Text Input
#'
#' Creates a text input with a label, matching the structure of
#' [shiny::textInput()]. Add model binding and event handling via
#' [bind_value()] and [on_input()].
#'
#' @param label Display label for the control, or `NULL` for no
#'   label.
#' @param placeholder A character string giving the user a hint as
#'   to what can be entered into the control.
#' @param width The width of the input (e.g. `"400px"` or
#'   `"100\%"`).
#' @param ... Additional HTML attributes passed to the `<input>` tag.
#'
#' @return A `<div>` containing a label and text input.
#'
#' @examples
#' mvu_text_input("Name") |>
#'   bind_value("model.name") |>
#'   on_input("set_name")
#'
#' mvu_text_input("Search", placeholder = "Type to search...") |>
#'   bind_value("model.search") |>
#'   on_input("set_search", debounce = 300)
#'
#' @export
mvu_text_input <- function(label, placeholder = NULL,
                           width = NULL, ...) {
  input_id <- random_id()
  div(
    class = "form-group",
    style = width_style(width),
    if (!is.null(label)) {
      tags$label(class = "control-label", `for` = input_id, label)
    },
    tags$input(
      id = input_id,
      type = "text",
      class = "form-control",
      placeholder = placeholder,
      ...
    )
  )
}

# -- Numeric Input ----------------------------------------------------------

#' Numeric Input
#'
#' Creates a number input with a label, matching the structure of
#' [shiny::numericInput()]. Add model binding and event handling via
#' [bind_value()] and [on_input_num()].
#'
#' @inheritParams mvu_text_input
#' @param min Minimum allowed value.
#' @param max Maximum allowed value.
#' @param step Interval between valid values.
#'
#' @return A `<div>` containing a label and number input.
#'
#' @examples
#' mvu_numeric_input("Age", min = 0, max = 120) |>
#'   bind_value("model.age") |>
#'   on_input_num("set_age")
#'
#' @export
mvu_numeric_input <- function(label, min = NA, max = NA,
                              step = NA, width = NULL, ...) {
  input_id <- random_id()
  input_tag <- tags$input(
    id = input_id,
    type = "number",
    class = "form-control",
    ...
  )
  if (!is.na(min)) input_tag$attribs$min <- min
  if (!is.na(max)) input_tag$attribs$max <- max
  if (!is.na(step)) input_tag$attribs$step <- step
  div(
    class = "form-group",
    style = width_style(width),
    if (!is.null(label)) {
      tags$label(class = "control-label", `for` = input_id, label)
    },
    input_tag
  )
}

# -- Password Input ---------------------------------------------------------

#' Password Input
#'
#' Creates a password input with a label, matching the structure of
#' [shiny::passwordInput()]. Add model binding and event handling
#' via [bind_value()] and [on_input()].
#'
#' @inheritParams mvu_text_input
#'
#' @return A `<div>` containing a label and password input.
#'
#' @examples
#' mvu_password_input("Password") |>
#'   bind_value("model.password") |>
#'   on_input("set_password", debounce = 300)
#'
#' @export
mvu_password_input <- function(label, placeholder = NULL,
                               width = NULL, ...) {
  input_id <- random_id()
  div(
    class = "form-group",
    style = width_style(width),
    if (!is.null(label)) {
      tags$label(class = "control-label", `for` = input_id, label)
    },
    tags$input(
      id = input_id,
      type = "password",
      class = "form-control",
      placeholder = placeholder,
      ...
    )
  )
}

# -- Textarea Input ---------------------------------------------------------

#' Textarea Input
#'
#' Creates a textarea with a label, matching the structure of
#' [shiny::textAreaInput()]. Add model binding and event handling
#' via [bind_value()] and [on_input()].
#'
#' @inheritParams mvu_text_input
#' @param rows Number of visible text rows.
#'
#' @return A `<div>` containing a label and textarea.
#'
#' @examples
#' mvu_textarea_input("Bio", rows = 4) |>
#'   bind_value("model.bio") |>
#'   on_input("set_bio", debounce = 300)
#'
#' @export
mvu_textarea_input <- function(label, rows = 3, placeholder = NULL,
                               width = NULL, ...) {
  input_id <- random_id()
  div(
    class = "form-group",
    style = width_style(width),
    if (!is.null(label)) {
      tags$label(class = "control-label", `for` = input_id, label)
    },
    tags$textarea(
      id = input_id,
      class = "form-control",
      rows = rows,
      placeholder = placeholder,
      ...
    )
  )
}

# -- Select Input -----------------------------------------------------------

#' Select Input
#'
#' Creates a select dropdown with a label, matching the structure
#' of [shiny::selectInput()] with `selectize = FALSE`. Generates
#' `<option>` tags from a choices vector. Add model binding and
#' event handling via [bind_value()] and [on_input()].
#'
#' @inheritParams mvu_text_input
#' @param choices List of values to select from. If elements are
#'   named, names are displayed to the user.
#' @param multiple If `TRUE`, allow multiple selections.
#' @param size Number of visible items. When set, renders as a list
#'   box instead of a dropdown.
#'
#' @return A `<div>` containing a label and select element.
#'
#' @examples
#' mvu_select_input("Color",
#'   choices = c("Red" = "red", "Blue" = "blue")
#' ) |>
#'   bind_value("model.color") |>
#'   on_input("set_color")
#'
#' @export
mvu_select_input <- function(label, choices, multiple = FALSE,
                             size = NULL, width = NULL, ...) {
  input_id <- random_id()
  select_tag <- do.call(
    tags$select,
    c(
      list(
        id = input_id,
        class = "form-control",
        ...
      ),
      make_options(choices)
    )
  )
  if (multiple) select_tag$attribs$multiple <- "multiple"
  if (!is.null(size)) select_tag$attribs$size <- size
  div(
    class = "form-group",
    style = width_style(width),
    if (!is.null(label)) {
      tags$label(class = "control-label", `for` = input_id, label)
    },
    div(select_tag)
  )
}

# -- Checkbox Input ---------------------------------------------------------

#' Checkbox Input
#'
#' Creates a single checkbox with a label, matching the structure
#' of [shiny::checkboxInput()]. Add model binding and event handling
#' via [bind_checked()] and [on_check()].
#'
#' @inheritParams mvu_text_input
#'
#' @return A `<div>` containing a checkbox and label.
#'
#' @examples
#' mvu_checkbox_input("I agree to the terms") |>
#'   bind_checked("model.agree") |>
#'   on_check("toggle_agree")
#'
#' @export
mvu_checkbox_input <- function(label, width = NULL, ...) {
  div(
    class = "form-group",
    style = width_style(width),
    div(
      class = "checkbox",
      tags$label(
        tags$input(type = "checkbox", ...),
        tags$span(label)
      )
    )
  )
}

# -- Checkbox Group Input ---------------------------------------------------

#' Checkbox Group Input
#'
#' Creates a group of checkboxes, matching the structure of
#' [shiny::checkboxGroupInput()]. Each checkbox dispatches the same
#' message type with the checkbox's value. Your `update` function
#' should toggle the value in and out of a list.
#'
#' Because each checkbox requires its own event wiring, the `msg`
#' and `selected` parameters are included here rather than added
#' via pipes.
#'
#' @inheritParams mvu_text_input
#' @param choices A character vector of values. If named, names are
#'   used as display labels.
#' @param msg Character string specifying the message type dispatched
#'   for each toggle.
#' @param selected A JavaScript expression for the array of selected
#'   values (e.g. `"model.toppings"`). Keeps checkboxes in sync with
#'   the model.
#' @param inline If `TRUE`, render checkboxes horizontally.
#'
#' @return A `<div>` containing a label and checkbox group.
#'
#' @examples
#' mvu_checkbox_group_input("Toppings",
#'   choices = c("Cheese", "Peppers", "Onions"),
#'   msg = "toggle_topping",
#'   selected = "model.toppings"
#' )
#'
#' @export
mvu_checkbox_group_input <- function(label, choices, msg,
                                     selected = NULL,
                                     inline = FALSE,
                                     width = NULL, ...) {
  nms <- if (!is.null(names(choices))) names(choices) else unname(choices)
  vals <- unname(choices)
  boxes <- mapply(function(lbl, val) {
    input_tag <- tags$input(
      type = "checkbox",
      value = val,
      `@change` = send_js(msg, sprintf("'%s'", val)),
      ...
    )
    if (!is.null(selected)) {
      input_tag$attribs[["x-bind:checked"]] <- sprintf(
        "%s.includes('%s')", selected, val
      )
    }
    if (inline) {
      tags$label(
        class = "checkbox-inline",
        input_tag,
        tags$span(lbl)
      )
    } else {
      div(
        class = "checkbox",
        tags$label(input_tag, tags$span(lbl))
      )
    }
  }, nms, vals, SIMPLIFY = FALSE, USE.NAMES = FALSE)
  div(
    class = "form-group",
    style = width_style(width),
    if (!is.null(label)) tags$label(class = "control-label", label),
    div(tagList(boxes))
  )
}

# -- Radio Button Input -----------------------------------------------------

#' Radio Button Input
#'
#' Creates a group of radio buttons, matching the structure of
#' [shiny::radioButtons()]. Each radio dispatches the selected
#' value as a message.
#'
#' Because each radio button requires its own checked expression,
#' the `msg` and `selected` parameters are included here rather
#' than added via pipes.
#'
#' @inheritParams mvu_checkbox_group_input
#' @param selected A JavaScript expression for the currently
#'   selected value (e.g. `"model.pet"`). Keeps the radio buttons
#'   in sync with the model.
#'
#' @return A `<div>` containing a label and radio button group.
#'
#' @examples
#' mvu_radio_input("Pet",
#'   choices = c("Dog" = "dog", "Cat" = "cat"),
#'   msg = "set_pet",
#'   selected = "model.pet"
#' )
#'
#' @export
mvu_radio_input <- function(label, choices, msg,
                            selected = NULL,
                            inline = FALSE,
                            width = NULL, ...) {
  nms <- if (!is.null(names(choices))) names(choices) else unname(choices)
  vals <- unname(choices)
  group_name <- random_id("radio")
  radios <- mapply(function(lbl, val) {
    input_tag <- tags$input(
      type = "radio",
      name = group_name,
      value = val,
      `@change` = send_js(msg, "$event.target.value"),
      ...
    )
    if (!is.null(selected)) {
      input_tag$attribs[["x-bind:checked"]] <- sprintf(
        "%s === '%s'", selected, val
      )
    }
    if (inline) {
      tags$label(
        class = "radio-inline",
        input_tag,
        tags$span(lbl)
      )
    } else {
      div(
        class = "radio",
        tags$label(input_tag, tags$span(lbl))
      )
    }
  }, nms, vals, SIMPLIFY = FALSE, USE.NAMES = FALSE)
  div(
    class = "form-group",
    style = width_style(width),
    if (!is.null(label)) tags$label(class = "control-label", label),
    div(tagList(radios))
  )
}

# -- Slider Input -----------------------------------------------------------

#' Slider Input
#'
#' Creates a range input with a label, matching the purpose of
#' [shiny::sliderInput()]. Add model binding and event handling via
#' [bind_value()] and [on_input_num()].
#'
#' @inheritParams mvu_text_input
#' @param min Minimum value.
#' @param max Maximum value.
#' @param step Step increment. Defaults to `1`.
#'
#' @return A `<div>` containing a label and range input.
#'
#' @examples
#' mvu_slider_input("Volume", min = 0, max = 100) |>
#'   bind_value("model.volume") |>
#'   on_input_num("set_volume")
#'
#' @export
mvu_slider_input <- function(label, min, max, step = 1,
                             width = NULL, ...) {
  input_id <- random_id()
  div(
    class = "form-group",
    style = width_style(width),
    if (!is.null(label)) {
      tags$label(class = "control-label", `for` = input_id, label)
    },
    tags$input(
      id = input_id,
      type = "range",
      class = "form-range",
      min = min,
      max = max,
      step = step,
      ...
    )
  )
}

# -- Date Input -------------------------------------------------------------

#' Date Input
#'
#' Creates a date input with a label. Uses the browser's native
#' date picker. Add model binding and event handling via
#' [bind_value()] and [on_input()].
#'
#' @inheritParams mvu_text_input
#' @param min Minimum date (character string in `"YYYY-MM-DD"`
#'   format).
#' @param max Maximum date (character string in `"YYYY-MM-DD"`
#'   format).
#'
#' @return A `<div>` containing a label and date input.
#'
#' @examples
#' mvu_date_input("Birthday") |>
#'   bind_value("model.birthday") |>
#'   on_input("set_birthday")
#'
#' @export
mvu_date_input <- function(label, min = NULL, max = NULL,
                           width = NULL, ...) {
  input_id <- random_id()
  input_tag <- tags$input(
    id = input_id,
    type = "date",
    class = "form-control",
    ...
  )
  if (!is.null(min)) input_tag$attribs$min <- as.character(min)
  if (!is.null(max)) input_tag$attribs$max <- as.character(max)
  div(
    class = "form-group",
    style = width_style(width),
    if (!is.null(label)) {
      tags$label(class = "control-label", `for` = input_id, label)
    },
    input_tag
  )
}

# -- Date Range Input -------------------------------------------------------

#' Date Range Input
#'
#' Creates two date inputs side by side, matching the purpose of
#' [shiny::dateRangeInput()]. Each date input can be individually
#' wired with [bind_value()] and [on_input()].
#'
#' For simpler cases, consider using two [mvu_date_input()] calls
#' directly.
#'
#' @inheritParams mvu_date_input
#' @param start_label,end_label Labels for the start and end date
#'   inputs.
#'
#' @return A `<div>` containing a group label and two date inputs.
#'
#' @examples
#' mvu_date_range_input("Trip Dates")
#'
#' @export
mvu_date_range_input <- function(label, min = NULL, max = NULL,
                                 start_label = "Start",
                                 end_label = "End",
                                 width = NULL, ...) {
  start_id <- random_id()
  end_id <- random_id()
  start_tag <- tags$input(
    id = start_id,
    type = "date",
    class = "form-control",
    ...
  )
  end_tag <- tags$input(
    id = end_id,
    type = "date",
    class = "form-control",
    ...
  )
  if (!is.null(min)) {
    start_tag$attribs$min <- as.character(min)
    end_tag$attribs$min <- as.character(min)
  }
  if (!is.null(max)) {
    start_tag$attribs$max <- as.character(max)
    end_tag$attribs$max <- as.character(max)
  }
  div(
    class = "form-group",
    style = width_style(width),
    if (!is.null(label)) tags$label(class = "control-label", label),
    div(
      class = "d-flex gap-2",
      div(
        class = "flex-fill",
        tags$label(
          class = "control-label small text-body-secondary",
          `for` = start_id,
          start_label
        ),
        start_tag
      ),
      div(
        class = "flex-fill",
        tags$label(
          class = "control-label small text-body-secondary",
          `for` = end_id,
          end_label
        ),
        end_tag
      )
    )
  )
}

# -- File Input -------------------------------------------------------------

#' File Input
#'
#' Creates a file input with a label, matching the structure of
#' [shiny::fileInput()]. Add event handling via [on_file()].
#'
#' File metadata (name, size, type) is sent to the server. File
#' contents are not sent automatically.
#'
#' @inheritParams mvu_text_input
#' @param accept A character vector of accepted MIME types or file
#'   extensions (e.g. `c(".csv", ".xlsx")` or `"image/*"`).
#' @param multiple If `TRUE`, allow selecting multiple files.
#'
#' @return A `<div>` containing a label and file input.
#'
#' @examples
#' mvu_file_input("Upload CSV", accept = ".csv") |>
#'   on_file("upload")
#'
#' mvu_file_input("Photos",
#'   accept = "image/*", multiple = TRUE
#' ) |>
#'   on_file("upload_photos")
#'
#' @export
mvu_file_input <- function(label, accept = NULL, multiple = FALSE,
                           width = NULL, ...) {
  input_id <- random_id()
  input_tag <- tags$input(
    id = input_id,
    type = "file",
    class = "form-control",
    ...
  )
  if (!is.null(accept)) {
    input_tag$attribs$accept <- paste(accept, collapse = ",")
  }
  if (multiple) input_tag$attribs$multiple <- NA
  div(
    class = "form-group",
    style = width_style(width),
    if (!is.null(label)) {
      tags$label(class = "control-label", `for` = input_id, label)
    },
    input_tag
  )
}

# -- Internal helpers -------------------------------------------------------

send_js <- function(msg, value_expr = NULL) {
  if (is.null(value_expr)) {
    sprintf("send('%s')", msg)
  } else {
    sprintf("send('%s', %s)", msg, value_expr)
  }
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

width_style <- function(width) {
  if (is.null(width)) return(NULL)
  htmltools::css(width = htmltools::validateCssUnit(width))
}

# shinymvu

<!-- badges: start -->
<!-- badges: end -->

> **Note:** shinymvu is in early development. The API may change before
> an initial CRAN release. Feedback and bug reports are very welcome.

## Overview

shinymvu brings the
[Model-View-Update](https://guide.elm-lang.org/architecture/) pattern to
Shiny. All state lives in one list, and the only way to change it is a
pure `update` function that takes the current state and a message and
returns the new state.

An MVU component is a standard Shiny module. You can drop one into a
corner of an existing app to try it out, use several side by side, or
let a single module own your entire app. Start small -- if you like it,
use more.

## Installation

```r
# install.packages("pak")
pak::pak("armcn/shinymvu")
```

## Usage

```r
library(shiny)
library(shinymvu)

init <- function() list(count = 0)

update <- function(model, msg, value) {
  switch(msg,
    increment = list_set(model, count = model$count + 1),
    decrement = list_set(model, count = model$count - 1)
  )
}

ui <- fluidPage(
  mvu_module_ui("counter",
    div(
      mvu_button("-") |> on_click("decrement"),
      tags$span() |> bind_text("model.count"),
      mvu_button("+") |> on_click("increment")
    )
  )
)

server <- function(input, output, session) {
  mvu_module_server("counter", init = init, update = update)
}

shinyApp(ui, server)
```

## Try it in part of an app

Because each MVU component is a Shiny module, it slots into any
existing app without touching the rest of your code.
`mvu_module_server()` returns the model as a `reactiveVal`, so the
rest of your app can read it like any other reactive source:

```r
mod_slider_ui <- function(id) {
  mvu_module_ui(id,
    mvu_slider_input("Rows to show", min = 1, max = 32) |>
      bind_value("model.n") |>
      on_input_num("set_n")
  )
}

mod_slider_server <- function(id) {
  init <- function() list(n = 10)

  update <- function(model, msg, value) {
    switch(msg, set_n = list_set(model, n = value))
  }

  mvu_module_server(id, init = init, update = update)
}

ui <- fluidPage(
  mod_slider_ui("controls"),
  tableOutput("table")
)

server <- function(input, output, session) {
  model <- mod_slider_server("controls")

  output$table <- renderTable({
    head(mtcars, model()$n)
  })
}

shinyApp(ui, server)
```

The MVU module manages the slider state; regular Shiny reads it like
any other reactive value. Add more modules, or let one module own the
whole page -- the API is the same either way.

## Key features

**One source of truth.** All state for a component lives in a single
list. No hunting through server functions to find which `reactiveVal`
holds what.

**Changes are explicit.** State only changes when `update` returns a new
list. Every possible state transition is in one function.

**Testing without Shiny.** Because `update` is pure, you can test your
app logic with plain unit tests:

```r
test_that("increment increases the count", {
  result <- mvu_dispatch(
    init = init,
    update = update,
    messages = list("increment", "increment", "increment")
  )
  expect_equal(result$count, 3)
})
```

**Exhaustive message handling.** `mvu_enum()` + `match_enum()` forces
you to handle every message type. Add a new message and forget to handle
it? You get an error, not a silent bug.

**Time-travel debugging.** Set `debug = TRUE` in `mvu_module_server()`
to get a built-in debugger overlay that records every state transition,
lets you step back and forward through history, inspect the model, and
import/export sessions.

**Inputs.** Structural helpers matching Shiny's native inputs, wired
to the MVU loop with pipe helpers:

```r
mvu_button("Submit") |> on_click("submit")
mvu_text_input("Name") |> bind_value("model.name") |> on_input("set_name")
mvu_select_input("Color", choices = c("Red", "Blue")) |>
  bind_value("model.color") |> on_input("set_color")
```

## Examples

Run the bundled counter example:

```r
shiny::runApp(system.file("examples/counter", package = "shinymvu"))
```

---

# Guide

This guide teaches the fundamentals of the Model-View-Update pattern
and shows you how to build interactive apps with shinymvu. By the end
you will be able to create well-structured Shiny apps with shinymvu,
and understand the core ideas that make this architecture work. If you
are on the fence, I can safely guarantee that if you give shinymvu a
shot and actually make a project with it, you will end up writing better
Shiny code. The ideas transfer pretty easily!

## The Model-View-Update Architecture

The Model-View-Update architecture is a pattern for building interactive
programs. It was created by Evan Czaplicki for the Elm programming language,
and has since inspired libraries in many other languages (Redux for
JavaScript, Jetpack Compose for Android, SwiftUI for iOS).

Rather than someone inventing it for Shiny, the pattern has proven itself
across many platforms. The core idea is simple: your program always breaks
into three parts:

- **Model** -- the state of your application
- **Update** -- a way to update your state based on messages
- **View** -- a way to turn your state into HTML

shinymvu programs always look something like this:

```r
# Model
init <- function() list(...)

# Update
update <- function(model, msg, value) { ... }

# View (the UI)
ui <- fluidPage(
  mvu_module_ui("app", ...)
)

# Wiring
server <- function(input, output, session) {
  mvu_module_server("app", init = init, update = update)
}
```

The next few examples are going to show how to use this pattern for user
input, like buttons and text fields. It will make this much more concrete!

## Buttons

Our first example is a counter that can be incremented or decremented.
Here is the full program:

```r
library(shiny)
library(shinymvu)


# MODEL

init <- function() list(count = 0)


# UPDATE

update <- function(model, msg, value) {
  switch(msg,
    increment = list_set(model, count = model$count + 1),
    decrement = list_set(model, count = model$count - 1)
  )
}


# VIEW

ui <- fluidPage(
  mvu_module_ui("counter",
    div(
      mvu_button("-") |> on_click("decrement"),
      tags$span() |> bind_text("model.count"),
      mvu_button("+") |> on_click("increment")
    )
  )
)

server <- function(input, output, session) {
  mvu_module_server("counter", init = init, update = update)
}

shinyApp(ui, server)
```

Now that you have looked at the code a little bit, you may have some
questions. What does `mvu_module_ui` do? How do the different parts fit
together? Let's go through the code and talk about it.

### Model

Data modeling is extremely important in shinymvu. The point of the model
is to capture all the details about your application as data.

To make a counter, we need to keep track of a number that is going up
and down. That means our model is really small this time:

```r
init <- function() list(count = 0)
```

`init` is a function that returns the initial model. The model is a list
with a single field `count`, starting at zero. It will go up and down as
people press different buttons.

### Update

The `update` function describes how our model will change over time.

```r
update <- function(model, msg, value) {
  switch(msg,
    increment = list_set(model, count = model$count + 1),
    decrement = list_set(model, count = model$count - 1)
  )
}
```

It takes three arguments:

1. `model` -- the current state
2. `msg` -- a string describing what happened (e.g. `"increment"`)
3. `value` -- optional data attached to the message (not used here)

`list_set` returns a new copy of the list with the specified fields
changed. The original model is never modified. If you get an
`"increment"` message, you increment the count. If you get a
`"decrement"` message, you decrement it.

### View

We have a model, but how do we show it on screen? That is the role of
the UI:

```r
ui <- fluidPage(
  mvu_module_ui("counter",
    div(
      mvu_button("-") |> on_click("decrement"),
      tags$span() |> bind_text("model.count"),
      mvu_button("+") |> on_click("increment")
    )
  )
)
```

`mvu_module_ui` creates the container for your MVU component. Inside it,
we place a decrement button, the current count, and an increment button.

Notice that each `mvu_button` is piped into `on_click()`. This is
saying: "when someone clicks this button, send this message." So the
plus button sends `"increment"` and the minus button sends
`"decrement"`. Where do these messages go? To the `update` function!

`bind_text("model.count")` is how you display model values. It means
"show the `count` field from the current model here."

### Wiring It Together

The server function connects everything:

```r
server <- function(input, output, session) {
  mvu_module_server("counter", init = init, update = update)
}
```

`mvu_module_server` starts the MVU loop: it creates the initial model by
calling `init`, renders it on screen, and waits for messages. When a
message arrives, it calls `update` to get a new model, renders that, and
repeats.

### How it fits together

Now that you have seen all the parts, it may be easier to see how they
fit together:

1. Render the initial model on screen.
2. Wait for user input.
3. Send a message to `update`.
4. Produce a new model.
5. Show the new model on screen.
6. Repeat!

This is the essence of the Model-View-Update architecture. Every example
from now on will be a slight variation on this basic pattern.

**Exercise:** Add a button to reset the counter to zero. You will need
to:

1. Add a `"reset"` branch in the `update` function
2. Add a `mvu_button("Reset") |> on_click("reset")` in the UI

If that goes well, try adding a button that increments by 10.

## Text Fields

We are about to create a simple app that reverses the contents of a
text field.

```r
library(shiny)
library(shinymvu)


# MODEL

init <- function() list(content = "")


# UPDATE

update <- function(model, msg, value) {
  switch(msg,
    change = list_set(model, content = value)
  )
}


# VIEW

ui <- fluidPage(
  mvu_module_ui("app",
    div(
      mvu_text_input("Text to reverse") |>
        bind_value("model.content") |>
        on_input("change"),
      tags$div() |>
        bind_text("model.content.split('').reverse().join('')")
    )
  )
)

server <- function(input, output, session) {
  mvu_module_server("app", init = init, update = update)
}

shinyApp(ui, server)
```

This code is a slight variant of the previous example. You set up a
model. You define some messages. You say how to `update`. You make your
view. The difference is just in how we filled this skeleton in.

### Model

We know we have to keep track of whatever the user has typed into the
text field. We need that information so we can show the reversed text.
So we go with:

```r
init <- function() list(content = "")
```

I always try to start with a minimal model, maybe with just one field.
I then attempt to write the view and `update` function. That often
reveals that I need to add more to my model. Building the model
gradually like this means I can have a working program through the
development process. It may not have all the features yet, but it is
getting there!

### Update

There is only one kind of message in this program, so our `update` only
has to handle one case:

```r
update <- function(model, msg, value) {
  switch(msg,
    change = list_set(model, content = value)
  )
}
```

When we receive a `"change"` message, we update the `content` of our
model with the new text. `value` contains the current text from the
input field.

### View

```r
mvu_text_input("Text to reverse") |>
  bind_value("model.content") |>
  on_input("change")
```

`mvu_text_input` creates a text input with a label. Piping it into
`bind_value("model.content")` keeps the input in sync with the model.
Piping into `on_input("change")` sends a `"change"` message every time
the user types.

The reversed text is displayed using a JavaScript expression in
`bind_text()`. Because the view is rendered in the browser, you have
access to JavaScript string methods for display logic.

**Exercise:** Show the length of the content below the reversed text.
You can use `bind_text("model.content.length")` on a `tags$span()`.

## Forms

Now we will make a rudimentary form. It has a field for your name, a
field for your password, and a field to verify that password. We will
also do some simple validation to check if the passwords match.

```r
library(shiny)
library(shinymvu)


# MODEL

init <- function() {
  list(name = "", password = "", password_again = "")
}


# UPDATE

update <- function(model, msg, value) {
  switch(msg,
    set_name           = list_set(model, name = value),
    set_password       = list_set(model, password = value),
    set_password_again = list_set(model, password_again = value)
  )
}


# VIEW

ui <- fluidPage(
  mvu_module_ui("app",
    div(
      mvu_text_input("Name") |>
        bind_value("model.name") |>
        on_input("set_name"),
      mvu_password_input("Password") |>
        bind_value("model.password") |>
        on_input("set_password"),
      mvu_password_input("Re-enter Password") |>
        bind_value("model.password_again") |>
        on_input("set_password_again"),
      tags$div() |>
        bind_show("model.password.length > 0") |>
        bind_style(paste0(
          "model.password === model.password_again",
          " ? 'color: green' : 'color: red'"
        )) |>
        bind_text(paste0(
          "model.password === model.password_again",
          " ? 'OK' : 'Passwords do not match!'"
        ))
    )
  )
)

server <- function(input, output, session) {
  mvu_module_server("app", init = init, update = update)
}

shinyApp(ui, server)
```

This is pretty similar to our text field example but with more fields.

### Model

We know there are going to be three text fields, so let's just go with
that:

```r
init <- function() {
  list(name = "", password = "", password_again = "")
}
```

### Update

We need to be able to change all three fields, so we need a message for
each:

```r
update <- function(model, msg, value) {
  switch(msg,
    set_name           = list_set(model, name = value),
    set_password       = list_set(model, password = value),
    set_password_again = list_set(model, password_again = value)
  )
}
```

Each branch uses `list_set` to update the appropriate field.

### View

The validation logic is handled entirely in the view using
`bind_style()` and `bind_text()` expressions. The model holds the raw
data; the view decides how to present it. This is a good pattern --
keep your model simple and derive display logic in the view.

**Exercise:** Try adding these features to the validation display:

- Check that the password is longer than 8 characters.
- Make sure the password contains at least one number.

## Message Types

As your application grows, you will want to make sure every message is
handled. shinymvu provides `mvu_enum()` and `match_enum()` for this.

### The Problem

With `switch`, forgetting to handle a message is a silent bug. If you
add a new message type and forget to update your `switch`, R returns
`NULL` and your model silently becomes `NULL`. Good luck debugging that.

### The Solution

`mvu_enum()` creates a factory function that validates message types.
`match_enum()` forces you to handle every one of them:

```r
Msg <- mvu_enum(c("increment", "decrement", "reset"))

update <- function(model, msg, value) {
  match_enum(Msg(msg),
    "increment" ~ list_set(model, count = model$count + 1),
    "decrement" ~ list_set(model, count = model$count - 1),
    "reset"     ~ list_set(model, count = 0)
  )
}
```

Now if you add `"save"` to your enum and forget to handle it in
`match_enum`, you get an immediate error: `Unhandled messages: save`.
If the client sends a message that is not in the enum, you get:
`Unknown message 'typo'`.

Pass the enum factory to `mvu_module_server` using the `msg` parameter,
and incoming messages are validated automatically:

```r
mvu_module_server("counter",
  init = init, update = update, msg = Msg
)
```

### Default Cases

Sometimes you want to handle a few cases explicitly and ignore the
rest. Use `.default` for that:

```r
match_enum(Msg(msg),
  "reset" ~ list_set(model, count = 0),
  .default = model
)
```

## Commands and Subscriptions

So far all of our programs have been self-contained. The `update`
function takes the current model and a message and returns a new model.
But what about talking to servers? Running expensive computations?
Responding to external data?

### The Basic Pattern

So far, the pattern has been:

1. User interacts with the view.
2. A message is sent to `update`.
3. `update` returns a new model.
4. The view re-renders.

This works great for pure state transitions. But sometimes `update`
needs to *describe* a side effect that the runtime should carry out.
That is where commands come in.

### Commands

A command is a description of work the runtime should do after a state
transition. The `update` function stays pure -- it *describes* the
effect as data rather than performing it.

Use `mvu_result()` to return both a new model and one or more effects:

```r
update <- function(model, msg, value) {
  switch(msg,
    save = mvu_result(
      model,
      effect_notify("Saved!", type = "message")
    ),
    model
  )
}
```

There are several built-in effect types:

```r
# Show a Shiny notification
effect_notify("Done!", type = "message", duration = 3)

# Send a custom message to JavaScript
effect_send("channel_name", list(key = "value"))

# Arbitrary side effect (escape hatch)
effect_custom(function(session) {
  shiny::showModal(shiny::modalDialog("Hello"))
})
```

### Async Commands

The most powerful effect type is `effect_cmd()`. It runs a function in
a background R process and dispatches the result back as a new message
when it completes. Your app stays interactive while the work runs.

```r
library(shiny)
library(shinymvu)
future::plan(future::multisession)

expensive_work <- function() {
  Sys.sleep(5)
  head(mtcars, 10)
}

init <- function() list(loading = FALSE, data = NULL)

update <- function(model, msg, value) {
  switch(msg,
    fetch = mvu_result(
      list_set(model, loading = TRUE),
      effect_cmd(expensive_work, msg = "fetched")
    ),
    fetched = list_set(model, loading = FALSE, data = value)
  )
}
```

When the user triggers `"fetch"`:

1. The model immediately updates to `loading = TRUE`.
2. The runtime starts `expensive_work` in a background process.
3. The app stays interactive -- buttons work, other modules respond.
4. When the work finishes, a `"fetched"` message arrives with the
   result.
5. The model updates to `loading = FALSE` with the new data.

You can also handle errors with `error_msg`:

```r
effect_cmd(
  fn = function() { stop("network error") },
  msg = "data_loaded",
  error_msg = "load_failed"
)
```

`effect_cmd` requires the `future` package. Install it with
`install.packages("future")` and set a plan at the top of your app with
`future::plan(future::multisession)`.

### Subscriptions

Commands are for one-off tasks. But what about ongoing events from
outside your component? That is where subscriptions come in.

A subscription connects an external reactive source to your MVU loop.
When the reactive changes, a message is dispatched through `update`,
just like a button click.

```r
mvu_module_server("dashboard",
  init = init, update = update,
  subscriptions = function() list(
    filters_changed = filter_model
  )
)
```

`subscriptions` is a function that returns a named list. The names are
message types; the values are reactive expressions. When
`filter_model()` changes, the runtime dispatches `"filters_changed"`
with the new value.

This is how MVU modules communicate. Module A returns its model (a
`reactiveVal`). Module B subscribes to it:

```r
server <- function(input, output, session) {
  filter_model <- filter_server("filters")
  table_server("table", filter_model)
}

table_server <- function(id, filter_model) {
  mvu_module_server(id,
    init = table_init, update = table_update,
    subscriptions = function() list(
      filters_changed = filter_model
    )
  )
}
```

Subscriptions work with any Shiny reactive -- `reactiveVal`,
`reactive()`, `reactiveTimer()`, `reactivePoll()`, or even
`input$some_id` wrapped in a `reactive()`.

## The View Layer

shinymvu uses [Alpine.js](https://alpinejs.dev/) to render the model in
the browser. The server pushes the model as JSON, and Alpine.js updates
the DOM reactively. You write standard HTML with pipe helpers that add
the right attributes.

### Displaying Values

Use `bind_text()` to display model fields as text:

```r
tags$span() |> bind_text("model.count")
tags$h1() |> bind_text("model.title")
tags$p() |> bind_text("'Hello, ' + model.name + '!'")
```

The value is a JavaScript expression. You have access to the full model
object and standard JavaScript operators.

### Conditional Display

Use `bind_show()` to show or hide elements:

```r
tags$div("Loading...") |> bind_show("model.loading")
tags$div("Items found") |> bind_show("model.items.length > 0")
```

### Lists

Use `bind_for()` inside a `tags$template` to render lists:

```r
tags$ul(
  tags$template(
    tags$li() |> bind_text("item.name")
  ) |> bind_for("item", "model.items")
)
```

### Dynamic Attributes

Use `bind_class()` and `bind_style()` to set attributes based on the
model:

```r
tags$div() |>
  bind_class("{ 'text-danger': model.hasError }") |>
  bind_style("'font-size: ' + model.fontSize + 'px'")
```

### Sending Messages

The pipe helpers use a `send()` function internally to dispatch
messages. All `on_*` event helpers build on it:

```r
mvu_button("Delete") |> on_click("delete")
mvu_text_input("Search") |> bind_value("model.query") |> on_input("search")
```

### to_frontend

By default, the entire model is sent to the browser as JSON. If you
need to derive display values, add computed fields, or reshape data for
the view, use the `to_frontend` function:

```r
to_frontend <- function(model) {
  list(
    count = model$count,
    is_even = model$count %% 2 == 0,
    display = paste("Count:", model$count)
  )
}

mvu_module_server("app",
  init = init, update = update, to_frontend = to_frontend
)
```

The view sees `model.is_even` and `model.display` even though those
fields do not exist in the model itself. This keeps your model lean and
your display logic explicit.

## Inputs

shinymvu provides structural helpers that match Shiny's native inputs.
Each helper creates the HTML markup (label, form element, styling), and
you add MVU wiring with pipe helpers:

```r
# Buttons
mvu_button("Click me") |> on_click("clicked")
mvu_button("Save", class = "btn btn-primary") |> on_click("save")

# Text
mvu_text_input("Name") |> bind_value("model.name") |> on_input("set_name")
mvu_password_input("Password") |>
  bind_value("model.pw") |> on_input("set_pw")
mvu_textarea_input("Bio") |> bind_value("model.bio") |> on_input("set_bio")

# Selection
mvu_select_input("Color", choices = c("Red", "Blue")) |>
  bind_value("model.color") |> on_input("set_color")
mvu_radio_input("Size", choices = c("S", "M", "L"),
  msg = "set_size", selected = "model.size")
mvu_checkbox_input("Agree") |>
  bind_checked("model.agree") |> on_check("toggle_agree")

# Numeric
mvu_numeric_input("Count") |>
  bind_value("model.count") |> on_input_num("set_count")
mvu_slider_input("Volume", min = 0, max = 100) |>
  bind_value("model.volume") |> on_input_num("set_vol")

# Date
mvu_date_input("Start") |>
  bind_value("model.start") |> on_input("set_start")
```

`bind_value()` (or `bind_checked()`) keeps the input in sync with the
model. The `on_*` helper specifies which message to send when the user
interacts with the input. Radio and checkbox group inputs include `msg`
and `selected` directly because each choice needs its own wiring.

## Immutable Updates

In the Model-View-Update architecture, you never modify the model in
place. You always return a new copy. shinymvu provides helper functions
for this:

```r
model <- list(count = 0, name = "Alice")

# Update a field
list_set(model, count = 1)
# => list(count = 1, name = "Alice")

# Update multiple fields
list_set(model, count = 5, name = "Bob")
# => list(count = 5, name = "Bob")
```

For nested models:

```r
model <- list(user = list(profile = list(name = "Alice")))

# Get a nested value
list_get_in(model, c("user", "profile", "name"))
# => "Alice"

# Set a nested value
list_set_in(model, c("user", "profile", "name"), "Bob")

# Transform a nested value
list_update_in(model, c("user", "profile", "name"), toupper)
```

These functions always return a new list. The original is never changed.

## Testing

Because `update` is a pure function, you can test your entire
application logic with plain unit tests. No Shiny session needed.

### mvu_dispatch

`mvu_dispatch()` simulates the MVU runtime loop by applying a sequence
of messages:

```r
test_that("three increments produce count of 3", {
  result <- mvu_dispatch(
    init = function() list(count = 0),
    update = function(model, msg, value) {
      switch(msg,
        increment = list_set(model, count = model$count + 1)
      )
    },
    messages = list("increment", "increment", "increment")
  )
  expect_equal(result$count, 3)
})
```

### Messages With Values

Pass a list with `$type` and `$value` for messages that carry data:

```r
test_that("set_name updates the name", {
  result <- mvu_dispatch(
    init = function() list(name = ""),
    update = function(model, msg, value) {
      switch(msg,
        set_name = list_set(model, name = value)
      )
    },
    messages = list(list(type = "set_name", value = "Alice"))
  )
  expect_equal(result$name, "Alice")
})
```

### Collecting Effects

Use `.collect_effects = TRUE` to inspect which effects your update
function would have triggered:

```r
test_that("save produces a notification", {
  result <- mvu_dispatch(
    init = function() list(count = 0),
    update = function(model, msg, value) {
      switch(msg,
        save = mvu_result(model, effect_notify("Saved!"))
      )
    },
    messages = list("save"),
    .collect_effects = TRUE
  )
  expect_length(result$effects, 1)
})
```

## Debugging

Set `debug = TRUE` in `mvu_module_server()` to enable the built-in
time-travel debugger:

```r
mvu_module_server("app",
  init = init, update = update, debug = TRUE
)
```

This adds a small overlay to your app that:

- **Records every state transition.** Every message, the model before,
  and the model after are logged.
- **Steps backward and forward.** Click through the history to see
  exactly how each message changed the model.
- **Inspects the model.** A tree view shows the current model state at
  any point in the history.
- **Imports and exports sessions.** Save the message history to a JSON
  file and replay it later.

The debugger works with any app or module. Subscription messages and
async command results appear in the timeline alongside regular UI
messages.

## Using Modules

Every MVU component in shinymvu is a standard Shiny module. This means
you can:

- Drop one into an existing Shiny app without changing anything else.
- Use several MVU modules side by side.
- Mix MVU modules with regular Shiny modules.
- Let a single MVU module own the entire page.

### Defining a Module

Wrap your init, update, and UI in standard Shiny module functions:

```r
counter_ui <- function(id) {
  mvu_module_ui(id,
    div(
      mvu_button("-") |> on_click("decrement"),
      tags$span() |> bind_text("model.count"),
      mvu_button("+") |> on_click("increment")
    )
  )
}

counter_server <- function(id) {
  mvu_module_server(id,
    init = function() list(count = 0),
    update = function(model, msg, value) {
      switch(msg,
        increment = list_set(model, count = model$count + 1),
        decrement = list_set(model, count = model$count - 1)
      )
    }
  )
}
```

### Using a Module

```r
ui <- fluidPage(
  counter_ui("my_counter")
)

server <- function(input, output, session) {
  model <- counter_server("my_counter")
}
```

### Reading Module State

`mvu_module_server()` returns the model as a `reactiveVal`. The rest of
your app can read it like any other reactive source:

```r
server <- function(input, output, session) {
  model <- counter_server("my_counter")

  output$message <- renderText({
    paste("The count is", model()$count)
  })
}
```

### Module Communication

Use subscriptions to connect modules. Module A returns its model;
module B subscribes to it:

```r
server <- function(input, output, session) {
  filter_model <- filter_server("filters")

  dashboard_server("dashboard", filter_model)
}

dashboard_server <- function(id, filter_model) {
  mvu_module_server(id,
    init = init, update = update,
    subscriptions = function() list(
      filters_changed = filter_model
    )
  )
}
```

When the filter model changes, `"filters_changed"` is dispatched
through the dashboard's `update` function with the new filter values.

---

## Getting help

If you encounter a bug, please file an issue with a minimal
reproducible example on
[GitHub](https://github.com/armcn/shinymvu/issues).

## License

MIT

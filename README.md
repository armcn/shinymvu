# shinymvu

<!-- badges: start -->
<!-- badges: end -->

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
      mvu_button("-", msg = "decrement"),
      tags$span(`x-text` = "model.count"),
      mvu_button("+", msg = "increment")
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
    mvu_slider_input(
      "Rows to show", 
      min = 1, 
      max = 32,
      msg = "set_n", 
      value = "model.n"
    )
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

**Inputs.** Wrappers for standard Shiny inputs that dispatch MVU
messages instead of using `inputId`:

```r
mvu_button("Submit", msg = "submit")
mvu_text_input("Name", msg = "set_name", value = "model.name")
mvu_select_input("Color", choices = c("Red", "Blue"), msg = "set_color")
mvu_slider_input("Volume", min = 0, max = 100, msg = "set_volume")
```

## Examples

Run the bundled examples:

```r
shiny::runApp(system.file("examples/counter", package = "shinymvu"))
shiny::runApp(system.file("examples/todomvc", package = "shinymvu"))
```

## Getting help

If you encounter a bug, please file an issue with a minimal
reproducible example on
[GitHub](https://github.com/armcn/shinymvu/issues).

## License

MIT

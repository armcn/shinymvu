# shinymvu

<!-- badges: start -->
<!-- badges: end -->

## Overview

shinymvu implements the [Model-View-Update](https://guide.elm-lang.org/architecture/)
pattern for Shiny. Your entire component state lives in one list, and
there is exactly one way to change it -- a pure `update` function that
takes the current state and a message and returns the new state. No
`reactiveVal`, no `observeEvent`, no side effects.

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

## Key features

**One source of truth.** All state for a component lives in a single
list. No hunting through server functions to find which `reactiveVal`
holds what.

**Changes are explicit.** State only changes when `update` returns a new
list. Every possible state transition is in one function. No reactive
chain mysteries.

**Testing without Shiny.** Because `update` is a pure function, you can
test your entire app logic with plain unit tests:

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

**Time-travel debugging.** Set `debug = TRUE` to get a built-in debugger
overlay that records every state transition, lets you step back and
forward through history, inspect the model, and import/export sessions.

**Inputs.** The package provides wrappers for standard Shiny inputs that
dispatch MVU messages instead of using `inputId`:

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

# shinymvu

The Elm Architecture for Shiny Applications.

**shinymvu** implements the Model-View-Update (MVU) pattern for
[Shiny](https://shiny.posit.co/) using [Alpine.js](https://alpinejs.dev/)
as the client-side rendering engine. Define three pure functions -- `init`,
`update`, `view` -- and the package handles all the reactive plumbing.

## Installation

```r
# install.packages("pak")
pak::pak("shinymvu/shinymvu")
```

## Quick Start: Counter

The classic Elm counter, translated to R.

```r
library(shiny)
library(shinymvu)

# Define valid messages
Msg <- mvu_enum(c("increment", "decrement", "reset"))

# Initial model
init <- function() list(count = 0)

# Pure update function
update <- function(model, msg, value = NULL) {
  match_enum(Msg(msg),
    "increment" ~ list_set(model, count = model$count + 1),
    "decrement" ~ list_set(model, count = model$count - 1),
    "reset"     ~ list_set(model, count = 0)
  )
}

# Transform model for the client
view <- function(model) {
  list(
    count = model$count,
    is_zero = model$count == 0
  )
}

# UI with Alpine.js directives
ui <- mvu_page(
  div(class = "container py-5 text-center",
    tags$h1("Counter", class = "mb-4"),
    div(class = "d-flex gap-3 justify-content-center align-items-center",
      mvu_button("\u2212", msg = "decrement",
        class = "btn btn-outline-primary btn-lg"),
      tags$span(`x-text` = "model.count", class = "display-4 mx-3"),
      mvu_button("+", msg = "increment",
        class = "btn btn-primary btn-lg")
    ),
    div(class = "mt-3",
      tags$button(
        class = "btn btn-outline-secondary",
        `x-show` = "!model.is_zero",
        `@click` = "send('reset')",
        "Reset"
      )
    )
  )
)

server <- function(input, output, session) {
  mvu_server(
    init = init, update = update, view = view,
    input = input, output = output, session = session
  )
}

shinyApp(ui, server)
```

## How It Works

```
Browser (Alpine.js)                    R Server (shinymvu)
───────────────────                    ────────────────────
  @click  ──────────►  send(type, value)
                       Shiny.setInputValue ──────► mvu_server dispatcher
                                                        │
                                              update(model, msg, value)
                                                        │
                                              view(new_model)
  Alpine patches DOM  ◄───────────────────  sendCustomMessage
  model = data
```

1. User clicks a button wired with `@click="send('increment')"`
2. Alpine.js calls `Shiny.setInputValue` to send the message to R
3. `mvu_server` receives the message and calls your `update` function
4. The new model is passed through `view` and sent back to the browser
5. Alpine.js reactively updates the DOM

## Core API

| Function | Purpose |
|---|---|
| `mvu_page()` | UI wrapper with Alpine.js CDN and bridge script |
| `mvu_server()` | Server-side reactive runtime loop |
| `mvu_module_ui()` / `mvu_module_server()` | Shiny module support |
| `mvu_enum()` | Define valid message types |
| `match_enum()` | Exhaustive pattern matching on messages |
| `list_set()` | Immutable model updates |
| `mvu_button()` | Message-dispatching button |
| `mvu_select()` | Message-dispatching select input |
| `mvu_checkbox()` | Message-dispatching checkbox |
| `mvu_slider()` | Message-dispatching range slider |
| `mvu_dispatch()` | Test helper for pure update functions |

## Testing

Since `update` is a pure function, testing is simple and does not require a
running Shiny session:

```r
test_that("increment and decrement work correctly", {
  result <- mvu_dispatch(
    init = function() list(count = 0),
    update = update,
    messages = list("increment", "increment", "decrement")
  )
  expect_equal(result$count, 1)
})
```

## Why No Text Input Helper?

Text inputs are deliberately excluded from the input helpers. With the
server round-trip on every keystroke, text input latency would be
unacceptable. Instead, use Alpine.js `x-model` to bind text inputs to
local Alpine state, then dispatch the full value on form submission or
blur.

## License

MIT

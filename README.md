# shinymvu

Predictable state management for Shiny.

If you've built complex Shiny apps, you know the pain: dozens of
`reactiveVal`s scattered across server functions, `observeEvent`s
triggering other `observeEvent`s, and the creeping dread that changing
one thing will break three others. The state lives everywhere and flows
in every direction. Debugging means tracing invisible chains of
reactivity with `browser()` calls and prayer.

**shinymvu** replaces that with a single idea: your entire component
state is one list, and there is exactly one way to change it. This
pattern -- called Model-View-Update (MVU), originally from
[The Elm Architecture](https://guide.elm-lang.org/architecture/) --
has been battle-tested in frontend frameworks for years. shinymvu brings
it to Shiny.

## The pattern in 30 seconds

You write two things:

1. **`init`** -- a function that returns your starting state (a plain list)
2. **`update`** -- a function that takes the current state + a message
   and returns the new state

That's it. No `reactiveVal`, no `observeEvent`, no side effects.
The package handles the plumbing.

```r
counter_msg <- mvu_enum(c("increment", "decrement", "reset"))

counter_init <- function() list(count = 0)

counter_update <- function(model, msg, value = NULL) {
  match_enum(msg,
    "increment" ~ list_set(model, count = model$count + 1),
    "decrement" ~ list_set(model, count = model$count - 1),
    "reset"     ~ list_set(model, count = 0)
  )
}
```

The UI uses [Alpine.js](https://alpinejs.dev/) directives to read state
and send messages -- no `renderUI`, no `uiOutput`:

```r
counter_ui <- function(id) {
  mvu_module_ui(id,
    div(class = "text-center py-4",
      mvu_button("\u2212", msg = "decrement"),
      tags$span(`x-text` = "model.count", class = "display-4 mx-3"),
      mvu_button("+", msg = "increment"),
      div(class = "mt-3",
        mvu_button("Reset", msg = "reset", `x-show` = "model.count !== 0")
      )
    )
  )
}

counter_server <- function(id) {
  mvu_module_server(id,
    init = counter_init,
    update = counter_update,
    msg = counter_msg
  )
}
```

## Why this is better for complex apps

**One source of truth.** All state for a component lives in a single
list. No hunting through server functions to find which `reactiveVal`
holds what.

**Changes are explicit.** State only changes when `update` returns a new
list. Every possible state transition is in one function, with every
message type enumerated. No reactive chain mysteries.

**Testing without Shiny.** Because `update` is a pure function (data
in, data out), you can test your entire app logic with plain unit tests:

```r
test_that("reset zeroes the count", {
  result <- mvu_dispatch(
    init = counter_init,
    update = counter_update,
    messages = list("increment", "increment", "reset")
  )
  expect_equal(result$count, 0)
})
```

No `testServer`, no mock sessions, no flaky async timing.

**Exhaustive message handling.** `mvu_enum` + `match_enum` forces you to
handle every message type. Add a new message and forget to handle it?
You get an error, not a silent bug.

## How it works

```
Browser (Alpine.js)                    R Server
───────────────────                    ────────
  button click ──────────►  send("increment")
                            Shiny.setInputValue ──────► update(model, msg, value)
                                                              │
                                                        new model
  Alpine patches DOM  ◄───────────────────────────  sendCustomMessage
```

The browser side uses Alpine.js for lightweight client-side reactivity.
When the user clicks a button, Alpine sends a message to R. The server
runs your `update` function, gets the new state, and pushes it back.
Alpine reactively updates the DOM. One direction, every time.

## Installation

```r
# install.packages("pak")
pak::pak("shinymvu/shinymvu")
```

## Inputs

The package provides wrappers for every standard Shiny input. They
generate Bootstrap-styled HTML wired to dispatch MVU messages via
Alpine.js -- no `inputId`, no `input$x`, just messages:

```r
mvu_text_input("Name", msg = "set_name", value = "model.name")
mvu_select_input("Color", choices = c("Red" = "red", "Blue" = "blue"),
  msg = "set_color", selected = "model.color")
mvu_checkbox_input("I agree", msg = "toggle_agree", value = "model.agree")
mvu_slider_input("Volume", min = 0, max = 100, msg = "set_volume",
  value = "model.volume")
```

See the full gallery with `shinymvu::run_gallery()` or browse
`inst/examples/gallery/app.R`.

For custom markup, use the low-level event handlers with any
`tags$` element:

```r
tags$button(!!!on_click("increment"), "Custom button")
tags$select(!!!on_input("set_color"), tags$option(value = "red", "Red"))
tags$input(type = "checkbox", !!!on_check("toggle"))
tags$input(type = "range", !!!on_input_num("set_volume"))
```

## API reference

| | |
|---|---|
| **Core** | |
| `mvu_module_ui()` / `mvu_module_server()` | Shiny module wrappers |
| `mvu_page()` / `mvu_server()` | Standalone (non-module) app |
| `mvu_enum()` / `match_enum()` | Message types + exhaustive matching |
| `list_set()` | Immutable list updates |
| `mvu_dispatch()` | Test helper for pure update functions |
| **Inputs** | |
| `mvu_button()` | Action button |
| `mvu_text_input()` / `mvu_password_input()` / `mvu_textarea_input()` | Text inputs |
| `mvu_numeric_input()` / `mvu_slider_input()` | Numeric inputs |
| `mvu_select_input()` | Dropdown select |
| `mvu_checkbox_input()` / `mvu_checkbox_group_input()` | Checkboxes |
| `mvu_radio_input()` | Radio buttons |
| `mvu_date_input()` / `mvu_date_range_input()` | Date pickers |
| `mvu_file_input()` | File upload |
| **Event handlers** | |
| `on_click()` / `on_click_expr()` | Click events (for custom markup via `!!!`) |
| `on_input()` / `on_input_num()` | Value change events |
| `on_check()` | Checkbox toggle events |

## License

MIT

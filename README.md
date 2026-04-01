
<!-- README.md is generated from README.Rmd. Please edit that file -->

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
let a single module own your entire app. Start small – if you like it,
use more.

## Installation

``` r
# install.packages("pak")
pak::pak("armcn/shinymvu")
```

## Usage

``` r
library(shiny)
library(shinymvu)

init <- function() list(count = 0)

update <- function(model, msg, value) {
  switch(
    msg,
    increment = list_set(model, count = model$count + 1),
    decrement = list_set(model, count = model$count - 1)
  )
}

ui <- fluidPage(
  mvu_module_ui(
    id = "counter",
    div(
      mvu_action_button("-") |> on_click("decrement"),
      tags$span() |> bind_text("model.count"),
      mvu_action_button("+") |> on_click("increment")
    )
  )
)

server <- function(input, output, session) {
  mvu_module_server(
    id = "counter", 
    init = init, 
    update = update
  )
}

shinyApp(ui, server)
```

library(shiny)
library(shinymvu)

# -- Messages ---------------------------------------------------------------
Msg <- mvu_enum(c("increment", "decrement", "reset"))

# -- Model -------------------------------------------------------------------
init <- function() {
  list(count = 0)
}

# -- Update ------------------------------------------------------------------
update <- function(model, msg, value = NULL) {
  match_enum(Msg(msg),
    "increment" ~ list_set(model, count = model$count + 1),
    "decrement" ~ list_set(model, count = model$count - 1),
    "reset"     ~ list_set(model, count = 0)
  )
}

# -- View --------------------------------------------------------------------
view <- function(model) {
  list(
    count = model$count,
    is_zero = model$count == 0
  )
}

# -- UI ----------------------------------------------------------------------
ui <- mvu_page(
  div(
    class = "container py-5 text-center",
    tags$h1("Counter", class = "mb-4"),
    div(
      class = "d-flex gap-3 justify-content-center align-items-center",
      mvu_button("\U2212", msg = "decrement",
        class = "btn btn-outline-primary btn-lg"),
      tags$span(`x-text` = "model.count", class = "display-4 mx-3"),
      mvu_button("+", msg = "increment",
        class = "btn btn-primary btn-lg")
    ),
    div(
      class = "mt-3",
      tags$button(
        class = "btn btn-outline-secondary",
        `x-show` = "!model.is_zero",
        `@click` = "send('reset')",
        "Reset"
      )
    )
  )
)

# -- Server ------------------------------------------------------------------
server <- function(input, output, session) {
  mvu_server(
    init = init, update = update, view = view,
    input = input, output = output, session = session
  )
}

shinyApp(ui, server)

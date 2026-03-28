library(shiny)
library(shinymvu)

# A counter app with the built-in time-travel debugger.
# Click the counter buttons to build up a message history,
# then open the debugger overlay to scrub through every state.

Msg <- mvu_enum(c("increment", "decrement", "set", "reset"))

init <- function() list(count = 0)

update <- function(model, msg, value) {
  match_enum(Msg(msg),
    "increment" ~ list_set(model, count = model$count + 1),
    "decrement" ~ list_set(model, count = model$count - 1),
    "set"       ~ list_set(model, count = as.integer(value)),
    "reset"     ~ mvu_result(
      list_set(model, count = 0),
      effect_notify("Counter reset to zero", type = "message", duration = 2)
    )
  )
}

to_frontend <- function(model) {
  list(
    count = model$count,
    is_zero = model$count == 0,
    is_negative = model$count < 0
  )
}

ui <- bslib::page_fillable(
  theme = bslib::bs_theme(),
  padding = "1.5rem",
  mvu_module_ui("counter", debug = TRUE,
    div(class = "text-center py-5",
      tags$h4("Counter with debugger", class = "mb-4 text-muted"),
      tags$h1(
        `x-text` = "model.count",
        class = "display-1 mb-3",
        `x-bind:class` = "{ 'text-danger': model.is_negative }"
      ),
      div(class = "d-flex gap-2 justify-content-center mb-3",
        mvu_button("\u2212", msg = "decrement",
          class = "btn btn-outline-primary btn-lg", style = "width: 3rem;"),
        mvu_button("+", msg = "increment",
          class = "btn btn-primary btn-lg", style = "width: 3rem;")
      ),
      div(class = "d-flex gap-2 justify-content-center",
        mvu_button("Reset", msg = "reset",
          class = "btn btn-outline-secondary btn-sm",
          `x-show` = "!model.is_zero"),
        mvu_button("Set to 100", msg = "set", value = 100,
          class = "btn btn-outline-secondary btn-sm")
      )
    )
  )
)

server <- function(input, output, session) {
  mvu_module_server("counter",
    init = init,
    update = update,
    msg = Msg,
    to_frontend = to_frontend,
    debug = TRUE
  )
}

shinyApp(ui, server)

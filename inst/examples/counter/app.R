library(shiny)
library(shinymvu)

# -- Counter Module ----------------------------------------------------------

counter_msg <- mvu_enum(c("increment", "decrement", "reset"))

counter_init <- function() {
  list(count = 0)
}

counter_update <- function(model, msg, value = NULL) {
  match_enum(msg,
    "increment" ~ list_set(model, count = model$count + 1),
    "decrement" ~ list_set(model, count = model$count - 1),
    "reset"     ~ list_set(model, count = 0)
  )
}

counter_ui <- function(id) {
  mvu_module_ui(id,
    div(
      class = "text-center py-4",
      div(
        mvu_button("\U2212", msg = "decrement"),
        tags$span(`x-text` = "model.count", class = "display-4 mx-3"),
        mvu_button("+", msg = "increment")
      ),
      div(
        class = "mt-3",
        mvu_button(
          "Reset", 
          msg = "reset", 
          `x-show` = "model.count !== 0"
        )
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

# -- App ---------------------------------------------------------------------

ui <- bslib::page_fillable(
  tags$h1("Counter", class = "text-center mt-4"),
  counter_ui("counter1")
)

server <- function(input, output, session) {
  counter_server("counter1")
}

shinyApp(ui, server)

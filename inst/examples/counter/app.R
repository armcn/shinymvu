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
      mvu_button("-", msg = "decrement"),
      tags$span(`x-text` = "model.count"),
      mvu_button("+", msg = "increment")
    )
  )
)

server <- function(input, output, session) {
  mvu_module_server(id = "counter", init = init, update = update, debug = TRUE)
}

shinyApp(ui, server)

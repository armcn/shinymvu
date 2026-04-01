library(shiny)
pkgload::load_all()

init <- function() list(count = 0)

update <- function(model, msg, value) {
  switch(
    msg,
    increment = purrr::list_assign(model, count = model$count + 1),
    decrement = purrr::list_assign(model, count = model$count - 1)
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

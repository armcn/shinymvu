library(shiny)
library(shinymvu)

expensive_summary <- function() {
  Sys.sleep(5)
  data.frame(
    Statistic = c("Mean MPG", "Mean HP", "Mean WT", "N"),
    Value = c(
      round(mean(mtcars$mpg), 1),
      round(mean(mtcars$hp), 1),
      round(mean(mtcars$wt), 2),
      nrow(mtcars)
    )
  )
}

init <- function() {
  list(
    count = 0,
    summary = expensive_summary()
  )
}

update <- function(model, msg, value) {
  switch(msg,
    increment = list_set(model, count = model$count + 1),
    decrement = list_set(model, count = model$count - 1),
    recalculate = list_set(model, summary = expensive_summary())
  )
}

to_frontend <- function(model) {
  list(
    count = model$count,
    rows = unname(lapply(seq_len(nrow(model$summary)), function(i) {
      as.list(model$summary[i, ])
    }))
  )
}

ui <- fluidPage(
  tags$style(HTML("
    .demo { max-width: 500px; margin: 2rem auto; }
    .counter { display: flex; gap: .5rem; align-items: center; }
  ")),
  mvu_module_ui("app",
    div(class = "demo",
      tags$h4("Blocking Example"),
      tags$p(class = "text-body-secondary",
        "Click Recalculate, then try the counter.",
        "The entire app freezes for 5 seconds."),

      div(class = "counter mb-3",
        mvu_button("-", msg = "decrement"),
        tags$span(`x-text` = "model.count"),
        mvu_button("+", msg = "increment")
      ),

      mvu_button("Recalculate", msg = "recalculate",
        class = "btn btn-primary mb-3"),

      tags$table(class = "table table-sm",
        tags$thead(tags$tr(tags$th("Statistic"), tags$th("Value"))),
        tags$tbody(
          tags$template(`x-for` = "row in model.rows",
            tags$tr(
              tags$td(`x-text` = "row.Statistic"),
              tags$td(`x-text` = "row.Value")
            )
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  mvu_module_server("app",
    init = init, update = update, to_frontend = to_frontend
  )
}

shinyApp(ui, server)

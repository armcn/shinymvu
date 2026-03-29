library(shiny)
library(shinymvu)
future::plan(future::multisession)

random_summary <- function() {
  Sys.sleep(5)
  idx <- sample(nrow(mtcars), nrow(mtcars), replace = TRUE)
  bootstrap <- mtcars[idx, ]
  data.frame(
    Statistic = c("Mean MPG", "Mean HP", "Mean WT", "N"),
    Value = c(
      round(mean(bootstrap$mpg), 1),
      round(mean(bootstrap$hp), 1),
      round(mean(bootstrap$wt), 2),
      nrow(bootstrap)
    )
  )
}

init <- function() {
  list(
    count = 0,
    loading = FALSE,
    summary = random_summary()
  )
}

update <- function(model, msg, value) {
  switch(msg,
    increment = list_set(model, count = model$count + 1),
    decrement = list_set(model, count = model$count - 1),
    recalculate = mvu_result(
      list_set(model, loading = TRUE),
      effect_cmd(random_summary, msg = "recalculated")
    ),
    recalculated = list_set(model, loading = FALSE, summary = value)
  )
}

to_frontend <- function(model) {
  list(
    count = model$count,
    loading = model$loading,
    rows = unname(lapply(seq_len(nrow(model$summary)), function(i) {
      as.list(model$summary[i, ])
    }))
  )
}

ui <- fluidPage(
  tags$style(HTML("
    .demo { max-width: 500px; margin: 2rem auto; }
    .counter { display: flex; gap: .5rem; align-items: center; }
    .table-wrap { position: relative; }
    .table-overlay { position: absolute; inset: 0; display: flex;
      align-items: center; justify-content: center;
      background: rgba(255,255,255,.7); z-index: 1; }
    .spinner { width: 1.5rem; height: 1.5rem;
      border: 3px solid var(--bs-primary); border-right-color: transparent;
      border-radius: 50%; animation: spin .6s linear infinite; }
    @keyframes spin { to { transform: rotate(360deg); } }
  ")),
  mvu_module_ui("app",
    div(class = "demo",
      tags$h4("Async Example"),
      tags$p(class = "text-body-secondary",
        "Click Recalculate, then try the counter.",
        "The app stays interactive while the background",
        "process runs. Each recalculation bootstraps",
        "mtcars so the numbers change."),

      div(class = "counter mb-3",
        mvu_button("-", msg = "decrement"),
        tags$span(`x-text` = "model.count"),
        mvu_button("+", msg = "increment")
      ),

      mvu_button("Recalculate", msg = "recalculate",
        class = "btn btn-primary mb-3"),

      div(class = "table-wrap",
        tags$template(`x-if` = "model.loading",
          div(class = "table-overlay",
            tags$span(class = "spinner")
          )
        ),
        tags$table(class = "table table-sm mb-0",
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
)

server <- function(input, output, session) {
  mvu_module_server("app",
    init = init, update = update, to_frontend = to_frontend,
    debug = TRUE
  )
}

shinyApp(ui, server)

library(shiny)
library(shinymvu)

# -- Filter module (MVU) -----------------------------------------------------
# Controls which dataset and how many rows to show.

filter_init <- function() list(dataset = "mtcars", n = 10)

filter_update <- function(model, msg, value) {
  switch(msg,
    set_dataset = list_set(model, dataset = value),
    set_n       = list_set(model, n = value)
  )
}

filter_ui <- function(id) {
  mvu_module_ui(id,
    div(class = "mb-3",
      mvu_select_input("Dataset",
        choices = c("mtcars", "iris", "airquality"),
        msg = "set_dataset", value = "model.dataset")
    ),
    mvu_slider_input("Rows", min = 1, max = 30,
      msg = "set_n", value = "model.n")
  )
}

filter_server <- function(id) {
  mvu_module_server(id, init = filter_init, update = filter_update)
}

# -- Table module (MVU) ------------------------------------------------------
# Displays data based on external filters. Has its own local state (sort
# column) to demonstrate that it stays interactive while responding to
# subscription messages.

table_init <- function() {
  list(dataset = "mtcars", n = 10, sort_col = "")
}

table_update <- function(model, msg, value) {
  switch(msg,
    filters_changed = list_set(model,
      dataset = value$dataset,
      n       = value$n
    ),
    set_sort = list_set(model, sort_col = value)
  )
}

table_to_frontend <- function(model) {
  df <- get(model$dataset, envir = asNamespace("datasets"))
  if (nzchar(model$sort_col) && model$sort_col %in% names(df)) {
    df <- df[order(df[[model$sort_col]]), ]
  }
  df <- utils::head(df, model$n)
  list(
    dataset  = model$dataset,
    n        = model$n,
    sort_col = model$sort_col,
    columns  = as.list(names(df)),
    rows     = unname(lapply(seq_len(nrow(df)), function(i) {
      as.list(df[i, ])
    }))
  )
}

table_ui <- function(id) {
  mvu_module_ui(id,
    div(
      tags$p(class = "text-body-secondary mb-2",
        tags$span(`x-text` = "model.dataset"),
        " \u2014 showing ",
        tags$span(`x-text` = "model.n"),
        " rows",
        tags$template(`x-if` = "model.sort_col",
          tags$span(", sorted by ",
            tags$strong(`x-text` = "model.sort_col"))
        )
      ),
      tags$table(class = "table table-sm table-hover mb-0",
        tags$thead(
          tags$tr(
            tags$template(`x-for` = "col in model.columns",
              tags$th(
                style = "cursor: pointer;",
                `x-text` = "col",
                `@click` = "send('set_sort', col)"
              )
            )
          )
        ),
        tags$tbody(
          tags$template(`x-for` = "(row, i) in model.rows",
            `x-bind:key` = "i",
            tags$tr(
              tags$template(`x-for` = "col in model.columns",
                tags$td(`x-text` = "row[col]")
              )
            )
          )
        )
      )
    )
  )
}

table_server <- function(id, filter_model) {
  mvu_module_server(id,
    init = table_init,
    update = table_update,
    to_frontend = table_to_frontend,
    subscriptions = function() list(
      filters_changed = filter_model
    )
  )
}

# -- App ---------------------------------------------------------------------

ui <- fluidPage(
  tags$style(HTML(".app { max-width: 800px; margin: 2rem auto; }")),
  div(class = "app",
    tags$h4("Subscriptions Example"),
    tags$p(class = "text-body-secondary",
      "Two MVU modules communicating. The filter module's model is",
      "subscribed to by the table module. Click a column header to",
      "sort \u2014 that's local table state, independent of the filters."),
    bslib::layout_columns(
      col_widths = c(4, 8),
      bslib::card(
        bslib::card_header("Filters"),
        bslib::card_body(filter_ui("filters"))
      ),
      bslib::card(
        bslib::card_header("Data"),
        bslib::card_body(class = "p-0", table_ui("table"))
      )
    )
  )
)

server <- function(input, output, session) {
  filter_model <- filter_server("filters")
  table_server("table", filter_model)
}

shinyApp(ui, server)

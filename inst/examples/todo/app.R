library(shiny)
library(shinymvu)

# --- Messages ---------------------------------------------------------------

Msg <- mvu_enum(c(
  "add_todo", "toggle_todo", "remove_todo",
  "set_filter", "clear_completed"
))

# --- Init / Update / View ---------------------------------------------------

init <- function() {
  list(
    todos = list(
      list(id = 1L, text = "Learn shinymvu", done = FALSE),
      list(id = 2L, text = "Build something cool", done = FALSE)
    ),
    next_id = 3L,
    filter = "all"
  )
}

update <- function(model, msg, value) {
  match_enum(Msg(msg),

    "add_todo" ~ {
      if (!nzchar(value)) return(model)
      new_todo <- list(id = model$next_id, text = value, done = FALSE)
      list_set(model,
        todos = c(model$todos, list(new_todo)),
        next_id = model$next_id + 1L
      )
    },

    "toggle_todo" ~ {
      id <- as.integer(value)
      list_set(model,
        todos = lapply(model$todos, function(t) {
          if (t$id == id) list_set(t, done = !t$done) else t
        })
      )
    },

    "remove_todo" ~ {
      id <- as.integer(value)
      mvu_result(
        list_set(model,
          todos = Filter(function(t) t$id != id, model$todos)
        ),
        effect_notify("Item removed", type = "message", duration = 2)
      )
    },

    "set_filter" ~ list_set(model, filter = value),

    "clear_completed" ~ {
      n <- sum(vapply(model$todos, function(t) t$done, logical(1)))
      mvu_result(
        list_set(model,
          todos = Filter(function(t) !t$done, model$todos)
        ),
        if (n > 0) effect_notify(sprintf("Cleared %d item(s)", n)) else NULL
      )
    }
  )
}

to_frontend <- function(model) {
  visible <- switch(model$filter,
    active    = Filter(function(t) !t$done, model$todos),
    completed = Filter(function(t) t$done, model$todos),
    model$todos
  )
  list(
    todos = visible,
    total = length(model$todos),
    done_count = sum(vapply(model$todos, function(t) t$done, logical(1))),
    active_count = sum(vapply(model$todos, function(t) !t$done, logical(1))),
    filter = model$filter
  )
}

# --- UI ---------------------------------------------------------------------

ui <- bslib::page_fillable(
  theme = bslib::bs_theme(),
  padding = 0,
  mvu_module_ui("todo",
    extra_js = "newTodo: ''",
    div(class = "container py-4", style = "max-width: 520px;",
    tags$h2(class = "mb-4 text-center", "Todo MVU"),

    # Input row
    div(class = "input-group mb-3",
      tags$input(
        type = "text", class = "form-control",
        placeholder = "What needs to be done?",
        `x-model` = "newTodo",
        `@keydown.enter` = "send('add_todo', newTodo); newTodo = ''"
      ),
      tags$button(
        class = "btn btn-primary",
        `@click` = "send('add_todo', newTodo); newTodo = ''",
        "Add"
      )
    ),

    # Todo list
    div(
      tags$template(
        `x-for` = "todo in model.todos", `x-bind:key` = "todo.id",
        div(class = "d-flex align-items-center border rounded-2 p-2 mb-2",
          tags$input(
            type = "checkbox", class = "form-check-input me-2 flex-shrink-0",
            `x-bind:checked` = "todo.done",
            `@change` = "send('toggle_todo', todo.id)"
          ),
          tags$span(
            class = "flex-grow-1",
            `x-text` = "todo.text",
            `x-bind:class` = "{ 'text-decoration-line-through text-body-secondary': todo.done }"
          ),
          tags$button(
            class = "btn btn-sm btn-outline-danger ms-2",
            `@click` = "send('remove_todo', todo.id)",
            "x"
          )
        )
      ),
      div(
        `x-show` = "model.todos.length === 0",
        class = "text-center text-body-secondary py-4",
        "No items to show"
      )
    ),

    # Footer: counts + filters
    div(class = "d-flex justify-content-between align-items-center mt-3",
      style = "font-size: .875rem;",
      tags$span(
        class = "text-body-secondary",
        `x-text` = "model.active_count + ' item(s) left'"
      ),
      div(class = "btn-group btn-group-sm",
        tags$button(
          class = "btn btn-outline-secondary",
          `x-bind:class` = "{ active: model.filter === 'all' }",
          `@click` = "send('set_filter', 'all')", "All"
        ),
        tags$button(
          class = "btn btn-outline-secondary",
          `x-bind:class` = "{ active: model.filter === 'active' }",
          `@click` = "send('set_filter', 'active')", "Active"
        ),
        tags$button(
          class = "btn btn-outline-secondary",
          `x-bind:class` = "{ active: model.filter === 'completed' }",
          `@click` = "send('set_filter', 'completed')", "Completed"
        )
      ),
      tags$button(
        class = "btn btn-link btn-sm text-body-secondary text-decoration-none p-0",
        `x-show` = "model.done_count > 0",
        `@click` = "send('clear_completed')",
        "Clear completed"
      )
    )
  )
  )
)

# --- Server -----------------------------------------------------------------

server <- function(input, output, session) {
  mvu_module_server("todo",
    init = init,
    update = update,
    to_frontend = to_frontend
  )
}

shinyApp(ui, server)

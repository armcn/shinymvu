library(shiny)
library(shinymvu)

# A faithful port of https://github.com/evancz/elm-todomvc using shinymvu.
# Uses the original TodoMVC CSS and identical behaviour.

# -- Messages ----------------------------------------------------------------

Msg <- mvu_enum(c(
  "add", "check", "check_all", "delete",
  "delete_complete", "update_entry", "change_visibility"
))

# -- Model -------------------------------------------------------------------

init <- function() {
  list(entries = list(), uid = 0L, visibility = "All")
}

update <- function(model, msg, value) {
  match_enum(msg,
    "add" ~ {
      desc <- trimws(as.character(value))
      if (!nzchar(desc)) return(model)
      entry <- list(id = model$uid, description = desc, completed = FALSE)
      list_set(model,
        entries = c(model$entries, list(entry)),
        uid = model$uid + 1L
      )
    },
    "check" ~ {
      id <- as.integer(value$id)
      completed <- as.logical(value$completed)
      list_set(model, entries = lapply(model$entries, function(e) {
        if (e$id == id) list_set(e, completed = completed) else e
      }))
    },
    "check_all" ~ {
      completed <- as.logical(value)
      list_set(model, entries = lapply(model$entries, function(e) {
        list_set(e, completed = completed)
      }))
    },
    "delete" ~ {
      id <- as.integer(value)
      list_set(model, entries = Filter(function(e) e$id != id, model$entries))
    },
    "delete_complete" ~ {
      list_set(model, entries = Filter(function(e) !e$completed, model$entries))
    },
    "update_entry" ~ {
      id <- as.integer(value$id)
      desc <- trimws(as.character(value$description))
      if (!nzchar(desc)) {
        return(list_set(model,
          entries = Filter(function(e) e$id != id, model$entries)))
      }
      list_set(model, entries = lapply(model$entries, function(e) {
        if (e$id == id) list_set(e, description = desc) else e
      }))
    },
    "change_visibility" ~ list_set(model, visibility = as.character(value))
  )
}

to_frontend <- function(model) {
  n <- length(model$entries)
  n_done <- sum(vapply(model$entries, function(e) e$completed, logical(1)))
  list(
    entries = model$entries,
    visibility = model$visibility,
    items_left = n - n_done,
    items_completed = n_done,
    all_completed = n > 0 && n_done == n,
    any_entries = n > 0
  )
}

# -- Client-side Alpine.js ---------------------------------------------------

extra_js <- "
field: '',
editingId: null,
editText: '',
get visibleEntries() {
  if (!this.model || !this.model.entries) return [];
  var v = this.model.visibility;
  return this.model.entries.filter(function(e) {
    if (v === 'Active') return !e.completed;
    if (v === 'Completed') return e.completed;
    return true;
  });
},
addTodo() {
  var text = this.field.trim();
  if (text) {
    this.send('add', text);
    this.field = '';
  }
},
startEdit(entry) {
  this.editingId = entry.id;
  this.editText = entry.description;
  var self = this;
  this.$nextTick(function() {
    var el = document.getElementById('todo-' + entry.id);
    if (el) el.focus();
  });
},
submitEdit(id) {
  if (this.editingId === null) return;
  var text = this.editText.trim();
  if (text) {
    this.send('update_entry', {id: id, description: text});
  } else {
    this.send('delete', id);
  }
  this.editingId = null;
},
cancelEdit() {
  this.editingId = null;
}
"

# -- CSS (Elm TodoMVC / todomvc-app-css) -------------------------------------

todomvc_css <- "
body {
  background-color: #f5f5f5 !important;
  min-height: 100vh;
}

.todoapp, .todoapp *, .todoapp *::before, .todoapp *::after {
  box-sizing: content-box;
}

.todomvc-wrapper {
  font: 14px 'Helvetica Neue', Helvetica, Arial, sans-serif;
  line-height: 1.4em;
  color: #4d4d4d;
  font-weight: 300;
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

.todoapp button {
  margin: 0; padding: 0; border: 0; background: none;
  font-size: 100%; vertical-align: baseline;
  font-family: inherit; font-weight: inherit; color: inherit;
  -webkit-appearance: none; appearance: none;
}

.todoapp button:focus,
.todoapp input[type='checkbox']:focus { outline: none; }

.todoapp {
  background: #fff;
  margin: 130px auto 40px;
  max-width: 550px;
  min-width: 230px;
  position: relative;
  box-shadow: 0 2px 4px 0 rgba(0,0,0,0.2), 0 25px 50px 0 rgba(0,0,0,0.1);
}

.todoapp input::placeholder {
  font-style: italic;
  font-weight: 300;
  color: #e6e6e6;
}

.todoapp h1 {
  position: absolute;
  top: -155px;
  width: 100%;
  font-size: 100px;
  font-weight: 100;
  text-align: center;
  color: rgba(175, 47, 47, 0.15);
  -webkit-text-rendering: optimizeLegibility;
  text-rendering: optimizeLegibility;
}

.new-todo, .edit {
  position: relative; margin: 0; width: 100%;
  font-size: 24px; font-family: inherit; font-weight: inherit;
  line-height: 1.4em; color: inherit; padding: 6px;
  border: 1px solid #999;
  box-shadow: inset 0 -1px 5px 0 rgba(0,0,0,0.2);
  box-sizing: border-box;
}

.new-todo {
  padding: 16px 16px 16px 60px;
  height: 65px;
  border: none;
  background: rgba(0,0,0,0.003);
  box-shadow: inset 0 -2px 1px rgba(0,0,0,0.03);
}

.main {
  position: relative; z-index: 2;
  border-top: 1px solid #e6e6e6;
}

.toggle-all {
  width: 1px; height: 1px; border: none; opacity: 0;
  position: absolute; right: 100%; bottom: 100%;
}

.toggle-all + label {
  display: flex; align-items: center; justify-content: center;
  width: 45px; height: 65px; font-size: 0;
  position: absolute; top: -65px; left: 0;
}

.toggle-all + label:before {
  content: '\\276F';
  display: inline-block; font-size: 22px; color: #e6e6e6;
  padding: 10px 27px;
  transform: rotate(90deg);
}

.toggle-all:checked + label:before { color: #737373; }

.todo-list { margin: 0; padding: 0; list-style: none; }

.todo-list li {
  position: relative; font-size: 24px;
  border-bottom: 1px solid #ededed;
}

.todo-list li:last-child { border-bottom: none; }
.todo-list li.editing { border-bottom: none; padding: 0; }

.todo-list li.editing .edit {
  display: block;
  width: calc(100% - 43px);
  padding: 12px 16px;
  margin: 0 0 0 43px;
}

.todo-list li.editing .view { display: none; }

.todo-list li .toggle {
  text-align: center; width: 40px; height: auto;
  position: absolute; top: 0; bottom: 0;
  margin: auto 0; border: none;
  -webkit-appearance: none; appearance: none;
  opacity: 0;
}

.todo-list li .toggle + label {
  background-image: url('data:image/svg+xml;utf8,%3Csvg%20xmlns%3D%22http%3A//www.w3.org/2000/svg%22%20width%3D%2240%22%20height%3D%2240%22%20viewBox%3D%22-10%20-18%20100%20135%22%3E%3Ccircle%20cx%3D%2250%22%20cy%3D%2250%22%20r%3D%2250%22%20fill%3D%22none%22%20stroke%3D%22%23ededed%22%20stroke-width%3D%223%22/%3E%3C/svg%3E');
  background-repeat: no-repeat;
  background-position: center left;
}

.todo-list li .toggle:checked + label {
  background-image: url('data:image/svg+xml;utf8,%3Csvg%20xmlns%3D%22http%3A%2F%2Fwww.w3.org%2F2000%2Fsvg%22%20width%3D%2240%22%20height%3D%2240%22%20viewBox%3D%22-10%20-18%20100%20135%22%3E%3Ccircle%20cx%3D%2250%22%20cy%3D%2250%22%20r%3D%2250%22%20fill%3D%22none%22%20stroke%3D%22%2359A193%22%20stroke-width%3D%223%22%2F%3E%3Cpath%20fill%3D%22%233EA390%22%20d%3D%22M72%2025L42%2071%2027%2056l-4%204%2020%2020%2034-52z%22%2F%3E%3C%2Fsvg%3E');
}

.todo-list li label {
  word-break: break-all;
  padding: 15px 60px 15px 15px;
  margin-left: 45px;
  display: block; line-height: 1.2;
  transition: color 0.4s;
}

.todo-list li.completed label {
  color: #d9d9d9;
  text-decoration: line-through;
}

.todo-list li .destroy {
  display: none; position: absolute;
  top: 0; right: 10px; bottom: 0;
  width: 40px; height: 40px; margin: auto 0;
  font-size: 30px; color: #cc9a9a;
  margin-bottom: 11px;
  transition: color 0.2s ease-out;
}

.todo-list li .destroy:hover { color: #af5b5e; }
.todo-list li .destroy:after { content: '\\00d7'; }
.todo-list li:hover .destroy { display: block; }

.todo-list li .edit { display: none; }
.todo-list li.editing:last-child { margin-bottom: -1px; }

.footer {
  color: #777; padding: 10px 15px;
  height: 20px; text-align: center;
  border-top: 1px solid #e6e6e6;
}

.footer:before {
  content: ''; position: absolute;
  right: 0; bottom: 0; left: 0;
  height: 50px; overflow: hidden;
  box-shadow:
    0 1px 1px rgba(0,0,0,0.2),
    0 8px 0 -3px #f6f6f6,
    0 9px 1px -3px rgba(0,0,0,0.2),
    0 16px 0 -6px #f6f6f6,
    0 17px 2px -6px rgba(0,0,0,0.2);
}

.todo-count { float: left; text-align: left; }
.todo-count strong { font-weight: 300; }

.filters {
  margin: 0; padding: 0; list-style: none;
  position: absolute; right: 0; left: 0;
}

.filters li { display: inline; }

.filters li a {
  color: inherit; margin: 3px; padding: 3px 7px;
  text-decoration: none;
  border: 1px solid transparent; border-radius: 3px;
}

.filters li a:hover { border-color: rgba(175, 47, 47, 0.1); }
.filters li a.selected { border-color: rgba(175, 47, 47, 0.2); }

.clear-completed, html .clear-completed:active {
  float: right; position: relative; line-height: 20px;
  text-decoration: none; cursor: pointer;
}

.clear-completed:hover { text-decoration: underline; }

.info {
  margin: 65px auto 0; max-width: 550px;
  color: #bfbfbf; font-size: 10px;
  text-shadow: 0 1px 0 rgba(255,255,255,0.5);
  text-align: center;
}

.info p { line-height: 1; }
.info a { color: inherit; text-decoration: none; font-weight: 400; }
.info a:hover { text-decoration: underline; }

@media screen and (-webkit-min-device-pixel-ratio:0) {
  .toggle-all, .todo-list li .toggle { background: none; }
  .todo-list li .toggle { height: 40px; }
}

@media (max-width: 430px) {
  .footer { height: 50px; }
  .filters { bottom: 10px; }
}
"

# -- UI ----------------------------------------------------------------------

ui <- bslib::page_fillable(
  theme = bslib::bs_theme(),
  padding = 0,
  tags$head(tags$style(HTML(todomvc_css))),
  mvu_module_ui("todo", debug = TRUE, extra_js = extra_js,
    div(class = "todomvc-wrapper",
      tags$section(class = "todoapp",

        # -- Header / input --
        tags$header(class = "header",
          tags$h1("todos"),
          tags$input(
            class = "new-todo",
            placeholder = "What needs to be done?",
            autofocus = NA,
            `x-model` = "field",
            `@keydown.enter` = "addTodo()"
          )
        ),

        # -- Main section (todo list) --
        tags$section(class = "main", `x-show` = "model.any_entries",
          tags$input(
            id = "toggle-all", class = "toggle-all", type = "checkbox",
            `x-bind:checked` = "model.all_completed",
            `@change` = "send('check_all', !model.all_completed)"
          ),
          tags$label(`for` = "toggle-all", "Mark all as complete"),
          tags$ul(class = "todo-list",
            tags$template(`x-for` = "entry in visibleEntries",
              `x-bind:key` = "entry.id",
              tags$li(
                `x-bind:class` =
                  "{ completed: entry.completed, editing: editingId === entry.id }",

                # View mode
                div(class = "view",
                  tags$input(
                    class = "toggle", type = "checkbox",
                    `x-bind:checked` = "entry.completed",
                    `@change` =
                      "send('check', {id: entry.id, completed: !entry.completed})"
                  ),
                  tags$label(
                    `@dblclick` = "startEdit(entry)",
                    `x-text` = "entry.description"
                  ),
                  tags$button(
                    class = "destroy",
                    `@click` = "send('delete', entry.id)"
                  )
                ),

                # Edit mode
                tags$input(
                  class = "edit",
                  `x-bind:id` = "'todo-' + entry.id",
                  `x-model` = "editText",
                  `@blur` = "submitEdit(entry.id)",
                  `@keydown.enter` = "$event.target.blur()",
                  `@keydown.escape` = "cancelEdit()"
                )
              )
            )
          )
        ),

        # -- Footer (counts, filters, clear) --
        tags$footer(class = "footer", `x-show` = "model.any_entries",
          tags$span(class = "todo-count",
            tags$strong(`x-text` = "model.items_left"),
            tags$span(`x-text` = "model.items_left === 1 ? ' item left' : ' items left'")
          ),
          tags$ul(class = "filters",
            tags$li(tags$a(
              href = "#/",
              `x-bind:class` = "{ selected: model.visibility === 'All' }",
              `@click.prevent` = "send('change_visibility', 'All')",
              "All"
            )),
            tags$li(tags$a(
              href = "#/active",
              `x-bind:class` = "{ selected: model.visibility === 'Active' }",
              `@click.prevent` = "send('change_visibility', 'Active')",
              "Active"
            )),
            tags$li(tags$a(
              href = "#/completed",
              `x-bind:class` = "{ selected: model.visibility === 'Completed' }",
              `@click.prevent` = "send('change_visibility', 'Completed')",
              "Completed"
            ))
          ),
          tags$button(
            class = "clear-completed",
            `x-show` = "model.items_completed > 0",
            `@click` = "send('delete_complete')",
            `x-text` = "'Clear completed (' + model.items_completed + ')'"
          )
        )
      ),

      # -- Info footer --
      tags$footer(class = "info",
        tags$p("Double-click to edit a todo"),
        tags$p(
          "Written with ",
          tags$a(href = "https://github.com/armcn/shinymvu", "shinymvu")
        ),
        tags$p(
          "Part of ",
          tags$a(href = "http://todomvc.com", "TodoMVC")
        )
      )
    )
  )
)

# -- Server ------------------------------------------------------------------

server <- function(input, output, session) {
  mvu_module_server("todo",
    init = init,
    update = update,
    msg = Msg,
    to_frontend = to_frontend,
    debug = TRUE
  )
}

shinyApp(ui, server)

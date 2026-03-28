library(shiny)
library(shinymvu)

# -- Messages ------------------------------------------------------------------

gallery_msg <- mvu_enum(c(
  "clicked", "set_name", "set_email", "set_password", "set_bio",
  "set_age", "set_color", "toggle_agree", "toggle_topping",
  "set_pet", "set_volume", "set_date", "set_depart", "set_return",
  "upload_file"
))

# -- Init ----------------------------------------------------------------------

gallery_init <- function() {
  list(
    click_count = 0,
    name = "", email = "", password = "", bio = "",
    age = 25, color = "blue",
    agree = FALSE, toppings = list(),
    pet = "dog", volume = 50,
    date = "", depart = "", return_date = "",
    files = list()
  )
}

# -- Update --------------------------------------------------------------------

gallery_update <- function(model, msg, value = NULL) {
  match_enum(msg,
    "clicked"        ~ list_set(model, click_count = model$click_count + 1),
    "set_name"       ~ list_set(model, name = value),
    "set_email"      ~ list_set(model, email = value),
    "set_password"   ~ list_set(model, password = value),
    "set_bio"        ~ list_set(model, bio = value),
    "set_age"        ~ list_set(model, age = value),
    "set_color"      ~ list_set(model, color = value),
    "toggle_agree"   ~ list_set(model, agree = value),
    "toggle_topping" ~ {
      current <- model$toppings
      if (value %in% current) {
        list_set(model, toppings = as.list(setdiff(current, value)))
      } else {
        list_set(model, toppings = as.list(c(current, value)))
      }
    },
    "set_pet"        ~ list_set(model, pet = value),
    "set_volume"     ~ list_set(model, volume = value),
    "set_date"       ~ list_set(model, date = value),
    "set_depart"     ~ list_set(model, depart = value),
    "set_return"     ~ list_set(model, return_date = value),
    "upload_file"    ~ list_set(model, files = value)
  )
}

# -- Helpers -------------------------------------------------------------------

code_block <- function(code) {
  escaped <- htmltools::htmlEscape(code)
  HTML(sprintf(
    '<pre class="border rounded p-3 mb-0" style="font-size: 0.82rem; overflow-x: auto; white-space: pre; background: #1e1e2e; color: #cdd6f4; line-height: 1.6;"><code style="color: inherit;">%s</code></pre>',
    escaped
  ))
}

value_pill <- function(label, alpine_expr) {
  tags$span(
    class = "badge bg-light text-dark border font-monospace me-2 mb-1",
    style = "font-size: 0.78rem;",
    paste0(label, " = "),
    tags$span(
      class = "text-primary",
      `x-text` = sprintf("JSON.stringify(%s)", alpine_expr)
    )
  )
}

input_card <- function(fn_name, code, ..., values = list()) {
  value_tags <- lapply(names(values), function(nm) {
    value_pill(nm, values[[nm]])
  })
  bslib::card(class = "mb-4",
    bslib::card_header(class = "bg-white py-3",
      tags$code(class = "fs-6 fw-semibold", fn_name)
    ),
    bslib::card_body(class = "p-0",
      div(class = "p-3 border-bottom", code_block(code)),
      div(class = "p-3",
        ...,
        if (length(value_tags) > 0) {
          div(class = "mt-3 pt-2 border-top d-flex flex-wrap",
            tagList(value_tags)
          )
        }
      )
    )
  )
}

# -- UI ------------------------------------------------------------------------

gallery_ui <- function(id) {
  mvu_module_ui(id,
    tags$style(HTML("
      body { background: #f8f9fa; }
      .card { box-shadow: 0 1px 3px rgba(0,0,0,.08); }
    ")),
    div(class = "container py-4", style = "max-width: 960px;",

      # Header
      div(class = "text-center mb-5",
        tags$h1(class = "fw-bold", "shinymvu Input Gallery"),
        tags$p(class = "text-muted lead",
          "Every input wrapper with its source code and live output.")
      ),

      # mvu_button
      input_card("mvu_button()",
        code = paste(
          "mvu_button(",
          "  label = \"Click me\",",
          "  msg = \"clicked\"",
          ")",
          sep = "\n"
        ),
        mvu_button("Click me", msg = "clicked"),
        values = list("click_count" = "model.click_count")
      ),

      # mvu_text_input
      input_card("mvu_text_input()",
        code = paste(
          "mvu_text_input(",
          "  label = \"Name\",",
          "  msg = \"set_name\",",
          "  value = \"model.name\",",
          "  placeholder = \"Your name\",",
          "  debounce = 300",
          ")",
          sep = "\n"
        ),
        mvu_text_input("Name", msg = "set_name",
          value = "model.name", placeholder = "Your name",
          debounce = 300),
        values = list("name" = "model.name")
      ),

      # mvu_select_input
      input_card("mvu_select_input()",
        code = paste(
          "mvu_select_input(",
          "  label = \"Favorite Color\",",
          "  choices = c(",
          "    \"Red\" = \"red\",",
          "    \"Blue\" = \"blue\",",
          "    \"Green\" = \"green\"",
          "  ),",
          "  msg = \"set_color\",",
          "  selected = \"model.color\"",
          ")",
          sep = "\n"
        ),
        div(style = "max-width: 300px;",
          mvu_select_input("Favorite Color",
            choices = c(
              "Red" = "red", "Blue" = "blue", "Green" = "green"
            ),
            msg = "set_color", selected = "model.color")
        ),
        values = list("color" = "model.color")
      ),

      # mvu_numeric_input
      input_card("mvu_numeric_input()",
        code = paste(
          "mvu_numeric_input(",
          "  label = \"Age\",",
          "  msg = \"set_age\",",
          "  value = \"model.age\",",
          "  min = 0,",
          "  max = 120,",
          "  step = 1",
          ")",
          sep = "\n"
        ),
        div(style = "max-width: 200px;",
          mvu_numeric_input("Age", msg = "set_age",
            value = "model.age", min = 0, max = 120, step = 1)
        ),
        values = list("age" = "model.age")
      ),

      # mvu_slider_input
      input_card("mvu_slider_input()",
        code = paste(
          "mvu_slider_input(",
          "  label = \"Volume\",",
          "  min = 0,",
          "  max = 100,",
          "  msg = \"set_volume\",",
          "  value = \"model.volume\"",
          ")",
          sep = "\n"
        ),
        mvu_slider_input("Volume", min = 0, max = 100,
          msg = "set_volume", value = "model.volume"),
        values = list("volume" = "model.volume")
      ),

      # mvu_checkbox_input
      input_card("mvu_checkbox_input()",
        code = paste(
          "mvu_checkbox_input(",
          "  label = \"I agree to the terms\",",
          "  msg = \"toggle_agree\",",
          "  value = \"model.agree\"",
          ")",
          sep = "\n"
        ),
        mvu_checkbox_input("I agree to the terms",
          msg = "toggle_agree", value = "model.agree"),
        values = list("agree" = "model.agree")
      ),

      # mvu_radio_input
      input_card("mvu_radio_input()",
        code = paste(
          "mvu_radio_input(",
          "  label = \"Favorite Pet\",",
          "  choices = c(",
          "    \"Dog\" = \"dog\",",
          "    \"Cat\" = \"cat\",",
          "    \"Fish\" = \"fish\",",
          "    \"Bird\" = \"bird\"",
          "  ),",
          "  msg = \"set_pet\",",
          "  selected = \"model.pet\"",
          ")",
          sep = "\n"
        ),
        mvu_radio_input("Favorite Pet",
          choices = c(
            "Dog" = "dog", "Cat" = "cat",
            "Fish" = "fish", "Bird" = "bird"
          ),
          msg = "set_pet", selected = "model.pet"),
        values = list("pet" = "model.pet")
      ),

      # mvu_textarea_input
      input_card("mvu_textarea_input()",
        code = paste(
          "mvu_textarea_input(",
          "  label = \"Bio\",",
          "  msg = \"set_bio\",",
          "  value = \"model.bio\",",
          "  rows = 3,",
          "  placeholder = \"Tell us...\",",
          "  debounce = 300",
          ")",
          sep = "\n"
        ),
        mvu_textarea_input("Bio", msg = "set_bio",
          value = "model.bio", rows = 3,
          placeholder = "Tell us...", debounce = 300),
        values = list("bio" = "model.bio")
      ),

      # mvu_checkbox_group_input
      input_card("mvu_checkbox_group_input()",
        code = paste(
          "mvu_checkbox_group_input(",
          "  label = \"Pizza Toppings\",",
          "  choices = c(",
          "    \"Cheese\" = \"cheese\",",
          "    \"Peppers\" = \"peppers\",",
          "    \"Onions\" = \"onions\",",
          "    \"Mushrooms\" = \"mushrooms\"",
          "  ),",
          "  msg = \"toggle_topping\",",
          "  selected = \"model.toppings\"",
          ")",
          sep = "\n"
        ),
        mvu_checkbox_group_input("Pizza Toppings",
          choices = c(
            "Cheese" = "cheese", "Peppers" = "peppers",
            "Onions" = "onions", "Mushrooms" = "mushrooms"
          ),
          msg = "toggle_topping", selected = "model.toppings"),
        values = list("toppings" = "model.toppings")
      ),

      # mvu_password_input
      input_card("mvu_password_input()",
        code = paste(
          "mvu_password_input(",
          "  label = \"Password\",",
          "  msg = \"set_password\",",
          "  value = \"model.password\",",
          "  placeholder = \"Secret\",",
          "  debounce = 300",
          ")",
          sep = "\n"
        ),
        div(style = "max-width: 300px;",
          mvu_password_input("Password", msg = "set_password",
            value = "model.password", placeholder = "Secret",
            debounce = 300)
        ),
        values = list("password" = "model.password")
      ),

      # mvu_date_input
      input_card("mvu_date_input()",
        code = paste(
          "mvu_date_input(",
          "  label = \"Birthday\",",
          "  msg = \"set_date\",",
          "  value = \"model.date\"",
          ")",
          sep = "\n"
        ),
        div(style = "max-width: 250px;",
          mvu_date_input("Birthday", msg = "set_date",
            value = "model.date")
        ),
        values = list("date" = "model.date")
      ),

      # mvu_date_range_input
      input_card("mvu_date_range_input()",
        code = paste(
          "mvu_date_range_input(",
          "  label = \"Trip Dates\",",
          "  msg_start = \"set_depart\",",
          "  msg_end = \"set_return\",",
          "  start = \"model.depart\",",
          "  end = \"model.return_date\"",
          ")",
          sep = "\n"
        ),
        mvu_date_range_input("Trip Dates",
          msg_start = "set_depart", msg_end = "set_return",
          start = "model.depart",
          end = "model.return_date"),
        values = list(
          "depart" = "model.depart",
          "return_date" = "model.return_date"
        )
      ),

      # mvu_file_input
      input_card("mvu_file_input()",
        code = paste(
          "mvu_file_input(",
          "  label = \"Upload a file\",",
          "  msg = \"upload_file\",",
          "  accept = \"image/*\",",
          "  multiple = TRUE",
          ")",
          sep = "\n"
        ),
        mvu_file_input("Upload a file", msg = "upload_file",
          accept = "image/*", multiple = TRUE),
        values = list("files" = "model.files")
      ),

      # Low-level event handlers
      input_card("on_click() / on_input() / on_check()",
        code = paste(
          "# Splice into any tags$ element with !!!",
          "",
          "tags$button(",
          "  !!!on_click(\"increment\"),",
          "  \"+\"",
          ")",
          "",
          "tags$select(",
          "  !!!on_input(\"set_color\"),",
          "  tags$option(value = \"red\", \"Red\"),",
          "  tags$option(value = \"blue\", \"Blue\")",
          ")",
          "",
          "tags$input(",
          "  type = \"checkbox\",",
          "  !!!on_check(\"toggle_agree\")",
          ")",
          "",
          "tags$input(",
          "  type = \"range\",",
          "  min = \"0\",",
          "  max = \"100\",",
          "  !!!on_input_num(\"set_volume\")",
          ")",
          sep = "\n"
        ),
        tags$p(class = "text-muted small mb-0",
          "These return named lists of Alpine.js attributes. ",
          "Use them when you need full control over the HTML markup."
        )
      ),

      # Footer
      tags$hr(class = "my-4"),
      tags$p(class = "text-center text-muted small pb-4",
        "shinymvu \u2014 The Elm Architecture for Shiny")
    )
  )
}

# -- Server --------------------------------------------------------------------

gallery_server <- function(id) {
  mvu_module_server(id,
    init = gallery_init,
    update = gallery_update,
    msg = gallery_msg
  )
}

# -- App -----------------------------------------------------------------------

ui <- bslib::page_fillable(
  theme = bslib::bs_theme(version = 5, bootswatch = "flatly"),
  gallery_ui("gallery")
)

server <- function(input, output, session) {
  gallery_server("gallery")
}

shinyApp(ui, server)

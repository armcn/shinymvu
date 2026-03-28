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

# -- UI ------------------------------------------------------------------------

gallery_ui <- function(id) {
  mvu_module_ui(id,
    div(class = "container-fluid py-4",
      div(class = "row",

        # Left column: inputs
        div(class = "col-lg-8",
          tags$h2("shinymvu Input Gallery", class = "mb-4"),

          # Button
          bslib::card(
            bslib::card_header("mvu_button"),
            bslib::card_body(
              div(class = "d-flex gap-2 align-items-center",
                mvu_button("Click me", msg = "clicked"),
                mvu_button("Danger", msg = "clicked",
                  class = "btn btn-danger"),
                mvu_button("Outline", msg = "clicked",
                  class = "btn btn-outline-secondary"),
                tags$span(
                  class = "ms-3 text-muted",
                  "Clicks: ",
                  tags$strong(`x-text` = "model.click_count")
                )
              )
            )
          ),

          # Text inputs row
          bslib::card(
            bslib::card_header("mvu_text_input / mvu_password_input"),
            bslib::card_body(
              div(class = "row g-3",
                div(class = "col-md-4",
                  mvu_text_input("Name", msg = "set_name",
                    bind = "model.name", placeholder = "Your name")
                ),
                div(class = "col-md-4",
                  mvu_text_input("Email", msg = "set_email",
                    bind = "model.email", placeholder = "you@example.com")
                ),
                div(class = "col-md-4",
                  mvu_password_input("Password", msg = "set_password",
                    bind = "model.password", placeholder = "Secret")
                )
              )
            )
          ),

          # Textarea
          bslib::card(
            bslib::card_header("mvu_textarea_input"),
            bslib::card_body(
              mvu_textarea_input("Bio", msg = "set_bio",
                bind = "model.bio", rows = 3,
                placeholder = "Tell us about yourself...")
            )
          ),

          # Numeric + Slider row
          bslib::card(
            bslib::card_header("mvu_numeric_input / mvu_slider_input"),
            bslib::card_body(
              div(class = "row g-3",
                div(class = "col-md-6",
                  mvu_numeric_input("Age", msg = "set_age",
                    bind = "model.age", min = 0, max = 120, step = 1)
                ),
                div(class = "col-md-6",
                  mvu_slider_input("Volume", min = 0, max = 100,
                    msg = "set_volume", bind = "model.volume"),
                  tags$div(class = "text-muted small mt-1",
                    "Value: ", tags$span(`x-text` = "model.volume"))
                )
              )
            )
          ),

          # Select
          bslib::card(
            bslib::card_header("mvu_select_input"),
            bslib::card_body(
              div(class = "row g-3",
                div(class = "col-md-6",
                  mvu_select_input("Favorite Color",
                    choices = c(
                      "Red" = "red", "Blue" = "blue", "Green" = "green",
                      "Yellow" = "yellow", "Purple" = "purple"
                    ),
                    msg = "set_color", bind = "model.color")
                )
              )
            )
          ),

          # Checkbox + Checkbox Group
          bslib::card(
            bslib::card_header(
              "mvu_checkbox_input / mvu_checkbox_group_input"
            ),
            bslib::card_body(
              div(class = "row g-3",
                div(class = "col-md-6",
                  mvu_checkbox_input("I agree to the terms",
                    msg = "toggle_agree", bind = "model.agree")
                ),
                div(class = "col-md-6",
                  mvu_checkbox_group_input("Pizza Toppings",
                    choices = c(
                      "Cheese" = "cheese", "Peppers" = "peppers",
                      "Onions" = "onions", "Mushrooms" = "mushrooms"
                    ),
                    msg = "toggle_topping", bind = "model.toppings")
                )
              )
            )
          ),

          # Radio
          bslib::card(
            bslib::card_header("mvu_radio_input"),
            bslib::card_body(
              div(class = "row g-3",
                div(class = "col-md-6",
                  mvu_radio_input("Favorite Pet",
                    choices = c(
                      "Dog" = "dog", "Cat" = "cat",
                      "Fish" = "fish", "Bird" = "bird"
                    ),
                    msg = "set_pet", bind = "model.pet")
                ),
                div(class = "col-md-6",
                  mvu_radio_input("(inline)",
                    choices = c("A" = "a", "B" = "b", "C" = "c"),
                    msg = "set_pet", bind = "model.pet", inline = TRUE)
                )
              )
            )
          ),

          # Dates
          bslib::card(
            bslib::card_header("mvu_date_input / mvu_date_range_input"),
            bslib::card_body(
              div(class = "row g-3",
                div(class = "col-md-4",
                  mvu_date_input("Single Date", msg = "set_date",
                    bind = "model.date")
                ),
                div(class = "col-md-8",
                  mvu_date_range_input("Trip Dates",
                    msg_start = "set_depart", msg_end = "set_return",
                    bind_start = "model.depart",
                    bind_end = "model.return_date")
                )
              )
            )
          ),

          # File
          bslib::card(
            bslib::card_header("mvu_file_input"),
            bslib::card_body(
              div(class = "row g-3",
                div(class = "col-md-6",
                  mvu_file_input("Upload a file", msg = "upload_file")
                ),
                div(class = "col-md-6",
                  mvu_file_input("Images only", msg = "upload_file",
                    accept = "image/*", multiple = TRUE)
                )
              )
            )
          )
        ),

        # Right column: model state
        div(class = "col-lg-4",
          div(class = "sticky-top", style = "top: 1rem;",
            bslib::card(
              bslib::card_header(
                tags$strong("Model State"),
                tags$span(class = "text-muted small ms-2", "(live)")
              ),
              bslib::card_body(
                tags$pre(
                  class = "bg-dark text-light p-3 rounded",
                  style = "font-size: 0.85rem; max-height: 80vh; overflow: auto;",
                  tags$code(`x-text` = "JSON.stringify(model, null, 2)")
                )
              )
            )
          )
        )

      )
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

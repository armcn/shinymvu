library(shiny)
library(shinymvu)

# Demonstrates the two types of inputs in shinymvu:
#
# 1. MESSAGE INPUTS (buttons, selects, checkboxes, sliders):
#    Every interaction dispatches a message immediately.
#    Value comes from the model. Good for discrete actions.
#
# 2. LOCAL INPUTS (text fields, textareas):
#    Value lives in Alpine-local state, never overwritten by the server.
#    Sent to R either on debounce or on explicit action (button click).
#    Good for typing, where round-trip latency would be noticeable.

Msg <- mvu_enum(c(
  "set_search", "set_name", "set_bio",
  "save_profile", "set_color", "toggle_public", "set_font_size",
  "clear_all"
))

init <- function() {
  list(
    search = "",
    name = "",
    bio = "",
    color = "blue",
    public = TRUE,
    font_size = 16,
    saved_count = 0L
  )
}

update <- function(model, msg, value) {
  match_enum(Msg(msg),
    "set_search"    ~ list_set(model, search = value),
    "set_name"      ~ list_set(model, name = value),
    "set_bio"       ~ list_set(model, bio = value),
    "save_profile"  ~ mvu_result(
      list_set(model,
        name = value$name,
        bio = value$bio,
        saved_count = model$saved_count + 1L
      ),
      effect_notify("Profile saved!", type = "message", duration = 2)
    ),
    "set_color"     ~ list_set(model, color = value),
    "toggle_public" ~ list_set(model, public = isTRUE(value)),
    "set_font_size" ~ list_set(model, font_size = as.integer(value)),
    "clear_all"     ~ mvu_result(
      init(),
      effect_notify("All cleared", type = "message", duration = 2)
    )
  )
}

to_frontend <- function(model) {
  items <- c("Apples", "Bananas", "Cherries", "Dates", "Elderberry",
             "Figs", "Grapes", "Honeydew", "Kiwi", "Lemon")
  filtered <- if (nzchar(model$search)) {
    items[grepl(model$search, items, ignore.case = TRUE)]
  } else {
    items
  }
  list(
    search = model$search,
    name = model$name,
    bio = model$bio,
    color = model$color,
    public = model$public,
    font_size = model$font_size,
    saved_count = model$saved_count,
    filtered_items = as.list(filtered),
    item_count = length(filtered),
    bio_length = nchar(model$bio)
  )
}

# --- UI ---------------------------------------------------------------------

ui <- mvu_page(
  component = "app",
  extra_js = "
    profileName: '',
    profileBio: '',
    searchText: ''
  ",

  div(class = "container py-4", style = "max-width: 900px;",
    tags$h4("shinymvu input patterns", class = "mb-4"),

    bslib::layout_columns(
      col_widths = c(6, 6),

      # --- Left column: Local inputs (text) ---
      div(
        tags$h6(class = "text-body-secondary fw-semibold text-uppercase mb-3",
          style = "font-size: .75rem; letter-spacing: .05em;",
          "Local inputs (Alpine-owned state)"),

        # Search with debounced auto-send
        bslib::card(class = "mb-3",
          bslib::card_header(class = "fw-semibold", style = "font-size: .875rem;",
            "Live Search (debounced)"),
          bslib::card_body(
            tags$p(class = "text-body-secondary", style = "font-size: .8125rem;",
              "Typing sends to R after 300ms of inactivity. ",
              "R filters the list server-side and pushes results back."),
            mvu_text_local("Search fruits", local = "searchText",
              msg = "set_search", debounce = 300,
              placeholder = "Try typing 'grape'..."),
            div(class = "mt-2",
              tags$small(class = "text-body-secondary",
                `x-text` = "'Showing ' + model.item_count + ' of 10 items'"),
              tags$ul(class = "list-unstyled mt-1 mb-0",
                style = "font-size: .875rem;",
                tags$template(`x-for` = "item in model.filtered_items",
                  `x-bind:key` = "item",
                  tags$li(`x-text` = "item")
                )
              )
            )
          )
        ),

        # Profile form with explicit save
        bslib::card(
          bslib::card_header(class = "fw-semibold", style = "font-size: .875rem;",
            "Profile Form (send on save)"),
          bslib::card_body(
            tags$p(class = "text-body-secondary", style = "font-size: .8125rem;",
              "Text stays local. Nothing is sent to R until you click Save."),
            div(class = "mb-2",
              mvu_text_local("Display name", local = "profileName",
                placeholder = "Enter your name...")
            ),
            div(class = "mb-3",
              mvu_textarea_local("About you", local = "profileBio",
                rows = 2, placeholder = "Write a short bio...")
            ),
            div(class = "d-flex gap-2 align-items-center",
              mvu_button_expr("Save Profile", msg = "save_profile",
                value_expr = "{ name: profileName, bio: profileBio }",
                class = "btn btn-primary btn-sm"),
              tags$small(class = "text-body-secondary",
                `x-show` = "model.saved_count > 0",
                `x-text` = "'Saved ' + model.saved_count + ' time(s)'")
            ),

            div(class = "mt-3 border rounded-2 p-2",
              `x-show` = "model.name",
              style = "background: var(--bs-tertiary-bg); font-size: .8125rem;",
              tags$span(class = "text-body-secondary", "Server model: "),
              tags$span(class = "fw-semibold", `x-text` = "model.name"),
              tags$span(class = "text-body-secondary ms-2",
                `x-show` = "model.bio_length > 0",
                `x-text` = "'(' + model.bio_length + ' char bio)'")
            )
          )
        )
      ),

      # --- Right column: Message inputs (model-owned state) ---
      div(
        tags$h6(class = "text-body-secondary fw-semibold text-uppercase mb-3",
          style = "font-size: .75rem; letter-spacing: .05em;",
          "Message inputs (model-owned state)"),

        bslib::card(class = "mb-3",
          bslib::card_header(class = "fw-semibold", style = "font-size: .875rem;",
            "Theme Settings"),
          bslib::card_body(
            tags$p(class = "text-body-secondary", style = "font-size: .8125rem;",
              "Each interaction dispatches a message immediately. ",
              "Value comes from the model."),
            div(class = "mb-3",
              mvu_select("Accent color",
                choices = c("red", "blue", "green", "purple", "orange"),
                msg = "set_color",
                value_expr = "model.color")
            ),
            div(class = "mb-3",
              mvu_checkbox("Public profile",
                msg = "toggle_public",
                checked_expr = "model.public")
            ),
            div(class = "mb-3",
              mvu_slider("Font size", min = 12, max = 24,
                msg = "set_font_size",
                value_expr = "model.font_size")
            )
          )
        ),

        # Preview card
        bslib::card(class = "mb-3",
          bslib::card_header(class = "fw-semibold", style = "font-size: .875rem;",
            "Preview"),
          bslib::card_body(
            div(
              class = "rounded-2 p-3 text-white",
              `x-bind:style` = paste0(
                "'background:' + model.color + ';",
                "font-size:' + model.font_size + 'px;'"
              ),
              tags$span(`x-text` = "model.name || 'Your Name'"),
              tags$span(class = "ms-2 opacity-75",
                `x-show` = "model.public",
                "(public)")
            )
          )
        ),

        div(class = "text-end",
          mvu_button("Clear everything", msg = "clear_all",
            class = "btn btn-outline-danger btn-sm")
        )
      )
    )
  )
)

# --- Server -----------------------------------------------------------------

server <- function(input, output, session) {
  mvu_server(
    init = init,
    update = update,
    msg = Msg,
    to_frontend = to_frontend,
    component = "app",
    input = input, output = output, session = session
  )
}

shinyApp(ui, server)

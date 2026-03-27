library(shiny)
library(shinymvu)

# Elm-style controlled text inputs: every keystroke round-trips through R.
# This app lets you see exactly how the latency feels and uses devtools
# to show the message rate.

Msg <- mvu_enum(c(
  "set_first", "set_last", "set_bio",
  "reverse_first", "uppercase_last", "clear_all"
))

init <- function() {
  list(
    first_name = "",
    last_name = "",
    bio = ""
  )
}

update <- function(model, msg, value) {
  match_enum(Msg(msg),
    "set_first"      ~ list_set(model, first_name = value),
    "set_last"       ~ list_set(model, last_name = value),
    "set_bio"        ~ list_set(model, bio = value),
    "reverse_first"  ~ list_set(model,
      first_name = paste0(rev(strsplit(model$first_name, "")[[1]]),
                          collapse = "")),
    "uppercase_last" ~ list_set(model,
      last_name = toupper(model$last_name)),
    "clear_all"      ~ mvu_result(
      list_set(model, first_name = "", last_name = "", bio = ""),
      effect_notify("All fields cleared", type = "message", duration = 2)
    )
  )
}

to_frontend <- function(model) {
  full_name <- trimws(paste(model$first_name, model$last_name))
  list(
    first_name = model$first_name,
    last_name = model$last_name,
    bio = model$bio,
    greeting = if (nzchar(full_name)) paste0("Hello, ", full_name, "!") else "",
    bio_chars = nchar(model$bio),
    name_length = nchar(full_name)
  )
}

# --- UI ---------------------------------------------------------------------

ui <- bslib::page_fillable(
  theme = bslib::bs_theme(),
  padding = "1.5rem",

  tags$head(
    tags$style(HTML("
      [x-cloak]{display:none!important}
      .derived { transition: all .15s ease; }
    "))
  ),

  tags$h4("Controlled text inputs (Elm-style full loop)", class = "mb-3"),

  bslib::layout_columns(
    col_widths = c(6, 6),

    # Left: the form
    bslib::card(
      bslib::card_header("Form"),
      bslib::card_body(
        mvu_module_ui("form",

          div(class = "mb-3",
            mvu_text_input("First name", msg = "set_first",
              value_expr = "model.first_name",
              placeholder = "Type here...")
          ),

          div(class = "mb-3",
            mvu_text_input("Last name", msg = "set_last",
              value_expr = "model.last_name",
              placeholder = "Type here...")
          ),

          div(class = "mb-3",
            mvu_textarea("Bio", msg = "set_bio",
              value_expr = "model.bio", rows = 3,
              placeholder = "Write something longer to test sustained typing...")
          ),

          # Derived values (computed in R, returned via to_frontend)
          div(class = "derived border rounded-2 p-3 mb-3",
            style = "background: var(--bs-tertiary-bg); font-size: .875rem;",

            div(`x-show` = "model.greeting", class = "mb-2",
              tags$span(class = "fw-semibold", `x-text` = "model.greeting")
            ),

            div(class = "d-flex gap-4 text-body-secondary",
              tags$span(`x-text` = "'Name: ' + model.name_length + ' chars'"),
              tags$span(`x-text` = "'Bio: ' + model.bio_chars + ' chars'")
            )
          ),

          # Buttons that transform the text server-side
          div(class = "d-flex gap-2",
            mvu_button("Reverse first name", msg = "reverse_first",
              class = "btn btn-outline-primary btn-sm"),
            mvu_button("Uppercase last name", msg = "uppercase_last",
              class = "btn btn-outline-primary btn-sm"),
            mvu_button("Clear all", msg = "clear_all",
              class = "btn btn-outline-danger btn-sm")
          )
        )
      )
    ),

    # Right: devtools log
    bslib::card(
      bslib::card_header(
        class = "d-flex justify-content-between align-items-center",
        tags$span("Message Log"),
        uiOutput("msg_rate")
      ),
      bslib::card_body(
        class = "p-0",
        div(style = "max-height: 500px; overflow-y: auto;",
          uiOutput("log_entries")
        )
      )
    )
  )
)

# --- Server -----------------------------------------------------------------

server <- function(input, output, session) {

  runtime <- mvu_module_server("form",
    init = init,
    update = update,
    msg = Msg,
    to_frontend = to_frontend,
    devtools = TRUE
  )

  output$msg_rate <- renderUI({
    log <- runtime$log()
    n <- length(log$transitions)
    if (n < 2) return(tags$span(class = "text-body-secondary", style = "font-size: .75rem;",
      sprintf("%d msg", n)))

    first_ts <- log$transitions[[1]]$timestamp
    last_ts <- log$transitions[[n]]$timestamp
    span_secs <- as.double(difftime(last_ts, first_ts, units = "secs"))

    rate <- if (span_secs > 0) round(n / span_secs, 1) else n
    tags$span(class = "text-body-secondary", style = "font-size: .75rem;",
      sprintf("%d msgs | %.1f msg/s", n, rate))
  })

  output$log_entries <- renderUI({
    log <- runtime$log()
    n <- length(log$transitions)
    if (n == 0) {
      return(div(class = "p-3 text-body-secondary",
        "Start typing to see messages appear here in real time."))
    }

    show_n <- min(n, 50)
    recent <- log$transitions[seq(n, n - show_n + 1)]

    tagList(
      lapply(recent, function(t) {
        val_display <- if (is.null(t$value)) {
          "null"
        } else if (nchar(as.character(t$value)) > 30) {
          paste0(substr(as.character(t$value), 1, 30), "...")
        } else {
          as.character(t$value)
        }

        has_effects <- length(t$effects) > 0

        div(class = "px-3 py-1 border-bottom d-flex justify-content-between align-items-center",
          style = "font-size: .75rem;",
          div(
            tags$span(class = "fw-semibold me-2", t$type),
            tags$code(class = "text-body-secondary", val_display),
            if (has_effects) {
              tags$span(class = "badge bg-warning-subtle text-warning-emphasis ms-1",
                style = "font-size: .625rem;",
                sprintf("%d fx", length(t$effects)))
            }
          ),
          tags$span(class = "text-body-secondary",
            format(t$timestamp, "%H:%M:%OS3"))
        )
      })
    )
  })
}

shinyApp(ui, server)

library(shiny)
library(shinymvu)

# A counter app with time-travel debugging. Click the counter buttons
# to build up a message history, then drag the slider to scrub through
# every state the UI has been in.

Msg <- mvu_enum(c("increment", "decrement", "set", "reset"))

init <- function() list(count = 0)

update <- function(model, msg, value) {
  match_enum(Msg(msg),
    "increment" ~ list_set(model, count = model$count + 1),
    "decrement" ~ list_set(model, count = model$count - 1),
    "set"       ~ list_set(model, count = as.integer(value)),
    "reset"     ~ mvu_result(
      list_set(model, count = 0),
      effect_notify("Counter reset to zero", type = "message", duration = 2)
    )
  )
}

to_frontend <- function(model) {
  list(
    count = model$count,
    is_zero = model$count == 0,
    is_negative = model$count < 0
  )
}

# --- UI ---------------------------------------------------------------------

ui <- bslib::page_fillable(
  theme = bslib::bs_theme(),
  padding = "1.5rem",

  tags$head(
    tags$style(HTML("
      [x-cloak]{display:none!important}
      .tt-paused { opacity: .5; pointer-events: none; }
    "))
  ),

  tags$h4("shinymvu time-travel debugger", class = "mb-3"),

  bslib::layout_columns(
    col_widths = c(5, 7),

    # Left: the app
    bslib::card(
      bslib::card_header(
        class = "d-flex justify-content-between align-items-center",
        tags$span("Counter App"),
        uiOutput("paused_badge")
      ),
      bslib::card_body(
        div(id = "app_container",
          mvu_module_ui("counter",
            div(class = "text-center py-4",
              tags$h1(
                `x-text` = "model.count",
                class = "display-1 mb-3",
                `x-bind:class` = "{ 'text-danger': model.is_negative }"
              ),
              div(class = "d-flex gap-2 justify-content-center mb-3",
                mvu_button("\u2212", msg = "decrement",
                  class = "btn btn-outline-primary btn-lg",
                  style = "width: 3rem;"),
                mvu_button("+", msg = "increment",
                  class = "btn btn-primary btn-lg",
                  style = "width: 3rem;")
              ),
              div(class = "d-flex gap-2 justify-content-center",
                mvu_button("Reset", msg = "reset",
                  class = "btn btn-outline-secondary btn-sm",
                  `x-show` = "!model.is_zero"),
                mvu_button_expr("Set to 100", msg = "set",
                  value_expr = "100",
                  class = "btn btn-outline-secondary btn-sm")
              )
            )
          )
        )
      )
    ),

    # Right: devtools
    bslib::card(
      bslib::card_header(
        class = "d-flex justify-content-between align-items-center",
        tags$span("Time Travel"),
        div(
          uiOutput("travel_controls", inline = TRUE)
        )
      ),
      bslib::card_body(
        class = "d-flex flex-column gap-3 p-3",

        # Slider
        div(
          uiOutput("slider_ui")
        ),

        # Current step info
        uiOutput("step_info"),

        # Transition log
        div(style = "flex: 1; overflow-y: auto; max-height: 300px;",
          uiOutput("transition_list")
        )
      )
    )
  )
)

# --- Server -----------------------------------------------------------------

server <- function(input, output, session) {

  runtime <- mvu_module_server("counter",
    init = init,
    update = update,
    msg = Msg,
    to_frontend = to_frontend,
    devtools = TRUE
  )

  # --- Paused badge on counter card ---
  output$paused_badge <- renderUI({
    if (isTRUE(runtime$is_traveling())) {
      tags$span(class = "badge bg-warning text-dark", "PAUSED")
    }
  })

  # --- Dim the counter when time-traveling ---
  observe({
    traveling <- runtime$is_traveling()
    shinyjs_class <- if (traveling) "tt-paused" else ""
    session$sendCustomMessage("tt_class", shinyjs_class)
  })

  # --- Travel controls: Pause / Resume buttons ---
  output$travel_controls <- renderUI({
    log <- runtime$log()
    n <- length(log$transitions)
    traveling <- runtime$is_traveling()

    if (traveling) {
      actionButton("resume_btn", "Resume",
        class = "btn btn-sm btn-success", icon = icon("play"))
    } else if (n > 0) {
      actionButton("pause_btn", "Pause",
        class = "btn btn-sm btn-outline-warning", icon = icon("pause"))
    }
  })

  observeEvent(input$pause_btn, {
    log <- runtime$log()
    n <- length(log$transitions)
    runtime$travel_to(n)
  })

  observeEvent(input$resume_btn, {
    runtime$resume()
  })

  # --- Slider ---
  output$slider_ui <- renderUI({
    log <- runtime$log()
    n <- length(log$transitions)
    traveling <- runtime$is_traveling()

    if (n == 0) {
      return(tags$p(class = "text-body-secondary mb-0",
        "Click the counter buttons to build a history, then pause to time-travel."))
    }

    current_val <- if (traveling && !is.null(input$time_slider)) {
      input$time_slider
    } else {
      n
    }

    labels <- c("init", vapply(log$transitions, function(t) t$type, character(1)))

    tagList(
      div(class = "d-flex justify-content-between mb-1",
        style = "font-size: .75rem;",
        tags$span(class = "text-body-secondary", "init"),
        tags$span(class = "text-body-secondary", sprintf("step %d", n))
      ),
      sliderInput("time_slider", NULL,
        min = 0, max = n, value = current_val, step = 1,
        width = "100%",
        animate = if (traveling) {
          animationOptions(interval = 600, loop = FALSE)
        }
      )
    )
  })

  # --- Drive time-travel from slider ---
  observeEvent(input$time_slider, {
    slider_val <- input$time_slider

    if (isTRUE(runtime$is_traveling())) {
      runtime$travel_to(slider_val)
      return()
    }

    log <- runtime$log()
    n <- length(log$transitions)
    if (slider_val != n) {
      runtime$travel_to(slider_val)
    }
  })

  # --- Step info ---
  output$step_info <- renderUI({
    log <- runtime$log()
    n <- length(log$transitions)
    traveling <- runtime$is_traveling()

    if (n == 0) return(NULL)

    step <- if (traveling && !is.null(input$time_slider)) {
      as.integer(input$time_slider)
    } else {
      n
    }

    if (step == 0) {
      div(class = "border rounded-2 p-2", style = "font-size: .8125rem;",
        div(class = "fw-semibold", "Step 0: Initial State"),
        div(class = "text-body-secondary",
          sprintf("count = %d", init()$count))
      )
    } else {
      t <- log$transitions[[step]]
      has_effects <- length(t$effects) > 0
      value_str <- if (is.null(t$value)) "null" else as.character(t$value)
      div(class = "border rounded-2 p-2", style = "font-size: .8125rem;",
        div(class = "d-flex justify-content-between",
          tags$span(class = "fw-semibold",
            sprintf("Step %d: %s", step, t$type)),
          tags$span(class = "text-body-secondary",
            format(t$timestamp, "%H:%M:%OS3"))
        ),
        div(class = "text-body-secondary",
          sprintf("value: %s | count: %s \u2192 %s",
            value_str, t$model_before$count, t$model_after$count)
        ),
        if (has_effects) {
          div(class = "mt-1",
            lapply(t$effects, function(e) {
              tags$span(
                class = "badge bg-warning-subtle text-warning-emphasis me-1",
                sprintf("effect: %s", e$kind)
              )
            })
          )
        }
      )
    }
  })

  # --- Transition list ---
  output$transition_list <- renderUI({
    log <- runtime$log()
    n <- length(log$transitions)
    traveling <- runtime$is_traveling()

    if (n == 0) return(NULL)

    current_step <- if (traveling && !is.null(input$time_slider)) {
      as.integer(input$time_slider)
    } else {
      n
    }

    tagList(
      lapply(rev(seq_len(n)), function(i) {
        t <- log$transitions[[i]]
        is_current <- i == current_step
        is_future <- i > current_step
        has_effects <- length(t$effects) > 0

        border_class <- if (is_current) {
          "border-primary"
        } else if (is_future) {
          "border-dashed"
        } else {
          ""
        }
        opacity <- if (is_future) "opacity: .4;" else ""

        div(
          class = paste("border rounded-2 p-2 mb-1", border_class),
          style = paste0("font-size: .75rem; cursor: pointer;", opacity),
          `data-step` = i,
          onclick = sprintf(
            "Shiny.setInputValue('jump_to', %d, {priority: 'event'})", i
          ),
          div(class = "d-flex justify-content-between",
            tags$span(class = if (is_current) "fw-bold" else "fw-semibold",
              sprintf("#%d %s", i, t$type)),
            tags$span(class = "text-body-secondary",
              sprintf("%s\u2192%s", t$model_before$count, t$model_after$count))
          ),
          if (has_effects) {
            tags$span(
              class = "badge bg-warning-subtle text-warning-emphasis",
              style = "font-size: .625rem;",
              sprintf("%d effect", length(t$effects))
            )
          }
        )
      })
    )
  })

  # --- Jump to step by clicking a transition ---
  observeEvent(input$jump_to, {
    if (isTRUE(runtime$is_traveling())) {
      updateSliderInput(session, "time_slider", value = input$jump_to)
    } else {
      runtime$travel_to(input$jump_to)
      updateSliderInput(session, "time_slider", value = input$jump_to)
    }
  })

  # --- Toggle app_container class for visual dimming ---
  observe({
    traveling <- runtime$is_traveling()
    session$sendCustomMessage("tt_class", traveling)
  })
}

# Inject a tiny script to handle the dimming custom message
ui_final <- tagList(
  ui,
  tags$script(HTML('
    Shiny.addCustomMessageHandler("tt_class", function(traveling) {
      var el = document.getElementById("app_container");
      if (el) {
        if (traveling) {
          el.classList.add("tt-paused");
        } else {
          el.classList.remove("tt-paused");
        }
      }
    });
  '))
)

shinyApp(ui_final, server)

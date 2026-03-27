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
      .tt-item { transition: border-color .15s, opacity .15s; }
      .tt-item[data-future='true'] { opacity: .4; }
      .tt-item.tt-current { border-color: var(--bs-primary) !important; }
      .tt-item.tt-current .tt-label { font-weight: 700; }
    ")),
    tags$script(HTML("
      Shiny.addCustomMessageHandler('tt_highlight', function(step) {
        document.querySelectorAll('.tt-item').forEach(function(el) {
          var s = parseInt(el.dataset.step);
          el.classList.toggle('tt-current', s === step);
          el.dataset.future = s > step ? 'true' : 'false';
        });
      });
    "))
  ),

  tags$h4("shinymvu time-travel debugger", class = "mb-3"),

  bslib::layout_columns(
    col_widths = c(5, 7),

    # Left: the app
    bslib::card(
      bslib::card_header("Counter App"),
      bslib::card_body(
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
    ),

    # Right: devtools
    bslib::card(
      bslib::card_header(
        class = "d-flex justify-content-between align-items-center",
        tags$span("Time Travel"),
        div(uiOutput("travel_controls", inline = TRUE))
      ),
      bslib::card_body(
        class = "d-flex flex-column gap-3 p-3",
        div(uiOutput("slider_ui")),
        uiOutput("step_info"),
        div(
          style = "flex: 1; overflow-y: auto; max-height: 300px;",
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

  # --- Travel controls: Resume button only when traveling ---
  output$travel_controls <- renderUI({
    traveling <- runtime$is_traveling()
    if (traveling) {
      actionButton("resume_btn", "Resume",
        class = "btn btn-sm btn-success", icon = icon("play"))
    }
  })

  observeEvent(input$resume_btn, {
    runtime$resume()
  })

  # --- Slider: only re-renders when log grows (new messages) ---
  output$slider_ui <- renderUI({
    log <- runtime$log()
    n <- length(log$transitions)

    if (n == 0) {
      return(tags$p(class = "text-body-secondary mb-0",
        "Click the counter buttons to build a history, then drag the slider to time-travel."))
    }

    tagList(
      div(class = "d-flex justify-content-between mb-1",
        style = "font-size: .75rem;",
        tags$span(class = "text-body-secondary", "init"),
        tags$span(class = "text-body-secondary", sprintf("step %d", n))
      ),
      sliderInput("time_slider", NULL,
        min = 0, max = n, value = n, step = 1,
        width = "100%",
        animate = animationOptions(interval = 600, loop = FALSE)
      )
    )
  })

  # --- Drive time-travel from slider ---
  observeEvent(input$time_slider, {
    slider_val <- input$time_slider

    if (isTRUE(runtime$is_traveling())) {
      runtime$travel_to(slider_val)
      session$sendCustomMessage("tt_highlight", slider_val)
      return()
    }

    log <- runtime$log()
    n <- length(log$transitions)
    if (slider_val != n) {
      runtime$travel_to(slider_val)
      session$sendCustomMessage("tt_highlight", slider_val)
    }
  })

  # --- Step info: re-renders on slider move (small, no visible flash) ---
  output$step_info <- renderUI({
    log <- runtime$log()
    n <- length(log$transitions)
    if (n == 0) return(NULL)

    step <- input$time_slider
    if (is.null(step)) return(NULL)
    step <- as.integer(step)

    if (step == 0) {
      div(class = "border rounded-2 p-2", style = "font-size: .8125rem;",
        div(class = "fw-semibold", "Step 0: Initial State"),
        div(class = "text-body-secondary",
          sprintf("count = %d", init()$count))
      )
    } else if (step <= n) {
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

  # --- Transition list: only re-renders when log grows, not on slider ---
  output$transition_list <- renderUI({
    log <- runtime$log()
    n <- length(log$transitions)
    if (n == 0) return(NULL)

    tagList(
      lapply(rev(seq_len(n)), function(i) {
        t <- log$transitions[[i]]
        has_effects <- length(t$effects) > 0

        div(
          class = "tt-item border rounded-2 p-2 mb-1",
          style = "font-size: .75rem; cursor: pointer;",
          `data-step` = i,
          `data-future` = "false",
          onclick = sprintf(
            "Shiny.setInputValue('jump_to', %d, {priority: 'event'})", i
          ),
          div(class = "d-flex justify-content-between",
            tags$span(class = "tt-label fw-semibold",
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
    if (!isTRUE(runtime$is_traveling())) {
      runtime$travel_to(input$jump_to)
    } else {
      runtime$travel_to(input$jump_to)
    }
    updateSliderInput(session, "time_slider", value = input$jump_to)
    session$sendCustomMessage("tt_highlight", input$jump_to)
  })

  # When resuming, push highlight to latest
  observe({
    if (!isTRUE(runtime$is_traveling())) {
      log <- runtime$log()
      n <- length(log$transitions)
      session$sendCustomMessage("tt_highlight", n)
    }
  })
}

shinyApp(ui, server)

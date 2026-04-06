format_leaf_value <- function(x) {
  if (is.null(x)) {
    "null"
  } else if (is.logical(x) && length(x) == 1L) {
    tolower(as.character(x))
  } else if (is.character(x) && length(x) == 1L) {
    sprintf('"%s"', x)
  } else if (is.atomic(x) && length(x) == 1L) {
    as.character(x)
  } else if (is.atomic(x) && length(x) > 1L) {
    vals <- vapply(x, function(v) {
      if (is.character(v)) {
        sprintf('"%s"', v)
      } else if (is.logical(v)) {
        tolower(as.character(v))
      } else {
        as.character(v)
      }
    }, character(1))
    sprintf("[%s]", paste(vals, collapse = ", "))
  } else {
    as.character(x)
  }
}

value_css_class <- function(x) {
  if (is.null(x)) {
    "mvu-dbg-null"
  } else if (is.logical(x)) {
    "mvu-dbg-bool"
  } else if (is.numeric(x)) {
    "mvu-dbg-num"
  } else if (is.character(x)) {
    "mvu-dbg-str"
  } else {
    ""
  }
}

model_tree_html <- function(obj, name = NULL) {
  if (is.list(obj) && length(obj) > 0) {
    nms <- names(obj)
    has_names <- !is.null(nms)
    if (!has_names) nms <- as.character(seq_along(obj) - 1L)

    children <- mapply(function(key, val) {
      model_tree_html(val, name = key)
    }, nms, obj, SIMPLIFY = FALSE, USE.NAMES = FALSE)

    label <- if (!is.null(name)) name else "model"
    count_str <- if (has_names) {
      sprintf("{%d}", length(obj))
    } else {
      sprintf("[%d]", length(obj))
    }

    tags$details(
      class = "mvu-dbg-node",
      tags$summary(
        tags$span(class = "mvu-dbg-key", label),
        tags$span(class = "mvu-dbg-count", count_str)
      ),
      div(class = "mvu-dbg-children", children)
    )
  } else if (is.list(obj) && length(obj) == 0) {
    div(
      class = "mvu-dbg-leaf",
      if (!is.null(name)) tags$span(class = "mvu-dbg-key", paste0(name, ": ")),
      tags$span(
        class = "mvu-dbg-null",
        if (is.null(names(obj))) "[]" else "{}"
      )
    )
  } else {
    val_class <- value_css_class(obj)
    div(
      class = "mvu-dbg-leaf",
      if (!is.null(name)) tags$span(class = "mvu-dbg-key", paste0(name, ": ")),
      tags$span(
        class = paste("mvu-dbg-val", val_class),
        format_leaf_value(obj)
      )
    )
  }
}

#' Debugger Overlay UI
#'
#' Creates the time-travel debugger overlay panel. Injected automatically
#' via [shiny::insertUI()] when `debug = TRUE` is set on [mvu_module_server()].
#'
#' @param id Module ID for the debugger.
#'
#' @return An [htmltools::tagList()] containing the debugger overlay.
#'
#' @keywords internal
debugger_ui <- function(id) {
  ns <- NS(id)

  tagList(
    debugger_dep(),
    div(
      class = "mvu-dbg",
      tags$button(
        id = ns("tab"),
        class = "mvu-dbg-tab",
        onclick = sprintf("shinymvu.toggleDebugger('%s','%s')", ns("panel"), ns("tab")),
        tags$span("shinymvu"),
        shiny::uiOutput(ns("badge"), inline = TRUE)
      ),
      div(
        id = ns("panel"),
        class = "mvu-dbg-panel",
        style = "display: none;",
        div(
          class = "mvu-dbg-header",
          shiny::uiOutput(ns("nav"), inline = TRUE),
          shiny::uiOutput(ns("resume_btn"), inline = TRUE),
          tags$button(
            class = "mvu-dbg-action",
            onclick = sprintf(
              "shinymvu.importSession('%s','%s')", ns("import_file"), ns("import_data")
            ),
            "Import"
          ),
          shiny::downloadLink(ns("export"), "Export",
            class = "mvu-dbg-action",
            style = "text-decoration: none;"
          ),
          tags$button(
            id = ns("expand_btn"),
            class = "mvu-dbg-action",
            onclick = sprintf("shinymvu.expandDebugger('%s','%s')", ns("panel"), ns("expand_btn")),
            "Expand"
          ),
          tags$button(
            class = "mvu-dbg-close",
            onclick = sprintf(
              "shinymvu.toggleDebugger('%s','%s')", ns("panel"), ns("tab")
            ),
            "\u00d7"
          ),
          tags$input(
            type = "file", id = ns("import_file"),
            accept = ".json", style = "display:none;"
          )
        ),
        div(
          class = "mvu-dbg-body",
          div(
            class = "mvu-dbg-list",
            shiny::uiOutput(ns("msg_list"))
          ),
          div(
            id = ns("model_section"),
            class = "mvu-dbg-model-section",
            shiny::uiOutput(ns("model_view"))
          )
        ),
        tags$script(sprintf(
          "setTimeout(function(){shinymvu.initModelPersistence('%s');},0);",
          ns("model_section")
        ))
      )
    )
  )
}

#' Debugger Server Module
#'
#' Wires up the time-travel debugger to an MVU runtime. Used internally
#' when `debug = TRUE` is set on [mvu_module_server()].
#'
#' @param id Module ID matching the [debugger_ui()] call.
#' @param runtime The runtime list produced when `debug = TRUE`.
#' @param init The init function used by the MVU component.
#'
#' @keywords internal
debugger_server <- function(id, runtime, init) {
  moduleServer(id, function(input, output, session) {
    current_step <- reactiveVal(NULL)

    output$badge <- shiny::renderUI({
      log <- runtime$log()
      n <- length(log$transitions)
      if (n > 0) tags$span(class = "mvu-dbg-badge", n)
    })

    # -- Navigation: back / forward / step label -------------------------------

    output$nav <- shiny::renderUI({
      log <- runtime$log()
      n <- length(log$transitions)
      step <- current_step()
      if (is.null(step)) step <- n
      if (n == 0) {
        return(NULL)
      }

      back_disabled <- if (step <= 0) NA else NULL
      fwd_disabled <- if (step >= n) NA else NULL

      div(
        class = "mvu-dbg-nav",
        tags$button(
          class = "mvu-dbg-nav-btn",
          disabled = back_disabled,
          onclick = sprintf(
            "Shiny.setInputValue('%s', Date.now(), {priority: 'event'})",
            session$ns("back")
          ),
          "\u25c0"
        ),
        tags$span(
          class = "mvu-dbg-nav-label",
          sprintf("%d/%d", step, n)
        ),
        tags$button(
          class = "mvu-dbg-nav-btn",
          disabled = fwd_disabled,
          onclick = sprintf(
            "Shiny.setInputValue('%s', Date.now(), {priority: 'event'})",
            session$ns("fwd")
          ),
          "\u25b6"
        )
      )
    })

    observeEvent(input$back, {
      step <- current_step()
      if (!is.null(step) && step > 0) {
        new_step <- step - 1L
        current_step(new_step)
        runtime$travel_to(new_step)
      }
    })

    observeEvent(input$fwd, {
      step <- current_step()
      log <- runtime$log()
      n <- length(log$transitions)
      if (!is.null(step) && step < n) {
        new_step <- step + 1L
        current_step(new_step)
        runtime$travel_to(new_step)
      }
    })

    # -- Resume ----------------------------------------------------------------

    output$resume_btn <- shiny::renderUI({
      if (isTRUE(runtime$is_traveling())) {
        tags$button(
          class = "mvu-dbg-resume",
          onclick = sprintf(
            "Shiny.setInputValue('%s', Date.now(), {priority: 'event'})",
            session$ns("resume")
          ),
          "Resume"
        )
      }
    })

    observeEvent(input$resume, {
      runtime$resume()
      log <- runtime$log()
      current_step(length(log$transitions))
    })

    # -- Message list ----------------------------------------------------------

    output$msg_list <- shiny::renderUI({
      log <- runtime$log()
      n <- length(log$transitions)
      step <- current_step()
      if (is.null(step)) step <- n

      if (n == 0) {
        return(div(
          class = "mvu-dbg-empty",
          "Interact with the app to build a message history."
        ))
      }

      items <- lapply(rev(seq_len(n)), function(i) {
        t <- log$transitions[[i]]
        classes <- "mvu-dbg-item"
        if (i == step) classes <- paste(classes, "mvu-dbg-item-current")
        if (i > step) classes <- paste(classes, "mvu-dbg-item-future")

        value_tag <- if (!is.null(t$value)) {
          tags$span(class = "mvu-dbg-value", as.character(t$value))
        }

        div(
          class = classes,
          onclick = sprintf(
            "Shiny.setInputValue('%s', %d, {priority: 'event'})",
            session$ns("jump"), i
          ),
          div(
            tags$span(
              class = "mvu-dbg-type",
              sprintf("%d. %s", i, t$type)
            ),
            value_tag
          )
        )
      })

      init_classes <- "mvu-dbg-item mvu-dbg-init"
      if (step == 0) {
        init_classes <- paste(init_classes, "mvu-dbg-item-current")
      }
      init_row <- div(
        class = init_classes,
        onclick = sprintf(
          "Shiny.setInputValue('%s', 0, {priority: 'event'})",
          session$ns("jump")
        ),
        tags$span("0. init")
      )

      tagList(items, init_row)
    })

    observeEvent(input$jump, {
      step <- as.integer(input$jump)
      current_step(step)
      runtime$travel_to(step)
    })

    observe({
      log <- runtime$log()
      n <- length(log$transitions)
      if (!isTRUE(runtime$is_traveling())) {
        current_step(n)
      }
    })

    # -- Model viewer ----------------------------------------------------------

    output$model_view <- shiny::renderUI({
      log <- runtime$log()
      step <- current_step()
      n <- length(log$transitions)
      if (is.null(step)) step <- n

      model_obj <- runtime$model_at(step)

      div(
        class = "mvu-dbg-model-wrap",
        div(
          class = "mvu-dbg-step-label",
          if (step == 0) "Model at init" else sprintf("Model at step %d", step)
        ),
        model_tree_html(model_obj)
      )
    })

    # -- Export / Import -------------------------------------------------------

    output$export <- shiny::downloadHandler(
      filename = function() {
        sprintf(
          "shinymvu-session-%s.json",
          format(Sys.time(), "%Y%m%d-%H%M%S")
        )
      },
      content = function(file) {
        log <- isolate(runtime$log())
        messages <- lapply(log$transitions, function(t) {
          list(type = as.character(t$type), value = t$value)
        })
        jsonlite::write_json(
          list(app = "shinymvu", version = 1L, messages = messages),
          file,
          auto_unbox = TRUE, pretty = TRUE
        )
      }
    )

    observeEvent(input$import_data, {
      tryCatch(
        {
          data <- jsonlite::fromJSON(input$import_data,
            simplifyVector = FALSE
          )
          if (!is.null(data$messages)) {
            runtime$import_log(data$messages)
          }
        },
        error = function(e) {
          warning("shinymvu debugger: failed to import session: ", e$message)
        }
      )
    })

    shiny::outputOptions(output, "msg_list", suspendWhenHidden = FALSE)
    shiny::outputOptions(output, "badge", suspendWhenHidden = FALSE)
    shiny::outputOptions(output, "resume_btn", suspendWhenHidden = FALSE)
    shiny::outputOptions(output, "model_view", suspendWhenHidden = FALSE)
    shiny::outputOptions(output, "nav", suspendWhenHidden = FALSE)
  })
}

# -- Internal: htmlDependency -------------------------------------------------

debugger_dep <- function() {
  htmltools::htmlDependency(
    name = "shinymvu-debugger",
    version = as.character(utils::packageVersion("shinymvu")),
    package = "shinymvu",
    src = "www/shinymvu",
    script = "debugger.js",
    stylesheet = "debugger.css"
  )
}

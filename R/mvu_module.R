mvu_module_ui <- function(id, ...) {
  ns <- NS(id)
  config <- make_alpine_config(ns)

  tagList(
    shinymvu_dep(),
    alpine_dep(),
    shinymvu_register(config),
    div(`x-data` = config$component, ...)
  )
}

mvu_module_server <- function(id, init, update) {
  moduleServer(id, \(input, output, session) {
    ns <- session$ns
    config <- make_alpine_config(ns)

    model <- reactiveVal(init())

    observe({
      session$sendCustomMessage(config$model_channel, model())
    })

    observe({
      model() |>
        update(input$msg_id) |>
        model()
    }) |>
      bindEvent(input$msg_id)
  })
}

make_alpine_config <- function(ns) {
  list(
    component = gsub("-", "_", ns("mvu_module")),
    model_channel = ns("model_channel"),
    msg_id = ns("msg_id")
  )
}

shinymvu_register <- function(config) {
  tags$script(
    paste0(
      "shinymvu.register(",
      jsonlite::toJSON(config, auto_unbox = TRUE),
      ");"
    )
  )
}

shinymvu_dep <- function() {
  htmltools::htmlDependency(
    name = "shinymvu",
    version = "0.0.1",
    package = "shinymvu",
    src = "www",
    script = "shinymvu.js",
    stylesheet = "shinymvu.css"
  )
}

alpine_dep <- function() {
  htmltools::htmlDependency(
    name = "alpinejs",
    version = "3",
    src = c(href = "https://cdn.jsdelivr.net/npm/alpinejs@3/dist"),
    script = list(src = "cdn.min.js", defer = NA)
  )
}

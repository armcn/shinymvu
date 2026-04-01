mvu_module_ui <- function(id, ...) {
  tagList(
    shinymvu_dep(),
    alpine_dep(),
    div(
      `x-data` = "module",
      ...
    )
  )
}

mvu_module_server <- function(id, init, update) {
  moduleServer(id, \(input, output, session) {
    ns <- session$ns

    model <- reactiveVal(init())

    observe({
      session$sendCustomMessage("model_channel", model())
    })

    observe({
      model() |>
        update(input$msg) |>
        model()
    }) |>
      bindEvent(input$msg)
  })
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

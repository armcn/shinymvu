#' Create an MVU Page with Alpine.js
#'
#' Wraps UI content with the Alpine.js CDN script, the client-server bridge
#' JavaScript, and x-cloak CSS. Prefer [mvu_module_ui()] inside a standard
#' Shiny page function instead.
#'
#' @param ... UI elements to include inside the Alpine.js `x-data` container.
#' @param component Alpine.js component name.
#' @param theme A [bslib::bs_theme()] object.
#' @param extra_js Optional JavaScript string for the Alpine.js component.
#' @param extra_channels Named list of additional Shiny message handlers.
#'
#' @return A [bslib::page_fillable()] UI definition.
#'
#' @keywords internal
mvu_page <- function(..., component = "mvu",
                     theme = bslib::bs_theme(),
                     extra_js = NULL, extra_channels = NULL) {
  bslib::page_fillable(
    theme = theme,
    padding = 0,
    shinymvu_dep(),
    alpine_dep(),
    mvu_bridge_js(component, extra_js = extra_js, extra_channels = extra_channels),
    div(`x-data` = component, `x-cloak` = NA, ...)
  )
}

#' Generate Bridge Configuration Script
#'
#' Creates a `<script>` tag that calls `shinymvu.register()` with
#' per-component configuration. The static bridge logic lives in
#' `inst/www/shinymvu/bridge.js` and is loaded via an `htmlDependency`.
#'
#' @param component Character string naming the Alpine.js component.
#' @param extra_js Optional JavaScript string for additional component
#'   properties.
#' @param extra_channels A named list mapping channel names to JavaScript
#'   handler code strings.
#'
#' @return An [htmltools::tags]`$script` element.
#'
#' @keywords internal
mvu_bridge_js <- function(component = "mvu", extra_js = NULL,
                          extra_channels = NULL) {
  js_name <- gsub("-", "_", component)
  model_channel <- paste0(component, "__model")
  msg_id <- paste0(component, "__msg")

  config_parts <- c(
    sprintf('name: "%s"', js_name),
    sprintf('modelChannel: "%s"', model_channel),
    sprintf('msgId: "%s"', msg_id)
  )

  if (!is.null(extra_channels)) {
    handler_entries <- vapply(names(extra_channels), function(ch) {
      sprintf('"%s": function(data) { %s }', ch, extra_channels[[ch]])
    }, character(1))
    config_parts <- c(
      config_parts,
      sprintf("handlers: {%s}", paste(handler_entries, collapse = ", "))
    )
  }

  if (!is.null(extra_js)) {
    config_parts <- c(config_parts, sprintf("extend: {%s}", extra_js))
  }

  config_js <- paste(config_parts, collapse = ", ")
  tags$script(HTML(sprintf("shinymvu.register({%s});", config_js)))
}

# -- Internal: htmlDependency objects ------------------------------------------

shinymvu_dep <- function() {
  htmltools::htmlDependency(
    name = "shinymvu",
    version = as.character(utils::packageVersion("shinymvu")),
    package = "shinymvu",
    src = "www/shinymvu",
    script = "bridge.js",
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

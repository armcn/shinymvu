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
  config <- make_alpine_config(component)

  extras <- character(0)

  if (!is.null(extra_channels)) {
    handler_entries <- vapply(names(extra_channels), function(ch) {
      sprintf('"%s": function(data) { %s }', ch, extra_channels[[ch]])
    }, character(1))
    extras <- c(extras, sprintf('"handlers": {%s}', paste(handler_entries, collapse = ", ")))
  }

  if (!is.null(extra_js)) {
    extras <- c(extras, sprintf('"extend": {%s}', extra_js))
  }

  config_json <- jsonlite::toJSON(config, auto_unbox = TRUE)

  if (length(extras) > 0) {
    extras_str <- paste(extras, collapse = ", ")
    config_json <- sub("\\}$", paste0(", ", extras_str, "}"), config_json)
  }

  tags$script(HTML(paste0("shinymvu.register(", config_json, ");")))
}

make_alpine_config <- function(component) {
  list(
    component = gsub("-", "_", component),
    model_channel = paste0(component, "__model"),
    msg_id = paste0(component, "__msg")
  )
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

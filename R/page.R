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
    tags$head(
      tags$style(HTML("[x-cloak]{display:none!important}")),
      mvu_bridge_js(component, extra_js = extra_js, extra_channels = extra_channels),
      tags$script(
        src = "https://cdn.jsdelivr.net/npm/alpinejs@3/dist/cdn.min.js",
        defer = NA
      )
    ),
    div(`x-data` = component, `x-cloak` = NA, ...)
  )
}

#' Generate Bridge JavaScript for the MVU Runtime
#'
#' Creates a `<script>` tag that registers an Alpine.js component with
#' Shiny message handlers. The bridge connects the client-side Alpine
#' reactive model to the server-side MVU runtime loop.
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

  extra_handlers <- ""
  if (!is.null(extra_channels)) {
    extra_handlers <- paste(vapply(names(extra_channels), function(ch) {
      sprintf(
        'Shiny.addCustomMessageHandler("%s", function(data) { %s });',
        ch, extra_channels[[ch]]
      )
    }, character(1)), collapse = "\n")
  }

  js <- sprintf('
    document.addEventListener("alpine:init", function() {
      Alpine.data("%s", function() {
        return {
          model: {},
          init: function() {
            var self = this;
            Shiny.addCustomMessageHandler("%s", function(data) {
              self.model = data;
            });
            %s
          },
          send: function(type, value) {
            Shiny.setInputValue("%s",
              { type: type, value: value === undefined ? null : value },
              { priority: "event" }
            );
          },
          shinySet: function(name) {
            Shiny.setInputValue(name, Date.now(), { priority: "event" });
          }
          %s
        };
      });
    });
  ', js_name, model_channel, extra_handlers, msg_id,
  if (!is.null(extra_js)) paste0(",\n", extra_js) else "")
  tags$script(HTML(js))
}

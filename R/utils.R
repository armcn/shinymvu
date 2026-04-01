validate_icon <- function(icon) {
  if (!is_tag_like(icon)) {
    rlang::warn(
      c(
        "It appears that a non-HTML value was provided to `icon`.",
        i = "Try using a `shiny::icon()` (or an equivalent) to get an icon."
      )
    )
  }
  icon
}

is_tag <- function(x) {
  inherits(x, "shiny.tag")
}

is_html <- function(x) {
  isTRUE(attr(x, "html"))
}

is_tag_like <- function(x, strict = FALSE) {
  is_tag(x) ||
  is_html(x) ||
  is_tag_list(x, strict = strict)
}

is_tag_list <- function(x, strict = TRUE) {
  if (strict) {
    inherits(x, "shiny.tag.list")
  } else {
    is.list(x) && purrr::every(x, \(el) is_tag_like(el, strict = FALSE))
  }
}

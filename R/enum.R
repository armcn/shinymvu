#' Create an Enum Type for Message Dispatching
#'
#' Returns a factory function that creates validated enum values. Use this to
#' define the set of valid message types for your application, ensuring that
#' only known messages are dispatched.
#'
#' @param values A character vector of valid enum values.
#'
#' @return A function that takes a single character string and returns a
#'   validated `mvu_enum` object. Throws an error if the value is not in the
#'   set of valid values.
#'
#' @examples
#' Msg <- mvu_enum(c("increment", "decrement", "reset"))
#' Msg("increment")
#'
#' @export
mvu_enum <- function(values) {
  force(values)
  function(x) {
    if (!x %in% values) {
      stop(sprintf(
        "Unknown message '%s'. Valid: %s",
        x,
        paste(values, collapse = ", ")
      ))
    }
    structure(factor(x, levels = values), class = c("mvu_enum", "factor"))
  }
}

#' Exhaustive Pattern Matching on Enum Values
#'
#' Matches a `mvu_enum` value against a set of cases defined using formula
#' syntax. Without `.default`, all enum levels must be handled, ensuring
#' exhaustive matching at runtime.
#'
#' @param enum A `mvu_enum` object created by a factory from [mvu_enum()].
#' @param ... Formulas of the form `"value" ~ expression` or
#'   `c("a", "b") ~ expression`. The left-hand side specifies which enum
#'   values to match; the right-hand side is the expression to evaluate.
#' @param .default An optional default expression evaluated when no case
#'   matches. When omitted, all enum levels must have a corresponding case.
#'
#' @return The result of evaluating the matched expression.
#'
#' @examples
#' Msg <- mvu_enum(c("increment", "decrement", "reset"))
#' model <- list(count = 5)
#'
#' match_enum(Msg("increment"),
#'   "increment" ~ list_set(model, count = model$count + 1),
#'   "decrement" ~ list_set(model, count = model$count - 1),
#'   "reset"     ~ list_set(model, count = 0)
#' )
#'
#' match_enum(Msg("reset"),
#'   "reset" ~ list_set(model, count = 0),
#'   .default = model
#' )
#'
#' @export
match_enum <- function(enum, ..., .default) {
  cases <- list(...)
  env <- parent.frame()
  parsed <- unlist(lapply(cases, function(f) {
    lhs <- eval(f_lhs(f))
    rhs <- f_rhs(f)
    setNames(rep(list(rhs), length(lhs)), lhs)
  }), recursive = FALSE)

  lvls <- levels(enum)
  invalid <- setdiff(names(parsed), lvls)
  if (length(invalid) > 0) {
    stop(sprintf("Unknown levels: %s", paste(invalid, collapse = ", ")))
  }

  if (missing(.default) && !setequal(names(parsed), lvls)) {
    missing_lvls <- setdiff(lvls, names(parsed))
    stop(sprintf("Unhandled messages: %s", paste(missing_lvls, collapse = ", ")))
  }

  key <- as.character(enum)
  expr <- parsed[[key]]
  if (is.null(expr)) {
    if (missing(.default)) stop(sprintf("No match for '%s'", key))
    return(eval(.default, envir = env))
  }
  eval(expr, envir = env)
}

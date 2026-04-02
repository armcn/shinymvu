#' @export
prepare_model <- function(model) {
  validate_model(model)
  transform_model(model)
}

transform_model <- function(x) {
  if (is.data.frame(x)) {
    df_to_rows(x)
  } else if (is.list(x)) {
    purrr::map(x, transform_model)
  } else if (inherits(x, "POSIXt")) {
    format(x, "%Y-%m-%dT%H:%M:%S%z")
  } else if (inherits(x, "Date")) {
    format(x)
  } else if (inherits(x, "factor")) {
    as.character(x)
  } else {
    x
  }
}

df_to_rows <- function(df) {
  cols <- purrr::map(df, transform_model)
  purrr::map(seq_len(nrow(df)), \(i) purrr::map(cols, i))
}

validate_model <- function(x, path = "model") {
  if (is.null(x) || is.atomic(x)) {
    invisible()
  } else if (is.data.frame(x)) {
    purrr::iwalk(x, \(col, name) validate_model(col, paste0(path, "$", name)))
  } else if (is.list(x)) {
    purrr::iwalk(x, \(val, name) validate_model(val, paste0(path, "$", name)))
  } else {
    rlang::abort(c(
      paste0("`", path, "` is a ", class(x)[[1]], "."),
      i = "The model can only contain lists, vectors, data frames, and NULL."
    ))
  }
}

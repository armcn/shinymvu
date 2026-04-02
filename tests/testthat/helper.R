expect_html <- function(tag, html) {
  expect_identical(as.character(tag), html)
}

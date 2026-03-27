test_that("mvu_bridge_js generates a script tag", {
  js <- mvu_bridge_js()
  expect_s3_class(js, "shiny.tag")
  expect_equal(js$name, "script")
})

test_that("mvu_bridge_js includes component name", {
  js <- mvu_bridge_js("mycomp")
  html <- as.character(js)
  expect_match(html, "mycomp")
})

test_that("mvu_bridge_js includes correct channel names", {
  js <- mvu_bridge_js("app")
  html <- as.character(js)
  expect_match(html, "app__model")
  expect_match(html, "app__msg")
})

test_that("mvu_bridge_js includes extra_js", {
  js <- mvu_bridge_js(extra_js = "customProp: 42")
  html <- as.character(js)
  expect_match(html, "customProp: 42")
})

test_that("mvu_bridge_js includes extra_channels", {
  js <- mvu_bridge_js(extra_channels = list(
    clipboard = "navigator.clipboard.writeText(data);"
  ))
  html <- as.character(js)
  expect_match(html, "clipboard")
  expect_match(html, "navigator.clipboard.writeText")
})

test_that("mvu_bridge_js registers Alpine.data component", {
  js <- mvu_bridge_js("counter")
  html <- as.character(js)
  expect_match(html, 'Alpine\\.data\\("counter"')
})

test_that("mvu_bridge_js registers send function", {
  js <- mvu_bridge_js()
  html <- as.character(js)
  expect_match(html, "send: function\\(type, value\\)")
})

test_that("mvu_page wraps content in x-data container", {
  page <- mvu_page(shiny::tags$p("hello"))
  html <- as.character(page)
  expect_match(html, 'x-data="mvu"')
  expect_match(html, "x-cloak")
  expect_match(html, "hello")
})

test_that("mvu_page uses custom component name", {
  page <- mvu_page(shiny::tags$p("test"), component = "myapp")
  html <- as.character(page)
  expect_match(html, 'x-data="myapp"')
})

test_that("mvu_page returns bslib page structure", {
  page <- mvu_page(shiny::tags$p("content"))
  html <- as.character(page)
  expect_match(html, "bslib-page-fill")
})

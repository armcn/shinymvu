test_that("mvu_bridge_js generates a script tag", {
  js <- mvu_bridge_js()
  expect_s3_class(js, "shiny.tag")
  expect_equal(js$name, "script")
})

test_that("mvu_bridge_js calls shinymvu.register", {
  js <- mvu_bridge_js("mycomp")
  html <- as.character(js)
  expect_match(html, "shinymvu\\.register")
})

test_that("mvu_bridge_js includes correct channel names", {
  js <- mvu_bridge_js("app")
  html <- as.character(js)
  expect_match(html, "app__model")
  expect_match(html, "app__msg")
})

test_that("mvu_bridge_js includes extra_js as extend", {
  js <- mvu_bridge_js(extra_js = "customProp: 42")
  html <- as.character(js)
  expect_match(html, "extend:")
  expect_match(html, "customProp: 42")
})

test_that("mvu_bridge_js includes extra_channels as handlers", {
  js <- mvu_bridge_js(extra_channels = list(
    clipboard = "navigator.clipboard.writeText(data);"
  ))
  html <- as.character(js)
  expect_match(html, "handlers:")
  expect_match(html, "clipboard")
  expect_match(html, "navigator.clipboard.writeText")
})

test_that("mvu_bridge_js includes component name in config", {
  js <- mvu_bridge_js("counter")
  html <- as.character(js)
  expect_match(html, '"counter"')
})

test_that("shinymvu_dep returns an htmlDependency", {
  dep <- shinymvu_dep()
  expect_s3_class(dep, "html_dependency")
  expect_equal(dep$name, "shinymvu")
})

test_that("alpine_dep returns an htmlDependency", {
  dep <- alpine_dep()
  expect_s3_class(dep, "html_dependency")
  expect_equal(dep$name, "alpinejs")
})


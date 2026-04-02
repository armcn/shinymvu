test_that("mvu_module_server initializes model from init()", {
  shiny::testServer(
    mvu_module_server,
    args = list(
      init = function() list(count = 0),
      update = function(model, msg, value) model
    ),
    {
      expect_equal(session$getReturned()(), list(count = 0))
    }
  )
})

test_that("mvu_module_server updates model on message", {
  shiny::testServer(
    mvu_module_server,
    args = list(
      init = function() list(count = 0),
      update = function(model, msg, value) {
        switch(msg,
          increment = modifyList(model, list(count = model$count + 1)),
          decrement = modifyList(model, list(count = model$count - 1)),
          model
        )
      }
    ),
    {
      model <- session$getReturned()

      session$setInputs(mvu__msg = list(type = "increment", value = NULL))
      expect_equal(model()$count, 1)

      session$setInputs(mvu__msg = list(type = "decrement", value = NULL))
      expect_equal(model()$count, 0)
    }
  )
})

test_that("mvu_module_ui includes Alpine x-data and x-cloak", {
  ui <- mvu_module_ui("test", tags$p("content"))
  html <- as.character(ui)
  expect_match(html, "x-data")
  expect_match(html, "x-cloak")
  expect_match(html, "content")
})

test_that("mvu_module_ui passes children to container div", {
  ui <- mvu_module_ui("test", tags$p("hello"), tags$span("world"))
  html <- as.character(ui)
  expect_match(html, "<p>hello</p>")
  expect_match(html, "<span>world</span>")
})

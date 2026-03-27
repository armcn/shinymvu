test_that("mvu_module_server runs without error", {
  init <- function() list(count = 0)
  update <- function(model, msg, value) {
    switch(msg,
      increment = list_set(model, count = model$count + 1),
      model
    )
  }
  to_frontend <- function(model) model

  shiny::testServer(
    mvu_module_server,
    args = list(init = init, update = update, to_frontend = to_frontend),
    {
      session$setInputs(mvu__msg = list(type = "increment", value = NULL))
    }
  )
  succeed()
})

test_that("mvu_module_server returns a reactive model", {
  init <- function() list(count = 0)
  update <- function(model, msg, value) {
    switch(msg,
      increment = list_set(model, count = model$count + 1),
      model
    )
  }
  to_frontend <- function(model) model

  shiny::testServer(
    mvu_module_server,
    args = list(init = init, update = update, to_frontend = to_frontend),
    {
      model <- session$getReturned()
      expect_equal(model()$count, 0)
    }
  )
})

test_that("mvu_module_server updates model on message", {
  init <- function() list(count = 0)
  update <- function(model, msg, value) {
    switch(msg,
      increment = list_set(model, count = model$count + 1),
      decrement = list_set(model, count = model$count - 1),
      model
    )
  }
  to_frontend <- function(model) model

  shiny::testServer(
    mvu_module_server,
    args = list(init = init, update = update, to_frontend = to_frontend),
    {
      model <- session$getReturned()

      session$setInputs(mvu__msg = list(type = "increment", value = NULL))
      expect_equal(model()$count, 1)

      session$setInputs(mvu__msg = list(type = "decrement", value = NULL))
      expect_equal(model()$count, 0)
    }
  )
})

test_that("mvu_module_server on_msg can intercept messages", {
  intercepted <- NULL
  init <- function() list(count = 0)
  update <- function(model, msg, value) {
    switch(msg,
      increment = list_set(model, count = model$count + 1),
      model
    )
  }
  to_frontend <- function(model) model

  on_msg <- function(model, type, value, session) {
    intercepted <<- type
    TRUE
  }

  shiny::testServer(
    mvu_module_server,
    args = list(
      init = init, update = update, to_frontend = to_frontend,
      on_msg = on_msg
    ),
    {
      session$setInputs(mvu__msg = list(type = "increment", value = NULL))
      expect_equal(intercepted, "increment")
    }
  )
})

test_that("mvu_module_server on_msg returning FALSE skips update", {
  init <- function() list(count = 0)
  update <- function(model, msg, value) {
    list_set(model, count = model$count + 1)
  }
  to_frontend <- function(model) model

  on_msg <- function(model, type, value, session) {
    FALSE
  }

  shiny::testServer(
    mvu_module_server,
    args = list(
      init = init, update = update, to_frontend = to_frontend,
      on_msg = on_msg
    ),
    {
      model <- session$getReturned()
      session$setInputs(mvu__msg = list(type = "anything", value = NULL))
      expect_equal(model()$count, 0)
    }
  )
})

test_that("mvu_module_server msg parameter auto-converts strings to enum", {
  check_msg <- mvu_enum(c("increment", "decrement"))
  init <- function() list(count = 0)
  update <- function(model, msg, value) {
    match_enum(msg,
      "increment" ~ list_set(model, count = model$count + 1),
      "decrement" ~ list_set(model, count = model$count - 1)
    )
  }

  shiny::testServer(
    mvu_module_server,
    args = list(init = init, update = update, msg = check_msg),
    {
      model <- session$getReturned()
      expect_equal(model()$count, 0)

      session$setInputs(mvu__msg = list(type = "increment", value = NULL))
      expect_equal(model()$count, 1)

      session$setInputs(mvu__msg = list(type = "decrement", value = NULL))
      expect_equal(model()$count, 0)
    }
  )
})

test_that("mvu_module_server msg parameter rejects invalid messages", {
  check_msg <- mvu_enum(c("increment", "decrement"))
  init <- function() list(count = 0)
  update <- function(model, msg, value) model

  shiny::testServer(
    mvu_module_server,
    args = list(init = init, update = update, msg = check_msg),
    {
      model <- session$getReturned()
      expect_warning(
        session$setInputs(mvu__msg = list(type = "invalid", value = NULL)),
        "Unknown message"
      )
      expect_equal(model()$count, 0)
    }
  )
})

test_that("mvu_module_ui generates correct structure", {
  ui <- mvu_module_ui("test_counter",
    shiny::tags$p("content"))
  html <- as.character(ui)
  expect_match(html, "x-data")
  expect_match(html, "x-cloak")
  expect_match(html, "content")
  expect_match(html, "test_counter")
})

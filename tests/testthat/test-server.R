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
    match_enum(
      msg,
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
  ui <- mvu_module_ui(
    "test_counter",
    shiny::tags$p("content")
  )
  html <- as.character(ui)
  expect_match(html, "x-data")
  expect_match(html, "x-cloak")
  expect_match(html, "content")
  expect_match(html, "test_counter")
})

# --- Effects in runtime -----------------------------------------------------

test_that("mvu_module_server handles mvu_result from update", {
  init <- function() list(count = 0)
  update <- function(model, msg, value) {
    switch(msg,
      save = mvu_result(model, effect_notify("Saved!")),
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

      session$setInputs(mvu__msg = list(type = "increment", value = NULL))
      expect_equal(model()$count, 1)

      session$setInputs(mvu__msg = list(type = "save", value = NULL))
      expect_equal(model()$count, 1)
    }
  )
})

test_that("mvu_module_server with plain model return is backward compatible", {
  init <- function() list(count = 0)
  update <- function(model, msg, value) {
    switch(msg,
      increment = list_set(model, count = model$count + 1),
      model
    )
  }

  shiny::testServer(
    mvu_module_server,
    args = list(init = init, update = update),
    {
      model <- session$getReturned()
      session$setInputs(mvu__msg = list(type = "increment", value = NULL))
      expect_equal(model()$count, 1)
    }
  )
})

# --- Debug ------------------------------------------------------------------

test_that("mvu_module_server with debug returns model and log", {
  init <- function() list(count = 0)
  update <- function(model, msg, value) {
    list_set(model, count = model$count + 1)
  }

  shiny::testServer(
    mvu_module_server,
    args = list(init = init, update = update, debug = TRUE),
    {
      runtime <- session$getReturned()
      expect_true(is.list(runtime))
      expect_true("model" %in% names(runtime))
      expect_true("log" %in% names(runtime))

      expect_equal(runtime$model()$count, 0)
      expect_length(runtime$log()$transitions, 0)
    }
  )
})

test_that("mvu_module_server debug records transitions", {
  init <- function() list(count = 0)
  update <- function(model, msg, value) {
    list_set(model, count = model$count + 1)
  }

  shiny::testServer(
    mvu_module_server,
    args = list(init = init, update = update, debug = TRUE),
    {
      runtime <- session$getReturned()

      session$setInputs(mvu__msg = list(type = "increment", value = NULL))
      expect_equal(runtime$model()$count, 1)
      expect_length(runtime$log()$transitions, 1)

      t <- runtime$log()$transitions[[1]]
      expect_equal(t$type, "increment")
      expect_equal(t$model_before$count, 0)
      expect_equal(t$model_after$count, 1)
    }
  )
})

test_that("mvu_module_server debug records effects", {
  init <- function() list(count = 0)
  update <- function(model, msg, value) {
    mvu_result(
      list_set(model, count = model$count + 1),
      effect_notify("Done!")
    )
  }

  shiny::testServer(
    mvu_module_server,
    args = list(init = init, update = update, debug = TRUE),
    {
      runtime <- session$getReturned()

      session$setInputs(mvu__msg = list(type = "increment", value = NULL))
      t <- runtime$log()$transitions[[1]]
      expect_length(t$effects, 1)
      expect_equal(t$effects[[1]]$kind, "notify")
    }
  )
})

test_that("mvu_module_server debug accumulates across messages", {
  init <- function() list(count = 0)
  update <- function(model, msg, value) {
    list_set(model, count = model$count + 1)
  }

  shiny::testServer(
    mvu_module_server,
    args = list(init = init, update = update, debug = TRUE),
    {
      runtime <- session$getReturned()

      session$setInputs(mvu__msg = list(type = "a", value = NULL))
      session$setInputs(mvu__msg = list(type = "b", value = NULL))
      session$setInputs(mvu__msg = list(type = "c", value = NULL))
      expect_length(runtime$log()$transitions, 3)
      expect_equal(runtime$log()$transitions[[3]]$seq, 3)
    }
  )
})

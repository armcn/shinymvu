test_that("mvu_dispatch processes a single message", {
  update <- function(model, msg, value) {
    switch(msg,
      increment = list_set(model, count = model$count + 1),
      model
    )
  }
  result <- mvu_dispatch(
    init = function() list(count = 0),
    update = update,
    messages = list("increment")
  )
  expect_equal(result$count, 1)
})

test_that("mvu_dispatch processes multiple messages in order", {
  update <- function(model, msg, value) {
    switch(msg,
      increment = list_set(model, count = model$count + 1),
      decrement = list_set(model, count = model$count - 1),
      model
    )
  }
  result <- mvu_dispatch(
    init = function() list(count = 0),
    update = update,
    messages = list("increment", "increment", "increment", "decrement")
  )
  expect_equal(result$count, 2)
})

test_that("mvu_dispatch handles list-format messages with values", {
  update <- function(model, msg, value) {
    switch(msg,
      set = list_set(model, count = value),
      model
    )
  }
  result <- mvu_dispatch(
    init = function() list(count = 0),
    update = update,
    messages = list(list(type = "set", value = 42))
  )
  expect_equal(result$count, 42)
})

test_that("mvu_dispatch accepts init as a list", {
  update <- function(model, msg, value) {
    switch(msg,
      increment = list_set(model, count = model$count + 1),
      model
    )
  }
  result <- mvu_dispatch(
    init = list(count = 10),
    update = update,
    messages = list("increment")
  )
  expect_equal(result$count, 11)
})

test_that("mvu_dispatch with empty messages returns init", {
  result <- mvu_dispatch(
    init = function() list(count = 5),
    update = function(model, msg, value) model,
    messages = list()
  )
  expect_equal(result$count, 5)
})

test_that("mvu_dispatch mixes string and list messages", {
  update <- function(model, msg, value) {
    switch(msg,
      increment = list_set(model, count = model$count + 1),
      add = list_set(model, count = model$count + value),
      model
    )
  }
  result <- mvu_dispatch(
    init = list(count = 0),
    update = update,
    messages = list("increment", list(type = "add", value = 10), "increment")
  )
  expect_equal(result$count, 12)
})

test_that("mvu_dispatch works with enum-based update", {
  Msg <- mvu_enum(c("increment", "decrement", "reset"))
  update <- function(model, msg, value) {
    msg <- Msg(msg)
    match_enum(
      msg,
      "increment" ~ list_set(model, count = model$count + 1),
      "decrement" ~ list_set(model, count = model$count - 1),
      "reset" ~ list_set(model, count = 0)
    )
  }
  result <- mvu_dispatch(
    init = function() list(count = 0),
    update = update,
    messages = list("increment", "increment", "increment", "reset")
  )
  expect_equal(result$count, 0)
})

test_that("mvu_dispatch propagates errors from update", {
  update <- function(model, msg, value) {
    stop("update error")
  }
  expect_error(
    mvu_dispatch(
      init = list(x = 1),
      update = update,
      messages = list("anything")
    ),
    "update error"
  )
})

# --- Effects collection -----------------------------------------------------

test_that("mvu_dispatch handles mvu_result return values", {
  update <- function(model, msg, value) {
    switch(msg,
      increment = list_set(model, count = model$count + 1),
      save = mvu_result(model, effect_notify("Saved!")),
      model
    )
  }
  result <- mvu_dispatch(
    init = list(count = 0),
    update = update,
    messages = list("increment", "save")
  )
  expect_equal(result$count, 1)
})

test_that("mvu_dispatch collects effects when requested", {
  update <- function(model, msg, value) {
    switch(msg,
      increment = list_set(model, count = model$count + 1),
      save = mvu_result(model, effect_notify("Saved!")),
      reset = mvu_result(
        list_set(model, count = 0),
        effect_notify("Reset!"),
        effect_send("analytics", list(event = "reset"))
      ),
      model
    )
  }
  result <- mvu_dispatch(
    init = list(count = 0),
    update = update,
    messages = list("increment", "save", "reset"),
    .collect_effects = TRUE
  )
  expect_equal(result$model$count, 0)
  expect_length(result$effects, 3)
  expect_equal(result$effects[[1]]$kind, "notify")
  expect_equal(result$effects[[1]]$text, "Saved!")
  expect_equal(result$effects[[2]]$kind, "notify")
  expect_equal(result$effects[[3]]$kind, "send")
})

test_that("mvu_dispatch with .collect_effects=TRUE and no effects returns empty list", {
  update <- function(model, msg, value) {
    list_set(model, count = model$count + 1)
  }
  result <- mvu_dispatch(
    init = list(count = 0),
    update = update,
    messages = list("increment"),
    .collect_effects = TRUE
  )
  expect_equal(result$model$count, 1)
  expect_length(result$effects, 0)
})

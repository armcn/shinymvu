# --- mvu_transition ----------------------------------------------------------

test_that("mvu_transition creates a transition record", {
  t <- mvu_transition(
    seq = 1, type = "increment", value = NULL,
    model_before = list(count = 0), model_after = list(count = 1)
  )
  expect_s3_class(t, "mvu_transition")
  expect_equal(t$seq, 1)
  expect_equal(t$type, "increment")
  expect_null(t$value)
  expect_equal(t$model_before, list(count = 0))
  expect_equal(t$model_after, list(count = 1))
  expect_length(t$effects, 0)
})

test_that("mvu_transition converts enum type to character", {
  Msg <- mvu_enum(c("a", "b"))
  t <- mvu_transition(
    seq = 1, type = Msg("a"), value = NULL,
    model_before = list(), model_after = list()
  )
  expect_equal(t$type, "a")
  expect_type(t$type, "character")
})

test_that("mvu_transition records effects", {
  effs <- list(effect_notify("done"))
  t <- mvu_transition(
    seq = 1, type = "save", value = NULL,
    model_before = list(), model_after = list(),
    effects = effs
  )
  expect_length(t$effects, 1)
  expect_equal(t$effects[[1]]$kind, "notify")
})

test_that("mvu_transition records timestamp", {
  before <- Sys.time()
  t <- mvu_transition(1, "x", NULL, list(), list())
  after <- Sys.time()
  expect_true(t$timestamp >= before && t$timestamp <= after)
})

test_that("print.mvu_transition produces output", {
  t <- mvu_transition(1, "increment", NULL, list(), list())
  expect_output(print(t), "mvu_transition")
  expect_output(print(t), "increment")
})

# --- mvu_log ----------------------------------------------------------------

test_that("mvu_log creates an empty log", {
  l <- mvu_log()
  expect_s3_class(l, "mvu_log")
  expect_length(l$transitions, 0)
})

test_that("mvu_log wraps existing transitions", {
  t1 <- mvu_transition(1, "a", NULL, list(), list())
  t2 <- mvu_transition(2, "b", NULL, list(), list())
  l <- mvu_log(list(t1, t2))
  expect_length(l$transitions, 2)
})

test_that("print.mvu_log shows transitions", {
  t1 <- mvu_transition(1, "a", NULL, list(), list())
  t2 <- mvu_transition(2, "b", NULL, list(), list())
  l <- mvu_log(list(t1, t2))
  expect_output(print(l), "2 transitions")
  expect_output(print(l), "'a'")
  expect_output(print(l), "'b'")
})

test_that("print.mvu_log handles empty log", {
  expect_output(print(mvu_log()), "0 transitions")
})

test_that("print.mvu_log shows effect count", {
  t <- mvu_transition(1, "save", NULL, list(), list(),
    effects = list(effect_notify("done"))
  )
  l <- mvu_log(list(t))
  expect_output(print(l), "effect")
})

test_that("summary.mvu_log shows message counts", {
  ts <- list(
    mvu_transition(1, "a", NULL, list(), list()),
    mvu_transition(2, "b", NULL, list(), list()),
    mvu_transition(3, "a", NULL, list(), list())
  )
  l <- mvu_log(ts)
  expect_output(summary(l), "a: 2")
  expect_output(summary(l), "b: 1")
})

test_that("summary.mvu_log handles empty log", {
  expect_output(summary(mvu_log()), "empty")
})

# --- mvu_replay -------------------------------------------------------------

test_that("mvu_replay reproduces model history", {
  update <- function(model, msg, value) {
    switch(msg,
      increment = list_set(model, count = model$count + 1),
      model
    )
  }
  log <- mvu_log(list(
    mvu_transition(1, "increment", NULL, list(count = 0), list(count = 1)),
    mvu_transition(2, "increment", NULL, list(count = 1), list(count = 2))
  ))
  models <- mvu_replay(list(count = 0), update, log)
  expect_length(models, 3)
  expect_equal(models[[1]]$count, 0)
  expect_equal(models[[2]]$count, 1)
  expect_equal(models[[3]]$count, 2)
})

test_that("mvu_replay accepts init as a function", {
  update <- function(model, msg, value) {
    list_set(model, count = model$count + 1)
  }
  log <- mvu_log(list(
    mvu_transition(1, "inc", NULL, list(count = 0), list(count = 1))
  ))
  models <- mvu_replay(function() list(count = 0), update, log)
  expect_equal(models[[2]]$count, 1)
})

test_that("mvu_replay handles empty log", {
  models <- mvu_replay(
    list(x = 1),
    function(model, msg, value) model,
    mvu_log()
  )
  expect_length(models, 1)
  expect_equal(models[[1]]$x, 1)
})

test_that("mvu_replay handles mvu_result return values", {
  update <- function(model, msg, value) {
    mvu_result(
      list_set(model, count = model$count + 1),
      effect_notify("incremented")
    )
  }
  log <- mvu_log(list(
    mvu_transition(1, "inc", NULL, list(count = 0), list(count = 1))
  ))
  models <- mvu_replay(list(count = 0), update, log)
  expect_equal(models[[2]]$count, 1)
})

test_that("mvu_replay accepts a plain list of transitions", {
  update <- function(model, msg, value) {
    list_set(model, count = model$count + 1)
  }
  transitions <- list(
    mvu_transition(1, "inc", NULL, list(count = 0), list(count = 1)),
    mvu_transition(2, "inc", NULL, list(count = 1), list(count = 2))
  )
  models <- mvu_replay(list(count = 0), update, transitions)
  expect_equal(models[[3]]$count, 2)
})

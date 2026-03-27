# --- mvu_result --------------------------------------------------------------

test_that("mvu_result wraps model with no effects", {
  r <- mvu_result(list(x = 1))
  expect_s3_class(r, "mvu_result")
  expect_equal(r$model, list(x = 1))
  expect_length(r$effects, 0)
})

test_that("mvu_result wraps model with one effect", {
  r <- mvu_result(list(x = 1), effect_notify("hi"))
  expect_equal(r$model, list(x = 1))
  expect_length(r$effects, 1)
  expect_equal(r$effects[[1]]$kind, "notify")
})

test_that("mvu_result wraps model with multiple effects", {
  r <- mvu_result(
    list(x = 1),
    effect_notify("hi"),
    effect_send("ch", "data")
  )
  expect_length(r$effects, 2)
  expect_equal(r$effects[[1]]$kind, "notify")
  expect_equal(r$effects[[2]]$kind, "send")
})

test_that("mvu_result flattens effects() combiner", {
  combined <- effects(effect_notify("a"), effect_notify("b"))
  r <- mvu_result(list(x = 1), combined, effect_send("c", "d"))
  expect_length(r$effects, 3)
  expect_equal(r$effects[[1]]$text, "a")
  expect_equal(r$effects[[2]]$text, "b")
  expect_equal(r$effects[[3]]$kind, "send")
})

test_that("mvu_result rejects non-effect arguments", {
  expect_error(mvu_result(list(x = 1), "not an effect"), "mvu_result")
})

test_that("mvu_result ignores NULL effects", {
  r <- mvu_result(list(x = 1), NULL, effect_notify("hi"), NULL)
  expect_length(r$effects, 1)
})

# --- is_mvu_result ----------------------------------------------------------

test_that("is_mvu_result identifies mvu_result objects", {
  expect_true(is_mvu_result(mvu_result(list())))
  expect_false(is_mvu_result(list(model = list(), effects = list())))
  expect_false(is_mvu_result(42))
})

# --- Effect constructors ----------------------------------------------------

test_that("effect_notify creates a notify effect", {
  e <- effect_notify("hello", type = "message", duration = 3)
  expect_s3_class(e, "mvu_effect")
  expect_equal(e$kind, "notify")
  expect_equal(e$text, "hello")
  expect_equal(e$type, "message")
  expect_equal(e$duration, 3)
})

test_that("effect_notify uses defaults", {
  e <- effect_notify("hi")
  expect_equal(e$type, "default")
  expect_equal(e$duration, 5)
})

test_that("effect_send creates a send effect", {
  e <- effect_send("clipboard", "some text")
  expect_s3_class(e, "mvu_effect")
  expect_equal(e$kind, "send")
  expect_equal(e$channel, "clipboard")
  expect_equal(e$data, "some text")
})

test_that("effect_custom creates a custom effect", {
  fn <- function(session) NULL
  e <- effect_custom(fn)
  expect_s3_class(e, "mvu_effect")
  expect_equal(e$kind, "custom")
  expect_identical(e$fn, fn)
})

test_that("effect_custom rejects non-functions", {
  expect_error(effect_custom("not a function"), "function")
})

# --- effects() combiner -----------------------------------------------------

test_that("effects() combines multiple effects", {
  combined <- effects(effect_notify("a"), effect_send("b", "c"))
  expect_s3_class(combined, "mvu_effects")
  expect_length(combined, 2)
})

# --- print methods ----------------------------------------------------------

test_that("print.mvu_result produces output", {
  r <- mvu_result(list(x = 1), effect_notify("hi"))
  expect_output(print(r), "mvu_result")
})

test_that("print.mvu_effect produces output", {
  e <- effect_notify("hi")
  expect_output(print(e), "notify")
})

# --- run_effects (internal) -------------------------------------------------

test_that("run_effects skips non-effect objects", {
  expect_no_error(shinymvu:::run_effects(list("not an effect"), NULL))
})

test_that("mvu_enum creates a factory function", {
  Msg <- mvu_enum(c("a", "b", "c"))
  expect_type(Msg, "closure")
})

test_that("mvu_enum factory produces valid enum values", {
  Msg <- mvu_enum(c("increment", "decrement"))
  result <- Msg("increment")
  expect_s3_class(result, "mvu_enum")
  expect_s3_class(result, "factor")
  expect_equal(as.character(result), "increment")
})

test_that("mvu_enum preserves all levels", {
  values <- c("a", "b", "c")
  Msg <- mvu_enum(values)
  result <- Msg("b")
  expect_equal(levels(result), values)
})

test_that("mvu_enum rejects unknown values", {
  Msg <- mvu_enum(c("increment", "decrement"))
  expect_error(Msg("reset"), "Unknown message 'reset'")
})

test_that("match_enum returns the correct case", {
  Msg <- mvu_enum(c("increment", "decrement"))
  result <- match_enum(
    Msg("increment"),
    "increment" ~ "went up",
    "decrement" ~ "went down"
  )
  expect_equal(result, "went up")
})

test_that("match_enum evaluates expressions in caller environment", {
  Msg <- mvu_enum(c("increment", "decrement"))
  model <- list(count = 5)
  result <- match_enum(
    Msg("increment"),
    "increment" ~ list_set(model, count = model$count + 1),
    "decrement" ~ list_set(model, count = model$count - 1)
  )
  expect_equal(result$count, 6)
})

test_that("match_enum errors on missing cases without .default", {
  Msg <- mvu_enum(c("a", "b", "c"))
  expect_error(
    match_enum(Msg("a"), "a" ~ 1, "b" ~ 2),
    "Unhandled messages: c"
  )
})

test_that("match_enum uses .default for unmatched cases", {
  Msg <- mvu_enum(c("a", "b", "c"))
  result <- match_enum(Msg("c"),
    "a" ~ 1,
    "b" ~ 2,
    .default = 99
  )
  expect_equal(result, 99)
})

test_that("match_enum errors on unknown levels", {
  Msg <- mvu_enum(c("a", "b"))
  expect_error(
    match_enum(Msg("a"), "a" ~ 1, "b" ~ 2, "c" ~ 3),
    "Unknown levels: c"
  )
})

test_that("match_enum supports multiple values on LHS", {
  Msg <- mvu_enum(c("a", "b", "c"))
  result_a <- match_enum(
    Msg("a"),
    c("a", "b") ~ "first two",
    "c" ~ "third"
  )
  result_b <- match_enum(
    Msg("b"),
    c("a", "b") ~ "first two",
    "c" ~ "third"
  )
  expect_equal(result_a, "first two")
  expect_equal(result_b, "first two")
})

test_that("match_enum with .default allows partial coverage", {
  Msg <- mvu_enum(c("a", "b", "c", "d"))
  result <- match_enum(Msg("d"),
    "a" ~ 1,
    .default = 0
  )
  expect_equal(result, 0)
})

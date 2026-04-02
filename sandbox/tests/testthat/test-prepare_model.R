test_that("prepare_model returns atomic values unchanged", {
  expect_equal(prepare_model(list(x = 1)), list(x = 1))
  expect_equal(prepare_model(list(x = "a")), list(x = "a"))
  expect_equal(prepare_model(list(x = TRUE)), list(x = TRUE))
  expect_equal(prepare_model(list(x = NULL)), list(x = NULL))
})

test_that("prepare_model converts data frames to row-oriented lists", {
  model <- list(df = data.frame(a = 1:2, b = c("x", "y")))
  result <- prepare_model(model)
  expect_equal(result$df, list(
    list(a = 1L, b = "x"),
    list(a = 2L, b = "y")
  ))
})

test_that("prepare_model converts factors to characters", {
  model <- list(x = factor(c("a", "b")))
  result <- prepare_model(model)
  expect_equal(result$x, c("a", "b"))
})

test_that("prepare_model converts factors inside data frames", {
  model <- list(df = data.frame(x = factor(c("a", "b"))))
  result <- prepare_model(model)
  expect_equal(result$df, list(
    list(x = "a"),
    list(x = "b")
  ))
})

test_that("prepare_model converts Date to character", {
  model <- list(d = as.Date("2024-01-15"))
  result <- prepare_model(model)
  expect_equal(result$d, "2024-01-15")
})

test_that("prepare_model converts POSIXct to ISO 8601 string", {
  model <- list(ts = as.POSIXct("2024-01-15 10:30:00", tz = "UTC"))
  result <- prepare_model(model)
  expect_match(result$ts, "^2024-01-15T10:30:00")
})

test_that("prepare_model handles nested lists", {
  model <- list(a = list(b = list(c = 1)))
  expect_equal(prepare_model(model), list(a = list(b = list(c = 1))))
})

test_that("prepare_model handles nested data frames", {
  model <- list(outer = list(df = data.frame(x = 1:2)))
  result <- prepare_model(model)
  expect_equal(result$outer$df, list(list(x = 1L), list(x = 2L)))
})

test_that("prepare_model errors on functions", {
  model <- list(fn = mean)
  expect_error(prepare_model(model), "fn.*function")
})

test_that("prepare_model errors on environments", {
  model <- list(env = new.env())
  expect_error(prepare_model(model), "env.*environment")
})

test_that("prepare_model errors on formulas", {
  model <- list(f = y ~ x)
  expect_error(prepare_model(model), "f.*formula")
})

test_that("prepare_model error includes path for nested values", {
  model <- list(a = list(b = mean))
  expect_error(prepare_model(model), "a\\$b")
})

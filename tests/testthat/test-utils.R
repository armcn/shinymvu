test_that("list_set updates an existing key", {
  result <- list_set(list(a = 1, b = 2), a = 10)
  expect_equal(result, list(a = 10, b = 2))
})

test_that("list_set adds a new key", {
  result <- list_set(list(a = 1), b = 2)
  expect_equal(result, list(a = 1, b = 2))
})

test_that("list_set handles multiple updates", {
  result <- list_set(list(a = 1, b = 2, c = 3), a = 10, c = 30)
  expect_equal(result, list(a = 10, b = 2, c = 30))
})

test_that("list_set with no updates returns the same list", {
  original <- list(a = 1, b = 2)
  result <- list_set(original)
  expect_equal(result, original)
})

test_that("list_set with NULL removes the element (standard R behavior)", {
  result <- list_set(list(a = 1, b = 2), b = NULL)
  expect_equal(result, list(a = 1))
  expect_null(result$b)
})

test_that("list_set works with nested lists", {
  model <- list(user = list(name = "Alice"), count = 0)
  result <- list_set(model, user = list(name = "Bob"))
  expect_equal(result$user$name, "Bob")
  expect_equal(result$count, 0)
})

test_that("list_set does not modify the original list", {
  original <- list(a = 1, b = 2)
  result <- list_set(original, a = 10)
  expect_equal(original$a, 1)
  expect_equal(result$a, 10)
})

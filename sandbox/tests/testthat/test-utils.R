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

# --- list_get_in ------------------------------------------------------------

test_that("list_get_in retrieves a shallow value", {
  expect_equal(list_get_in(list(a = 1), "a"), 1)
})

test_that("list_get_in retrieves a nested value", {
  model <- list(user = list(profile = list(name = "Alice")))
  expect_equal(list_get_in(model, c("user", "profile", "name")), "Alice")
})

test_that("list_get_in returns NULL for missing key", {
  expect_null(list_get_in(list(a = 1), "b"))
})

test_that("list_get_in returns NULL for missing nested key", {
  model <- list(a = list(b = 1))
  expect_null(list_get_in(model, c("a", "c")))
})

test_that("list_get_in returns NULL when traversing non-list", {
  model <- list(a = 42)
  expect_null(list_get_in(model, c("a", "b")))
})

test_that("list_get_in with empty path returns the list itself", {
  model <- list(a = 1)
  expect_equal(list_get_in(model, character(0)), list(a = 1))
})

# --- list_set_in ------------------------------------------------------------

test_that("list_set_in sets a shallow value", {
  result <- list_set_in(list(a = 1, b = 2), "a", 99)
  expect_equal(result$a, 99)
  expect_equal(result$b, 2)
})

test_that("list_set_in sets a nested value", {
  model <- list(a = list(b = list(c = 1)))
  result <- list_set_in(model, c("a", "b", "c"), 99)
  expect_equal(result$a$b$c, 99)
})

test_that("list_set_in creates missing intermediates", {
  result <- list_set_in(list(), c("a", "b", "c"), 42)
  expect_equal(result$a$b$c, 42)
})

test_that("list_set_in adds a new key at a nested level", {
  model <- list(a = list(b = 1))
  result <- list_set_in(model, c("a", "c"), 2)
  expect_equal(result$a$b, 1)
  expect_equal(result$a$c, 2)
})

test_that("list_set_in with NULL removes nested key", {
  model <- list(a = list(b = 1, c = 2))
  result <- list_set_in(model, c("a", "b"), NULL)
  expect_null(result$a$b)
  expect_equal(result$a$c, 2)
})

test_that("list_set_in does not modify the original", {
  model <- list(a = list(b = 1))
  result <- list_set_in(model, c("a", "b"), 99)
  expect_equal(model$a$b, 1)
  expect_equal(result$a$b, 99)
})

test_that("list_set_in with empty path replaces entire value", {
  result <- list_set_in(list(a = 1), character(0), list(b = 2))
  expect_equal(result, list(b = 2))
})

test_that("list_set_in overwrites non-list intermediate", {
  model <- list(a = 42)
  result <- list_set_in(model, c("a", "b"), 99)
  expect_equal(result$a$b, 99)
})

# --- list_update_in ---------------------------------------------------------

test_that("list_update_in applies function to nested value", {
  model <- list(counters = list(a = 5))
  result <- list_update_in(model, c("counters", "a"), function(x) x + 1)
  expect_equal(result$counters$a, 6)
})

test_that("list_update_in passes NULL for missing path", {
  model <- list(a = list())
  result <- list_update_in(model, c("a", "b"), function(x) {
    if (is.null(x)) 0 else x + 1
  })
  expect_equal(result$a$b, 0)
})

test_that("list_update_in does not modify the original", {
  model <- list(a = list(b = 1))
  result <- list_update_in(model, c("a", "b"), function(x) x * 10)
  expect_equal(model$a$b, 1)
  expect_equal(result$a$b, 10)
})

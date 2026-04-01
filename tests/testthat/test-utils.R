test_that("is_tag recognizes shiny tags", {
  expect_true(is_tag(shiny::tags$div()))
  expect_true(is_tag(shiny::tags$span("hello")))
  expect_false(is_tag(shiny::tagList(shiny::tags$div())))
  expect_false(is_tag(shiny::HTML("<b>bold</b>")))
  expect_false(is_tag("hello"))
  expect_false(is_tag(42))
  expect_false(is_tag(NULL))
})

test_that("is_html recognizes HTML objects", {
  expect_true(is_html(shiny::HTML("<b>bold</b>")))
  expect_false(is_html(shiny::tags$div()))
  expect_false(is_html("plain string"))
  expect_false(is_html(42))
})

test_that("is_tag_list recognizes tag lists (strict)", {
  expect_true(is_tag_list(shiny::tagList(shiny::tags$div())))
  expect_false(is_tag_list(list(shiny::tags$div())))
  expect_false(is_tag_list(shiny::tags$div()))
  expect_false(is_tag_list("hello"))
})

test_that("is_tag_list recognizes tag lists (non-strict)", {
  expect_true(is_tag_list(shiny::tagList(shiny::tags$div()), strict = FALSE))
  expect_true(is_tag_list(list(shiny::tags$div(), shiny::tags$span()), strict = FALSE))
  expect_false(is_tag_list(list(1, 2, 3), strict = FALSE))
  expect_false(is_tag_list(shiny::tags$div(), strict = FALSE))
  expect_false(is_tag_list("hello", strict = FALSE))
})

test_that("is_tag_like accepts tags, tag lists, and HTML", {
  expect_true(is_tag_like(shiny::tags$div()))
  expect_true(is_tag_like(shiny::HTML("<b>bold</b>")))
  expect_true(is_tag_like(shiny::tagList(shiny::tags$div())))
  expect_true(is_tag_like(shiny::icon("table")))
  expect_false(is_tag_like("hello"))
  expect_false(is_tag_like(42))
  expect_false(is_tag_like(NULL))
})

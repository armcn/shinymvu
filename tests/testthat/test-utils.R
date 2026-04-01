test_inputs <- list(
  tag       = shiny::tags$div(),
  tag_list  = shiny::tagList(shiny::tags$div()),
  tag_list2 = list(shiny::tags$div(), shiny::tags$span()),
  html      = shiny::HTML("<b>bold</b>"),
  icon      = shiny::icon("table"),
  string    = "hello",
  number    = 42,
  null      = NULL,
  bad_list  = list(1, 2, 3)
)

test_that("is_tag matches shiny:::isTag", {
  for (nm in names(test_inputs)) {
    expect_identical(
      is_tag(test_inputs[[nm]]),
      shiny:::isTag(test_inputs[[nm]]),
      label = nm
    )
  }
})

test_that("is_tag_list matches shiny:::isTagList (strict)", {
  for (nm in names(test_inputs)) {
    expect_identical(
      is_tag_list(test_inputs[[nm]], strict = TRUE),
      shiny:::isTagList(test_inputs[[nm]], strict = TRUE),
      label = nm
    )
  }
})

test_that("is_tag_list matches shiny:::isTagList (non-strict)", {
  for (nm in names(test_inputs)) {
    expect_identical(
      is_tag_list(test_inputs[[nm]], strict = FALSE),
      shiny:::isTagList(test_inputs[[nm]], strict = FALSE),
      label = nm
    )
  }
})

test_that("is_tag_like matches shiny:::isTagLike (strict)", {
  for (nm in names(test_inputs)) {
    expect_identical(
      is_tag_like(test_inputs[[nm]], strict = TRUE),
      shiny:::isTagLike(test_inputs[[nm]], strict = TRUE),
      label = nm
    )
  }
})

test_that("is_tag_like matches shiny:::isTagLike (non-strict)", {
  for (nm in names(test_inputs)) {
    expect_identical(
      is_tag_like(test_inputs[[nm]], strict = FALSE),
      shiny:::isTagLike(test_inputs[[nm]], strict = FALSE),
      label = nm
    )
  }
})

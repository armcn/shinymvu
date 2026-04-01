test_that("An empty button is created by default", {
  mvu_action_button() |>
    expect_html('<button type="button" class="btn btn-default"></button>')
})

test_that("A null 'label' doesn't have a label", {
  mvu_action_button(label = NULL) |>
    expect_html('<button type="button" class="btn btn-default"></button>')
})

test_that("The 'label' is inserted into the button", {
  mvu_action_button(label = "label") |>
    expect_html('<button type="button" class="btn btn-default"><span class="action-label">label</span></button>')
})

test_that("An invalid 'icon' gives a warning", {
  mvu_action_button(icon = 1) |>
    expect_warning()
})

test_that("A null 'icon' doesn't have an icon", {
  mvu_action_button(icon = NULL) |>
    expect_html('<button type="button" class="btn btn-default"></button>')
})

test_that("The 'icon' is inserted into the button", {
  mvu_action_button(icon = shiny::tags$i(class = "icon")) |>
    expect_html('<button type="button" class="btn btn-default"><span class="action-icon"><i class="icon"></i></span></button>')
})

test_that("An invalid 'width' gives an error", {
  mvu_action_button(width = c(1, 2)) |>
    expect_error()
})

test_that("A null 'width' has no width style", {
  mvu_action_button(width = NULL) |>
    expect_html('<button type="button" class="btn btn-default"></button>')
})

test_that("The 'width' is added as a style attribute", {
  mvu_action_button(width = "10px") |>
    expect_html('<button style="width:10px;" type="button" class="btn btn-default"></button>')
})


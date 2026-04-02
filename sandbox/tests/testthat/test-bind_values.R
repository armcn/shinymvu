test_that("bind_text adds x-text attribute", {
  tags$span() |>
    bind_text("model.count") |>
    expect_html('<span x-text="model.count"></span>')
})

test_that("bind_text preserves existing attributes", {
  tags$span(class = "label") |>
    bind_text("model.name") |>
    expect_html('<span class="label" x-text="model.name"></span>')
})

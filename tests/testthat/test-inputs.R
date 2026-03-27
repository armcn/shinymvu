test_that("mvu_button generates correct HTML", {
  btn <- mvu_button("Click", msg = "clicked")
  html <- as.character(btn)
  expect_match(html, "send")
  expect_match(html, "clicked")
  expect_match(html, "Click")
  expect_match(html, "btn btn-primary")
})

test_that("mvu_button with character value includes value", {
  btn <- mvu_button("Save", msg = "save", value = "draft")
  html <- as.character(btn)
  expect_match(html, "save")
  expect_match(html, "draft")
})

test_that("mvu_button with numeric value includes value", {
  btn <- mvu_button("Add", msg = "add", value = 5)
  html <- as.character(btn)
  expect_match(html, "add")
  expect_match(html, "5")
})

test_that("mvu_button accepts custom class", {
  btn <- mvu_button("Go", msg = "go", class = "btn btn-danger")
  html <- as.character(btn)
  expect_match(html, "btn btn-danger")
})

test_that("mvu_button passes extra attributes", {
  btn <- mvu_button("Go", msg = "go", disabled = NA)
  html <- as.character(btn)
  expect_match(html, "disabled")
})

test_that("mvu_button_expr uses JavaScript expression for value", {
  btn <- mvu_button_expr("Submit", msg = "submit",
    value_expr = "model.formData")
  html <- as.character(btn)
  expect_match(html, "submit")
  expect_match(html, "model.formData")
})

test_that("mvu_button generates button tag", {
  btn <- mvu_button("Test", msg = "test")
  expect_equal(btn$name, "button")
})

test_that("mvu_button @click attribute wires send correctly", {
  btn <- mvu_button("X", msg = "do_it")
  click_attr <- btn$attribs[["@click"]]
  expect_match(click_attr, "^send\\('do_it'\\)$")
})

test_that("mvu_button with value wires send with value", {
  btn <- mvu_button("X", msg = "do_it", value = "yes")
  click_attr <- btn$attribs[["@click"]]
  expect_match(click_attr, "^send\\('do_it', 'yes'\\)$")
})

test_that("mvu_button_expr @click attribute includes expression", {
  btn <- mvu_button_expr("X", msg = "go", value_expr = "model.x")
  click_attr <- btn$attribs[["@click"]]
  expect_equal(click_attr, "send('go', model.x)")
})

test_that("mvu_select generates select element with options", {
  sel <- mvu_select("Color", c("red", "blue"), msg = "set_color",
    value_expr = "model.color")
  html <- as.character(sel)
  expect_match(html, "<select")
  expect_match(html, "<label")
  expect_match(html, "red")
  expect_match(html, "blue")
  expect_match(html, "Select\\.\\.\\.")
  expect_match(html, "x-bind:value")
})

test_that("mvu_select @change wires send correctly", {
  sel <- mvu_select("X", c("a"), msg = "pick", value_expr = "model.x")
  html <- as.character(sel)
  expect_match(html, "set_color|pick")
})

test_that("mvu_checkbox generates checkbox with correct wiring", {
  cb <- mvu_checkbox("Enable", msg = "toggle", checked_expr = "model.on")
  html <- as.character(cb)
  expect_match(html, 'type="checkbox"')
  expect_match(html, "toggle")
  expect_match(html, "x-bind:checked")
  expect_match(html, "Enable")
})

test_that("mvu_slider generates range input", {
  sl <- mvu_slider("Volume", min = 0, max = 100, msg = "set_vol",
    value_expr = "model.vol")
  html <- as.character(sl)
  expect_match(html, 'type="range"')
  expect_match(html, 'min="0"')
  expect_match(html, 'max="100"')
  expect_match(html, "set_vol")
  expect_match(html, "x-bind:value")
})

test_that("mvu_slider respects custom step", {
  sl <- mvu_slider("X", min = 0, max = 10, msg = "x",
    value_expr = "model.x", step = 0.5)
  html <- as.character(sl)
  expect_match(html, 'step="0.5"')
})

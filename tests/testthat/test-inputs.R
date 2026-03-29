# -- Event handlers ------------------------------------------------------------

test_that("on_click returns @click attribute", {
  result <- on_click("increment")
  expect_equal(result, list(`@click` = "send('increment')"))
})

test_that("on_click with string value quotes it", {
  result <- on_click("set", value = "hello")
  expect_equal(result, list(`@click` = "send('set', 'hello')"))
})

test_that("on_click with numeric value serializes it", {
  result <- on_click("add", value = 42)
  expect_equal(result, list(`@click` = "send('add', 42)"))
})

test_that("on_click_expr uses JS expression", {
  result <- on_click_expr("submit", "model.formData")
  expect_equal(result, list(`@click` = "send('submit', model.formData)"))
})

test_that("on_input returns @change attribute", {
  result <- on_input("set_color")
  expect_equal(result, list(`@change` = "send('set_color', $event.target.value)"))
})

test_that("on_input with debounce uses @input.debounce", {
  result <- on_input("set_name", debounce = 300)
  expect_equal(
    result,
    setNames(
      list("send('set_name', $event.target.value)"),
      "@input.debounce.300ms"
    )
  )
})

test_that("on_check returns @change with checked", {
  result <- on_check("toggle")
  expect_equal(
    result,
    list(`@change` = "send('toggle', $event.target.checked)")
  )
})

test_that("on_input_num returns @change with Number()", {
  result <- on_input_num("set_vol")
  expect_equal(
    result,
    list(`@change` = "send('set_vol', Number($event.target.value))")
  )
})

test_that("on_input_num with debounce", {
  result <- on_input_num("set_age", debounce = 500)
  expect_equal(
    names(result),
    "@input.debounce.500ms"
  )
})

# -- mvu_button ----------------------------------------------------------------

test_that("mvu_button generates button with @click", {
  btn <- mvu_button("Go", msg = "go")
  expect_equal(btn$name, "button")
  expect_equal(btn$attribs[["@click"]], "send('go')")
  expect_match(as.character(btn), "btn btn-primary")
  expect_match(as.character(btn), "Go")
})

test_that("mvu_button with value", {
  btn <- mvu_button("Save", msg = "save", value = "draft")
  expect_equal(btn$attribs[["@click"]], "send('save', 'draft')")
})

test_that("mvu_button custom class", {
  btn <- mvu_button("X", msg = "x", class = "btn btn-danger")
  expect_match(as.character(btn), "btn btn-danger")
})

test_that("mvu_button passes extra attributes", {
  btn <- mvu_button("X", msg = "x", disabled = NA)
  expect_match(as.character(btn), "disabled")
})

# -- mvu_text_input ------------------------------------------------------------

test_that("mvu_text_input generates text input with label", {
  ti <- mvu_text_input("Name", msg = "set_name")
  html <- as.character(ti)
  expect_match(html, 'type="text"')
  expect_match(html, "<label")
  expect_match(html, "Name")
  expect_match(html, "@change")
  expect_match(html, "set_name")
})

test_that("mvu_text_input with value adds x-bind:value", {
  ti <- mvu_text_input("Name", msg = "set_name", value = "model.name")
  html <- as.character(ti)
  expect_match(html, "x-bind:value")
  expect_match(html, "model.name")
})

test_that("mvu_text_input with debounce uses @input.debounce", {
  ti <- mvu_text_input("Name", msg = "set_name", debounce = 300)
  html <- as.character(ti)
  expect_match(html, "input.debounce.300ms")
})

# -- mvu_numeric_input ---------------------------------------------------------

test_that("mvu_numeric_input generates number input", {
  ni <- mvu_numeric_input("Age", msg = "set_age", min = 0, max = 120)
  html <- as.character(ni)
  expect_match(html, 'type="number"')
  expect_match(html, 'min="0"')
  expect_match(html, 'max="120"')
  expect_match(html, "Number\\(")
})

# -- mvu_password_input --------------------------------------------------------

test_that("mvu_password_input generates password input", {
  pi <- mvu_password_input("Password", msg = "set_pw")
  html <- as.character(pi)
  expect_match(html, 'type="password"')
  expect_match(html, "set_pw")
})

# -- mvu_textarea_input --------------------------------------------------------

test_that("mvu_textarea_input generates textarea", {
  ta <- mvu_textarea_input("Bio", msg = "set_bio", rows = 5)
  html <- as.character(ta)
  expect_match(html, "<textarea")
  expect_match(html, 'rows="5"')
  expect_match(html, "set_bio")
})

# -- mvu_select_input ----------------------------------------------------------

test_that("mvu_select_input generates select with options", {
  sel <- mvu_select_input("Color",
    choices = c("Red" = "red", "Blue" = "blue"),
    msg = "set_color", selected = "model.color"
  )
  html <- as.character(sel)
  expect_match(html, "<select")
  expect_match(html, 'value="red"')
  expect_match(html, "Red")
  expect_match(html, "Blue")
  expect_match(html, "set_color")
  expect_match(html, "x-bind:value")
})

test_that("mvu_select_input unnamed choices use value as label", {
  sel <- mvu_select_input("X", choices = c("a", "b"), msg = "x")
  html <- as.character(sel)
  expect_match(html, ">a<")
  expect_match(html, ">b<")
})

# -- mvu_checkbox_input --------------------------------------------------------

test_that("mvu_checkbox_input generates checkbox", {
  cb <- mvu_checkbox_input("Agree", msg = "toggle", value = "model.agree")
  html <- as.character(cb)
  expect_match(html, 'type="checkbox"')
  expect_match(html, "toggle")
  expect_match(html, "x-bind:checked")
  expect_match(html, "Agree")
})

# -- mvu_checkbox_group_input -------------------------------------------------

test_that("mvu_checkbox_group_input generates multiple checkboxes", {
  cg <- mvu_checkbox_group_input("Toppings",
    choices = c("Cheese", "Peppers"),
    msg = "toggle_top", selected = "model.toppings"
  )
  html <- as.character(cg)
  expect_match(html, "Cheese")
  expect_match(html, "Peppers")
  expect_match(html, "toggle_top")
  expect_match(html, "includes")
})

# -- mvu_radio_input -----------------------------------------------------------

test_that("mvu_radio_input generates radio buttons", {
  ri <- mvu_radio_input("Pet",
    choices = c("Dog" = "dog", "Cat" = "cat"),
    msg = "set_pet", selected = "model.pet"
  )
  html <- as.character(ri)
  expect_match(html, 'type="radio"')
  expect_match(html, "Dog")
  expect_match(html, "Cat")
  expect_match(html, "set_pet")
  expect_match(html, "model.pet")
})

test_that("mvu_radio_input shares group name", {
  ri <- mvu_radio_input("X", choices = c("a", "b"), msg = "x")
  html <- as.character(ri)
  name_match <- regmatches(html, gregexpr('name="[^"]+"', html))[[1]]
  expect_true(length(name_match) >= 2)
  expect_equal(name_match[1], name_match[2])
})

# -- mvu_slider_input ----------------------------------------------------------

test_that("mvu_slider_input generates range input", {
  sl <- mvu_slider_input("Vol",
    min = 0, max = 100, msg = "set_vol",
    value = "model.vol"
  )
  html <- as.character(sl)
  expect_match(html, 'type="range"')
  expect_match(html, 'min="0"')
  expect_match(html, 'max="100"')
  expect_match(html, "set_vol")
  expect_match(html, "x-bind:value")
})

test_that("mvu_slider_input custom step", {
  sl <- mvu_slider_input("X", min = 0, max = 10, msg = "x", step = 0.5)
  html <- as.character(sl)
  expect_match(html, 'step="0.5"')
})

# -- mvu_date_input ------------------------------------------------------------

test_that("mvu_date_input generates date input", {
  di <- mvu_date_input("Birthday", msg = "set_bday", value = "model.bday")
  html <- as.character(di)
  expect_match(html, 'type="date"')
  expect_match(html, "set_bday")
  expect_match(html, "x-bind:value")
})

test_that("mvu_date_input with min/max constraints", {
  di <- mvu_date_input("Date", msg = "x", min = "2020-01-01", max = "2030-12-31")
  html <- as.character(di)
  expect_match(html, 'min="2020-01-01"')
  expect_match(html, 'max="2030-12-31"')
})

# -- mvu_date_range_input ------------------------------------------------------

test_that("mvu_date_range_input generates two date inputs", {
  dr <- mvu_date_range_input("Trip",
    msg_start = "set_depart", msg_end = "set_return",
    start = "model.depart", end = "model.return"
  )
  html <- as.character(dr)
  expect_match(html, "set_depart")
  expect_match(html, "set_return")
  expect_equal(length(gregexpr('type="date"', html)[[1]]), 2)
})

# -- mvu_file_input ------------------------------------------------------------

test_that("mvu_file_input generates file input", {
  fi <- mvu_file_input("Upload", msg = "upload")
  html <- as.character(fi)
  expect_match(html, 'type="file"')
  expect_match(html, "upload")
  expect_match(html, "f.name")
})

test_that("mvu_file_input with accept and multiple", {
  fi <- mvu_file_input("Photos",
    msg = "up", accept = c("image/*", ".png"),
    multiple = TRUE
  )
  html <- as.character(fi)
  expect_match(html, "image/\\*")
  expect_match(html, "multiple")
})

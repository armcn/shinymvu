# -- Pipe event helpers --------------------------------------------------------

test_that("on_click adds @click attribute to tag", {
  btn <- tags$button("+") |> on_click("increment")
  expect_equal(btn$attribs[["@click"]], "send('increment')")
})

test_that("on_click with string value quotes it", {
  btn <- tags$button("Save") |> on_click("set", value = "hello")
  expect_equal(btn$attribs[["@click"]], "send('set', 'hello')")
})

test_that("on_click with numeric value serializes it", {
  btn <- tags$button("Add") |> on_click("add", value = 42)
  expect_equal(btn$attribs[["@click"]], "send('add', 42)")
})

test_that("on_input adds @change attribute", {
  input <- tags$input(type = "text") |> on_input("set_color")
  expect_equal(
    input$attribs[["@change"]],
    "send('set_color', $event.target.value)"
  )
})

test_that("on_input with debounce uses @input.debounce", {
  input <- tags$input(type = "text") |> on_input("set_name", debounce = 300)
  expect_equal(
    input$attribs[["@input.debounce.300ms"]],
    "send('set_name', $event.target.value)"
  )
})

test_that("on_check adds @change with checked", {
  input <- tags$input(type = "checkbox") |> on_check("toggle")
  expect_equal(
    input$attribs[["@change"]],
    "send('toggle', $event.target.checked)"
  )
})

test_that("on_input_num adds @change with Number()", {
  input <- tags$input(type = "number") |> on_input_num("set_vol")
  expect_equal(
    input$attribs[["@change"]],
    "send('set_vol', Number($event.target.value))"
  )
})

test_that("on_input_num with debounce", {
  input <- tags$input(type = "number") |> on_input_num("set_age", debounce = 500)
  expect_equal(
    names(input$attribs[["@input.debounce.500ms"]]),
    NULL
  )
  expect_match(
    input$attribs[["@input.debounce.500ms"]],
    "Number"
  )
})

test_that("on_change adds @change with static value", {
  input <- tags$input(type = "checkbox") |> on_change("toggle_top", "cheese")
  expect_equal(
    input$attribs[["@change"]],
    "send('toggle_top', 'cheese')"
  )
})

test_that("on_key adds @keydown attribute", {
  input <- tags$input(type = "text") |> on_key("enter", "submit")
  expect_equal(
    input$attribs[["@keydown.enter"]],
    "send('submit')"
  )
})

test_that("on_file adds @change with file metadata", {
  input <- tags$input(type = "file") |> on_file("upload")
  expect_match(input$attribs[["@change"]], "f.name")
  expect_match(input$attribs[["@change"]], "upload")
})

# -- Pipe bind helpers --------------------------------------------------------

test_that("bind_text adds x-text", {
  span <- tags$span() |> bind_text("model.count")
  expect_equal(span$attribs[["x-text"]], "model.count")
})

test_that("bind_value adds x-bind:value", {
  input <- tags$input(type = "text") |> bind_value("model.name")
  expect_equal(input$attribs[["x-bind:value"]], "model.name")
})

test_that("bind_checked adds x-bind:checked", {
  input <- tags$input(type = "checkbox") |> bind_checked("model.agree")
  expect_equal(input$attribs[["x-bind:checked"]], "model.agree")
})

test_that("bind_show adds x-show", {
  el <- tags$div("Loading") |> bind_show("model.loading")
  expect_equal(el$attribs[["x-show"]], "model.loading")
})

test_that("bind_if adds x-if", {
  el <- tags$template(tags$p("Hi")) |> bind_if("model.loggedIn")
  expect_equal(el$attribs[["x-if"]], "model.loggedIn")
})

test_that("bind_for adds x-for", {
  el <- tags$template(tags$li()) |> bind_for("item", "model.items")
  expect_equal(el$attribs[["x-for"]], "item in model.items")
})

test_that("bind_key adds x-bind:key", {
  el <- tags$li() |> bind_key("item.id")
  expect_equal(el$attribs[["x-bind:key"]], "item.id")
})

test_that("bind_class adds x-bind:class", {
  el <- tags$div() |> bind_class("model.active ? 'on' : ''")
  expect_equal(el$attribs[["x-bind:class"]], "model.active ? 'on' : ''")
})

test_that("bind_style adds x-bind:style", {
  el <- tags$div() |> bind_style("{ opacity: model.x }")
  expect_equal(el$attribs[["x-bind:style"]], "{ opacity: model.x }")
})

test_that("bind_disabled adds x-bind:disabled", {
  btn <- tags$button("Go") |> bind_disabled("model.loading")
  expect_equal(btn$attribs[["x-bind:disabled"]], "model.loading")
})

test_that("bind_attr adds x-bind:{name}", {
  el <- tags$a() |> bind_attr("href", "model.url")
  expect_equal(el$attribs[["x-bind:href"]], "model.url")
})

# -- add_to_input finds form element inside wrapper ---------------------------

test_that("bind_value finds input inside wrapper div", {
  wrapper <- mvu_text_input("Name") |> bind_value("model.name")
  html <- as.character(wrapper)
  expect_match(html, "x-bind:value")
  expect_match(html, "model.name")
})

test_that("on_input finds input inside wrapper div", {
  wrapper <- mvu_text_input("Name") |> on_input("set_name")
  html <- as.character(wrapper)
  expect_match(html, "@change")
  expect_match(html, "set_name")
})

test_that("on_input with debounce finds input inside wrapper", {
  wrapper <- mvu_text_input("Name") |> on_input("set_name", debounce = 300)
  html <- as.character(wrapper)
  expect_match(html, "input.debounce.300ms")
})

# -- mvu_button ---------------------------------------------------------------

test_that("mvu_button generates styled button", {
  btn <- mvu_button("Go")
  expect_equal(btn$name, "button")
  expect_match(as.character(btn), "btn btn-default")
  expect_match(as.character(btn), "Go")
})

test_that("mvu_button with on_click adds @click", {
  btn <- mvu_button("Go") |> on_click("go")
  expect_equal(btn$attribs[["@click"]], "send('go')")
})

test_that("mvu_button custom class", {
  btn <- mvu_button("X", class = "btn btn-danger")
  expect_match(as.character(btn), "btn btn-danger")
})

# -- mvu_text_input -----------------------------------------------------------

test_that("mvu_text_input generates text input with label", {
  ti <- mvu_text_input("Name")
  html <- as.character(ti)
  expect_match(html, 'type="text"')
  expect_match(html, "<label")
  expect_match(html, "Name")
  expect_match(html, "form-control")
})

test_that("mvu_text_input with placeholder", {
  ti <- mvu_text_input("Search", placeholder = "Type...")
  html <- as.character(ti)
  expect_match(html, "Type...")
})

test_that("mvu_text_input NULL label omits label", {
  ti <- mvu_text_input(NULL)
  html <- as.character(ti)
  expect_false(grepl("<label", html))
})

# -- mvu_numeric_input --------------------------------------------------------

test_that("mvu_numeric_input generates number input", {
  ni <- mvu_numeric_input("Age", min = 0, max = 120)
  html <- as.character(ni)
  expect_match(html, 'type="number"')
  expect_match(html, 'min="0"')
  expect_match(html, 'max="120"')
})

# -- mvu_password_input -------------------------------------------------------

test_that("mvu_password_input generates password input", {
  pi <- mvu_password_input("Password")
  html <- as.character(pi)
  expect_match(html, 'type="password"')
})

# -- mvu_textarea_input -------------------------------------------------------

test_that("mvu_textarea_input generates textarea", {
  ta <- mvu_textarea_input("Bio", rows = 5)
  html <- as.character(ta)
  expect_match(html, "<textarea")
  expect_match(html, 'rows="5"')
})

# -- mvu_select_input ---------------------------------------------------------

test_that("mvu_select_input generates select with options", {
  sel <- mvu_select_input("Color",
    choices = c("Red" = "red", "Blue" = "blue")
  )
  html <- as.character(sel)
  expect_match(html, "<select")
  expect_match(html, 'value="red"')
  expect_match(html, "Red")
  expect_match(html, "Blue")
})

test_that("mvu_select_input with bind_value adds x-bind:value", {
  sel <- mvu_select_input("Color",
    choices = c("Red" = "red", "Blue" = "blue")
  ) |>
    bind_value("model.color")
  html <- as.character(sel)
  expect_match(html, "x-bind:value")
})

test_that("mvu_select_input unnamed choices use value as label", {
  sel <- mvu_select_input("X", choices = c("a", "b"))
  html <- as.character(sel)
  expect_match(html, ">a<")
  expect_match(html, ">b<")
})

# -- mvu_checkbox_input -------------------------------------------------------

test_that("mvu_checkbox_input generates checkbox", {
  cb <- mvu_checkbox_input("Agree")
  html <- as.character(cb)
  expect_match(html, 'type="checkbox"')
  expect_match(html, "Agree")
})

test_that("mvu_checkbox_input with bind_checked adds x-bind:checked", {
  cb <- mvu_checkbox_input("Agree") |>
    bind_checked("model.agree") |>
    on_check("toggle")
  html <- as.character(cb)
  expect_match(html, "x-bind:checked")
  expect_match(html, "toggle")
})

# -- mvu_checkbox_group_input ------------------------------------------------

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

# -- mvu_radio_input ----------------------------------------------------------

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

# -- mvu_slider_input ---------------------------------------------------------

test_that("mvu_slider_input generates range input", {
  sl <- mvu_slider_input("Vol", min = 0, max = 100)
  html <- as.character(sl)
  expect_match(html, 'type="range"')
  expect_match(html, 'min="0"')
  expect_match(html, 'max="100"')
})

test_that("mvu_slider_input with bind_value", {
  sl <- mvu_slider_input("Vol", min = 0, max = 100) |>
    bind_value("model.vol") |>
    on_input_num("set_vol")
  html <- as.character(sl)
  expect_match(html, "x-bind:value")
  expect_match(html, "set_vol")
})

test_that("mvu_slider_input custom step", {
  sl <- mvu_slider_input("X", min = 0, max = 10, step = 0.5)
  html <- as.character(sl)
  expect_match(html, 'step="0.5"')
})

# -- mvu_date_input -----------------------------------------------------------

test_that("mvu_date_input generates date input", {
  di <- mvu_date_input("Birthday")
  html <- as.character(di)
  expect_match(html, 'type="date"')
})

test_that("mvu_date_input with bind_value", {
  di <- mvu_date_input("Birthday") |>
    bind_value("model.bday") |>
    on_input("set_bday")
  html <- as.character(di)
  expect_match(html, "x-bind:value")
  expect_match(html, "set_bday")
})

test_that("mvu_date_input with min/max constraints", {
  di <- mvu_date_input("Date", min = "2020-01-01", max = "2030-12-31")
  html <- as.character(di)
  expect_match(html, 'min="2020-01-01"')
  expect_match(html, 'max="2030-12-31"')
})

# -- mvu_date_range_input -----------------------------------------------------

test_that("mvu_date_range_input generates two date inputs", {
  dr <- mvu_date_range_input("Trip")
  html <- as.character(dr)
  expect_equal(length(gregexpr('type="date"', html)[[1]]), 2)
})

# -- mvu_file_input -----------------------------------------------------------

test_that("mvu_file_input generates file input", {
  fi <- mvu_file_input("Upload")
  html <- as.character(fi)
  expect_match(html, 'type="file"')
})

test_that("mvu_file_input with on_file adds event handler", {
  fi <- mvu_file_input("Upload") |> on_file("upload")
  html <- as.character(fi)
  expect_match(html, "f.name")
  expect_match(html, "upload")
})

test_that("mvu_file_input with accept and multiple", {
  fi <- mvu_file_input("Photos",
    accept = c("image/*", ".png"),
    multiple = TRUE
  )
  html <- as.character(fi)
  expect_match(html, "image/\\*")
  expect_match(html, "multiple")
})

# -- Shiny parity tests -------------------------------------------------------
#
# Strip framework-specific attributes from both Shiny and MVU HTML, then
# compare. Inputs with fundamentally different implementations (slider,
# file, date) are excluded.

strip_shiny <- function(html) {
  html <- gsub(' id="[^"]*"', "", html)
  html <- gsub(' for="[^"]*"', "", html)
  html <- gsub(' name="[^"]*"', "", html)
  html <- gsub(' data-[a-z-]+="[^"]*"', "", html)
  html <- gsub(' role="[^"]*"', "", html)
  html <- gsub(' aria-[a-z-]+="[^"]*"', "", html)
  html <- gsub(' value="[^"]*"', "", html)
  html <- gsub(" selected", "", html)
  html <- gsub(' checked="checked"', "", html)
  shiny_classes <- c(
    "shiny-input-container-inline",
    "shiny-input-checkboxgroup", "shiny-input-radiogroup",
    "shiny-input-container", "shiny-input-textarea",
    "shiny-input-password", "shiny-input-checkbox",
    "shiny-input-number", "shiny-input-select",
    "shiny-input-text", "shiny-input-file",
    "shiny-options-group", "shiny-label-null", "action-button"
  )
  for (cls in shiny_classes) {
    html <- gsub(paste0(" ?", cls), "", html)
  }
  html <- gsub(' class=""', "", html)
  html <- gsub('class=" ', 'class="', html)
  html <- gsub("\\s+", " ", html)
  html <- gsub("> <", "><", html)
  trimws(html)
}

strip_mvu <- function(html) {
  html <- gsub(' id="[^"]*"', "", html)
  html <- gsub(' for="[^"]*"', "", html)
  html <- gsub(' name="[^"]*"', "", html)
  html <- gsub(' value="[^"]*"', "", html)
  html <- gsub(' @[a-z.0-9]+="[^"]*"', "", html)
  html <- gsub(' x-bind:[a-z-]+="[^"]*"', "", html)
  html <- gsub("\\s+", " ", html)
  html <- gsub("> <", "><", html)
  trimws(html)
}

test_that("mvu_button matches shiny::actionButton", {
  shiny_html <- as.character(shiny::actionButton("id", "Go"))
  mvu_html <- as.character(mvu_button("Go"))
  expect_equal(strip_shiny(shiny_html), strip_mvu(mvu_html))
})

test_that("mvu_button with width matches shiny::actionButton", {
  shiny_html <- as.character(shiny::actionButton("id", "Go", width = "200px"))
  mvu_html <- as.character(mvu_button("Go", width = "200px"))
  expect_equal(strip_shiny(shiny_html), strip_mvu(mvu_html))
})

test_that("mvu_text_input matches shiny::textInput", {
  shiny_html <- as.character(shiny::textInput("id", "Name"))
  mvu_html <- as.character(mvu_text_input("Name"))
  expect_equal(strip_shiny(shiny_html), strip_mvu(mvu_html))
})

test_that("mvu_text_input with placeholder matches shiny::textInput", {
  shiny_html <- as.character(
    shiny::textInput("id", "Name", placeholder = "Enter name")
  )
  mvu_html <- as.character(mvu_text_input("Name", placeholder = "Enter name"))
  expect_equal(strip_shiny(shiny_html), strip_mvu(mvu_html))
})

test_that("mvu_text_input with width matches shiny::textInput", {
  shiny_html <- as.character(shiny::textInput("id", "Name", width = "300px"))
  mvu_html <- as.character(mvu_text_input("Name", width = "300px"))
  expect_equal(strip_shiny(shiny_html), strip_mvu(mvu_html))
})

test_that("mvu_numeric_input matches shiny::numericInput", {
  shiny_html <- as.character(
    shiny::numericInput("id", "Age", value = 0, min = 0, max = 120)
  )
  mvu_html <- as.character(mvu_numeric_input("Age", min = 0, max = 120))
  expect_equal(strip_shiny(shiny_html), strip_mvu(mvu_html))
})

test_that("mvu_password_input matches shiny::passwordInput", {
  shiny_html <- as.character(shiny::passwordInput("id", "Password"))
  mvu_html <- as.character(mvu_password_input("Password"))
  expect_equal(strip_shiny(shiny_html), strip_mvu(mvu_html))
})

test_that("mvu_textarea_input matches shiny::textAreaInput", {
  shiny_html <- as.character(shiny::textAreaInput("id", "Bio", rows = 4))
  mvu_html <- as.character(mvu_textarea_input("Bio", rows = 4))
  expect_equal(strip_shiny(shiny_html), strip_mvu(mvu_html))
})

test_that("mvu_select_input matches shiny::selectInput", {
  shiny_html <- as.character(
    shiny::selectInput("id", "Color",
      choices = c("Red" = "red", "Blue" = "blue"),
      selectize = FALSE
    )
  )
  mvu_html <- as.character(
    mvu_select_input("Color", choices = c("Red" = "red", "Blue" = "blue"))
  )
  expect_equal(strip_shiny(shiny_html), strip_mvu(mvu_html))
})

test_that("mvu_checkbox_input matches shiny::checkboxInput", {
  shiny_html <- as.character(shiny::checkboxInput("id", "Agree"))
  mvu_html <- as.character(mvu_checkbox_input("Agree"))
  expect_equal(strip_shiny(shiny_html), strip_mvu(mvu_html))
})

test_that("mvu_checkbox_group_input matches shiny::checkboxGroupInput", {
  shiny_html <- as.character(
    shiny::checkboxGroupInput("id", "Toppings", c("Cheese", "Peppers"))
  )
  mvu_html <- as.character(
    mvu_checkbox_group_input("Toppings", c("Cheese", "Peppers"),
      msg = "toggle", selected = "model.sel"
    )
  )
  expect_equal(strip_shiny(shiny_html), strip_mvu(mvu_html))
})

test_that("mvu_checkbox_group_input inline matches shiny", {
  shiny_html <- as.character(
    shiny::checkboxGroupInput("id", "T", c("A", "B"), inline = TRUE)
  )
  mvu_html <- as.character(
    mvu_checkbox_group_input("T", c("A", "B"),
      msg = "tog", selected = "model.s", inline = TRUE
    )
  )
  expect_equal(strip_shiny(shiny_html), strip_mvu(mvu_html))
})

test_that("mvu_radio_input matches shiny::radioButtons", {
  shiny_html <- as.character(
    shiny::radioButtons("id", "Pet", c("Dog" = "dog", "Cat" = "cat"))
  )
  mvu_html <- as.character(
    mvu_radio_input("Pet", c("Dog" = "dog", "Cat" = "cat"),
      msg = "set_pet", selected = "model.pet"
    )
  )
  expect_equal(strip_shiny(shiny_html), strip_mvu(mvu_html))
})

test_that("mvu_radio_input inline matches shiny", {
  shiny_html <- as.character(
    shiny::radioButtons("id", "P", c("X" = "x", "Y" = "y"), inline = TRUE)
  )
  mvu_html <- as.character(
    mvu_radio_input("P", c("X" = "x", "Y" = "y"),
      msg = "set", selected = "model.p", inline = TRUE
    )
  )
  expect_equal(strip_shiny(shiny_html), strip_mvu(mvu_html))
})

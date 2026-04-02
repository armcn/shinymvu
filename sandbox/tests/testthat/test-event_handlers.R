test_that("on_click adds @click attribute with message", {
  tags$button("Click") |>
    on_click("increment") |>
    expect_html('<button @click="send(&#39;increment&#39;)">Click</button>')
})

test_that("on_click preserves existing attributes", {
  tags$button("Go", class = "btn") |>
    on_click("go") |>
    expect_html('<button class="btn" @click="send(&#39;go&#39;)">Go</button>')
})

test_that("send_js wraps message in send() call", {
  expect_equal(send_js("increment"), "send('increment')")
})

test_that("send_js includes value expression", {
  expect_equal(send_js("set", "42"), "send('set', 42)")
})

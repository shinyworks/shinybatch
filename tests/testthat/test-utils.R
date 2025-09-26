test_that(".get_value_maybe_reactive returns non-reactive values", {
  expect_identical("a", .get_value_maybe_reactive("a"))
})

test_that(".get_value_maybe_reactive returns the evaluated value of a reactive", {
  rctv <- reactiveVal("a")
  expect_identical("a", isolate(.get_value_maybe_reactive(rctv)))
  rctv("b")
  expect_identical("b", isolate(.get_value_maybe_reactive(rctv)))
})

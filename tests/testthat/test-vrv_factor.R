test_that("vrv_factor() initializes as expected", {
  mock_data <- list("A" = "A1")
  level <- shiny::reactiveVal("A")
  group <- vrv_factor(
    levels = {
      mock_data[[level()]]
    },
    default = {
      mock_data[[level()]][[1]]
    },
    value = "A1"
  )
  expect_equal(isolate(level()), "A")
  expect_equal(isolate(group()), "A1")
})

test_that("vrv_factor() allows setting within valid levels", {
  mock_data <- list("A" = c("A1", "A2"))
  level <- shiny::reactiveVal("A")
  group <- vrv_factor(
    levels = {
      mock_data[[level()]]
    },
    default = {
      mock_data[[level()]][[1]]
    },
    value = "A1"
  )
  group("A2")
  expect_equal(isolate(level()), "A")
  expect_equal(isolate(group()), "A2")
})

test_that("vrv_factor() sets value to default when invalid", {
  mock_data <- list("A" = "A1", "B" = "B1")
  level <- shiny::reactiveVal("A")
  group <- vrv_factor(
    levels = {
      mock_data[[level()]]
    },
    default = {
      mock_data[[level()]][[1]]
    },
    value = "A1"
  )
  level("B")
  expect_equal(isolate(level()), "B")
  expect_equal(isolate(group()), "B1")
})

test_that("vrv_factor() doesn't break with overlapping groups", {
  mock_data <- list("B" = "B2", "C" = c("A1", "B2"))
  level <- shiny::reactiveVal("B")
  group <- vrv_factor(
    levels = {
      mock_data[[level()]]
    },
    default = {
      mock_data[[level()]][[1]]
    },
    value = "B2"
  )
  # Group should remain valid, since both B and C have "B2"
  level("C")
  expect_equal(isolate(level()), "C")
  expect_equal(isolate(group()), "B2")
})

test_that("vrv_factor handles NULL initialization", {
  mock_data <- list("A" = "A1", "B" = "B1")
  level <- shiny::reactiveVal("A")
  factor_vrv <- vrv_factor(
    levels = {
      mock_data[[level()]]
    },
    default = {
      mock_data[[level()]][[1]]
    },
    value = NULL # Explicitly start with NULL
  )
  # Should immediately be set to the default value for "A"
  expect_equal(isolate(factor_vrv()), "A1")

  # Change level, should switch to the new default
  level("B")
  expect_equal(isolate(factor_vrv()), "B1")
})

test_that("vrv_factor handles being set to NULL", {
  mock_data <- list("A" = c("A1", "A2"), "B" = "B1")
  level <- shiny::reactiveVal("A")
  factor_vrv <- vrv_factor(
    levels = {
      mock_data[[level()]]
    },
    default = {
      mock_data[[level()]][[1]]
    },
    value = "A2" # Start with a valid value
  )
  expect_equal(isolate(factor_vrv()), "A2")

  # Imperatively set to NULL
  factor_vrv(NULL)
  # On the next read, it should be validated and revert to the default
  expect_equal(isolate(factor_vrv()), "A1")
})

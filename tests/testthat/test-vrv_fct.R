# vrv_fct ----

test_that("vrv_fct() initializes as expected", {
  mock_data <- list("A" = "A1")
  level <- shiny::reactiveVal("A")
  group <- vrv_fct(
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
  expect_false(isolate(group$is_default()))
  expect_null(isolate(group$error()))
})

test_that("vrv_fct() allows setting within valid levels", {
  mock_data <- list("A" = c("A1", "A2"))
  level <- shiny::reactiveVal("A")
  group <- vrv_fct(
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
  expect_false(isolate(group$is_default()))
  expect_null(isolate(group$error()))
})

test_that("vrv_fct() sets value to default when invalid", {
  mock_data <- list("A" = "A1", "B" = "B1")
  level <- shiny::reactiveVal("A")
  group <- vrv_fct(
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
  expect_true(isolate(group$is_default()))
  error <- isolate(group$error())
  expect_s3_class(error, "captured-stbl-error-fct_levels")
  class(error) <- sub("captured-", "", class(error))
  expect_error({
    signalCondition(error)
  })
  expect_snapshot(
    {
      signalCondition(error)
    },
    error = TRUE
  )
})

test_that("vrv_fct() doesn't break with overlapping groups", {
  mock_data <- list("B" = "B2", "C" = c("A1", "B2"))
  level <- shiny::reactiveVal("B")
  group <- vrv_fct(
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
  expect_false(isolate(group$is_default()))
  expect_null(isolate(group$error()))
})

test_that("vrv_fct handles NULL initialization when NULL isn't allowed", {
  mock_data <- list("A" = "A1", "B" = "B1")
  level <- shiny::reactiveVal("A")
  factor_vrv <- vrv_fct(
    levels = {
      mock_data[[level()]]
    },
    default = {
      mock_data[[level()]][[1]]
    },
    value = NULL,
    allow_null = FALSE
  )
  # Should immediately be set to the default value for "A"
  expect_equal(isolate(factor_vrv()), "A1")
  expect_true(isolate(factor_vrv$is_default()))

  # Change level, should switch to the new default
  level("B")
  expect_equal(isolate(factor_vrv()), "B1")
  expect_true(isolate(factor_vrv$is_default()))
})

test_that("vrv_fct handles being set to NULL when NULL isn't allowed", {
  mock_data <- list("A" = c("A1", "A2"), "B" = "B1")
  level <- shiny::reactiveVal("A")
  factor_vrv <- vrv_fct(
    levels = {
      mock_data[[level()]]
    },
    default = {
      mock_data[[level()]][[1]]
    },
    value = "A2",
    allow_null = FALSE
  )
  expect_equal(isolate(factor_vrv()), "A2")

  # Imperatively set to NULL
  factor_vrv(NULL)
  # On the next read, it should be validated and revert to the default
  expect_equal(isolate(factor_vrv()), "A1")
  expect_true(isolate(factor_vrv$is_default()))
})

# vrv_fct_scalar ----

test_that("vrv_fct_scalar() initializes as expected", {
  mock_data <- list("A" = "A1")
  level <- shiny::reactiveVal("A")
  group <- vrv_fct_scalar(
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
  expect_false(isolate(group$is_default()))
  expect_null(isolate(group$error()))
})

test_that("vrv_fct_scalar() allows setting a valid value", {
  mock_data <- list("A" = paste0("A", 1:2))
  level <- shiny::reactiveVal("A")
  group <- vrv_fct_scalar(
    levels = {
      mock_data[[level()]]
    },
    default = {
      mock_data[[level()]][[1]]
    },
    value = "A1"
  )
  group("A2")
  expect_equal(isolate(group()), "A2")
  expect_false(isolate(group$is_default()))
  expect_null(isolate(group$error()))
})

test_that("vrv_fct_scalar() sets value to default when invalid (levels)", {
  mock_data <- list("A" = "A1", "B" = "B1")
  level <- shiny::reactiveVal("A")
  group <- vrv_fct_scalar(
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
  expect_true(isolate(group$is_default()))
  error <- isolate(group$error())
  expect_s3_class(error, "captured-stbl-error-fct_levels")
  class(error) <- sub("captured-", "", class(error))
  expect_error({
    signalCondition(error)
  })
  expect_snapshot(
    {
      signalCondition(error)
    },
    error = TRUE
  )
})

test_that("vrv_fct_scalar() sets value to default when invalid (size)", {
  mock_data <- list("A" = paste0("A", 1:2))
  level <- shiny::reactiveVal("A")
  group <- vrv_fct_scalar(
    levels = {
      mock_data[[level()]]
    },
    default = {
      mock_data[[level()]][[1]]
    },
    value = "A1"
  )
  group(c("A1", "A2"))
  expect_equal(isolate(group()), "A1")
  expect_true(isolate(group$is_default()))
  error <- isolate(group$error())
  expect_s3_class(error, "captured-stbl-error-non_scalar")
  class(error) <- sub("captured-", "", class(error))
  expect_error({
    signalCondition(error)
  })
  expect_snapshot(
    {
      signalCondition(error)
    },
    error = TRUE
  )
})

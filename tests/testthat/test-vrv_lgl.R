# vrv_lgl ----

test_that("vrv_lgl() initializes as expected", {
  lgl_vrv <- vrv_lgl(value = TRUE)
  expect_true(isolate(lgl_vrv()))
  expect_false(isolate(lgl_vrv$is_default()))
  expect_null(isolate(lgl_vrv$error()))
})

test_that("vrv_lgl() allows setting a valid value", {
  lgl_vrv <- vrv_lgl(value = TRUE)
  lgl_vrv(FALSE)
  expect_false(isolate(lgl_vrv()))
  expect_false(isolate(lgl_vrv$is_default()))
  expect_null(isolate(lgl_vrv$error()))
})

test_that("vrv_lgl() sets value to default when invalid (size)", {
  lgl_vrv <- vrv_lgl(
    value = TRUE,
    default = FALSE,
    max_size = 1
  )
  lgl_vrv(c(TRUE, TRUE))
  expect_false(isolate(lgl_vrv()))
  expect_true(isolate(lgl_vrv$is_default()))
  error <- isolate(lgl_vrv$error())
  expect_s3_class(error, "captured-stbl-error-size_too_large")
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

test_that("vrv_lgl() handles NULL initialization", {
  # allow_null = TRUE (default)
  lgl_vrv_null_ok <- vrv_lgl(
    value = NULL,
    default = FALSE
  )
  expect_null(isolate(lgl_vrv_null_ok()))
  expect_false(isolate(lgl_vrv_null_ok$is_default()))

  # allow_null = FALSE
  lgl_vrv_null_bad <- vrv_lgl(
    value = NULL,
    default = FALSE,
    allow_null = FALSE
  )
  expect_false(isolate(lgl_vrv_null_bad()))
  expect_true(isolate(lgl_vrv_null_bad$is_default()))
  error <- isolate(lgl_vrv_null_bad$error())
  expect_s3_class(error, "captured-stbl-error-bad_null")
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

test_that("vrv_lgl() handles being set to NULL", {
  # allow_null = TRUE (default)
  lgl_vrv_null_ok <- vrv_lgl(
    value = TRUE,
    default = FALSE
  )
  lgl_vrv_null_ok(NULL)
  expect_null(isolate(lgl_vrv_null_ok()))
  expect_false(isolate(lgl_vrv_null_ok$is_default()))

  # allow_null = FALSE
  lgl_vrv_null_bad <- vrv_lgl(
    value = TRUE,
    default = FALSE,
    allow_null = FALSE
  )
  lgl_vrv_null_bad(NULL)
  expect_false(isolate(lgl_vrv_null_bad()))
  expect_true(isolate(lgl_vrv_null_bad$is_default()))
  error <- isolate(lgl_vrv_null_bad$error())
  expect_s3_class(error, "captured-stbl-error-bad_null")
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

# vrv_lgl_scalar ----

test_that("vrv_lgl_scalar() initializes as expected", {
  lgl_vrv <- vrv_lgl_scalar(value = TRUE)
  expect_true(isolate(lgl_vrv()))
  expect_false(isolate(lgl_vrv$is_default()))
  expect_null(isolate(lgl_vrv$error()))
})

test_that("vrv_lgl_scalar() allows setting a valid value", {
  lgl_vrv <- vrv_lgl_scalar(value = TRUE)
  lgl_vrv(FALSE)
  expect_false(isolate(lgl_vrv()))
  expect_false(isolate(lgl_vrv$is_default()))
  expect_null(isolate(lgl_vrv$error()))
})

test_that("vrv_lgl_scalar() sets value to default when invalid (size)", {
  lgl_vrv <- vrv_lgl_scalar(
    value = TRUE,
    default = FALSE
  )
  lgl_vrv(c(TRUE, TRUE))
  expect_false(isolate(lgl_vrv()))
  expect_true(isolate(lgl_vrv$is_default()))
  error <- isolate(lgl_vrv$error())
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

test_that("vrv_lgl_scalar() handles NULL initialization", {
  # allow_null = TRUE (default)
  lgl_vrv_null_ok <- vrv_lgl_scalar(
    value = NULL,
    default = FALSE
  )
  expect_null(isolate(lgl_vrv_null_ok()))
  expect_false(isolate(lgl_vrv_null_ok$is_default()))

  # allow_null = FALSE
  lgl_vrv_null_bad <- vrv_lgl_scalar(
    value = NULL,
    default = FALSE,
    allow_null = FALSE
  )
  expect_false(isolate(lgl_vrv_null_bad()))
  expect_true(isolate(lgl_vrv_null_bad$is_default()))
  error <- isolate(lgl_vrv_null_bad$error())
  expect_s3_class(error, "captured-stbl-error-bad_null")
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

test_that("vrv_lgl_scalar() handles zero-length logical vector", {
  # allow_zero_length = TRUE (default)
  lgl_vrv_zero_ok <- vrv_lgl_scalar(
    value = TRUE,
    default = FALSE
  )
  lgl_vrv_zero_ok(logical())
  expect_equal(isolate(lgl_vrv_zero_ok()), logical())
  expect_false(isolate(lgl_vrv_zero_ok$is_default()))

  # allow_zero_length = FALSE
  lgl_vrv_zero_bad <- vrv_lgl_scalar(
    value = TRUE,
    default = FALSE,
    allow_zero_length = FALSE
  )
  lgl_vrv_zero_bad(logical())
  expect_false(isolate(lgl_vrv_zero_bad()))
  expect_true(isolate(lgl_vrv_zero_bad$is_default()))
  error <- isolate(lgl_vrv_zero_bad$error())
  expect_s3_class(error, "captured-stbl-error-bad_empty")
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

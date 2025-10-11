# vrv_int ----

test_that("vrv_int() initializes as expected", {
  int_vrv <- vrv_int(value = 1L)
  expect_equal(isolate(int_vrv()), 1L)
  expect_false(isolate(int_vrv$is_default()))
  expect_null(isolate(int_vrv$error()))
})

test_that("vrv_int() allows setting a valid value", {
  int_vrv <- vrv_int(value = 1L)
  int_vrv(2L)
  expect_equal(isolate(int_vrv()), 2L)
  expect_false(isolate(int_vrv$is_default()))
  expect_null(isolate(int_vrv$error()))
})

test_that("vrv_int() sets value to default when invalid (size)", {
  int_vrv <- vrv_int(
    value = 1L,
    default = 99L,
    max_size = 1
  )
  int_vrv(c(1L, 2L))
  expect_equal(isolate(int_vrv()), 99L)
  expect_true(isolate(int_vrv$is_default()))
  error <- isolate(int_vrv$error())
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

test_that("vrv_int() sets value to default when invalid (min_value)", {
  int_vrv <- vrv_int(
    value = 10L,
    default = 99L,
    min_value = 5L
  )
  int_vrv(4L)
  expect_equal(isolate(int_vrv()), 99L)
  expect_true(isolate(int_vrv$is_default()))
  error <- isolate(int_vrv$error())
  expect_s3_class(error, "captured-stbl-error-outside_range")
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

test_that("vrv_int() sets value to default when invalid (max_value)", {
  int_vrv <- vrv_int(
    value = 10L,
    default = 99L,
    max_value = 15L
  )
  int_vrv(16L)
  expect_equal(isolate(int_vrv()), 99L)
  expect_true(isolate(int_vrv$is_default()))
  error <- isolate(int_vrv$error())
  expect_s3_class(error, "captured-stbl-error-outside_range")
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

test_that("vrv_int() coerces character", {
  int_vrv <- vrv_int(value = "1")
  expect_equal(isolate(int_vrv()), 1L)

  int_vrv("2.0")
  expect_equal(isolate(int_vrv()), 2L)

  int_vrv_no_coerce <- vrv_int(
    value = 1L,
    default = 99L,
    coerce_character = FALSE
  )
  int_vrv_no_coerce("2")
  expect_equal(isolate(int_vrv_no_coerce()), 99L)
})

test_that("vrv_int() coerces factor", {
  int_vrv <- vrv_int(value = factor("1"))
  expect_equal(isolate(int_vrv()), 1L)

  int_vrv(factor("2.0"))
  expect_equal(isolate(int_vrv()), 2L)

  int_vrv_no_coerce <- vrv_int(
    value = 1L,
    default = 99L,
    coerce_factor = FALSE
  )
  int_vrv_no_coerce(factor("2"))
  expect_equal(isolate(int_vrv_no_coerce()), 99L)
})


test_that("vrv_int() handles NULL initialization", {
  # allow_null = TRUE (default)
  int_vrv_null_ok <- vrv_int(
    value = NULL,
    default = 99L
  )
  expect_null(isolate(int_vrv_null_ok()))
  expect_false(isolate(int_vrv_null_ok$is_default()))

  # allow_null = FALSE
  int_vrv_null_bad <- vrv_int(
    value = NULL,
    default = 99L,
    allow_null = FALSE
  )
  expect_equal(isolate(int_vrv_null_bad()), 99L)
  expect_true(isolate(int_vrv_null_bad$is_default()))
  error <- isolate(int_vrv_null_bad$error())
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

test_that("vrv_int() handles being set to NULL", {
  # allow_null = TRUE (default)
  int_vrv_null_ok <- vrv_int(
    value = 1L,
    default = 99L
  )
  int_vrv_null_ok(NULL)
  expect_null(isolate(int_vrv_null_ok()))
  expect_false(isolate(int_vrv_null_ok$is_default()))

  # allow_null = FALSE
  int_vrv_null_bad <- vrv_int(
    value = 1L,
    default = 99L,
    allow_null = FALSE
  )
  int_vrv_null_bad(NULL)
  expect_equal(isolate(int_vrv_null_bad()), 99L)
  expect_true(isolate(int_vrv_null_bad$is_default()))
  error <- isolate(int_vrv_null_bad$error())
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

# vrv_int_scalar ----

test_that("vrv_int_scalar() initializes as expected", {
  int_vrv <- vrv_int_scalar(value = 1L)
  expect_equal(isolate(int_vrv()), 1L)
  expect_false(isolate(int_vrv$is_default()))
  expect_null(isolate(int_vrv$error()))
})

test_that("vrv_int_scalar() allows setting a valid value", {
  int_vrv <- vrv_int_scalar(value = 1L)
  int_vrv(2L)
  expect_equal(isolate(int_vrv()), 2L)
  expect_false(isolate(int_vrv$is_default()))
  expect_null(isolate(int_vrv$error()))
})

test_that("vrv_int_scalar() sets value to default when invalid (size)", {
  int_vrv <- vrv_int_scalar(
    value = 1L,
    default = 99L
  )
  int_vrv(c(1L, 2L))
  expect_equal(isolate(int_vrv()), 99L)
  expect_true(isolate(int_vrv$is_default()))
  error <- isolate(int_vrv$error())
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

test_that("vrv_int_scalar() sets value to default when invalid (max_value)", {
  int_vrv <- vrv_int_scalar(
    value = 10L,
    default = 99L,
    max_value = 15L
  )
  int_vrv(16L)
  expect_equal(isolate(int_vrv()), 99L)
  expect_true(isolate(int_vrv$is_default()))
  error <- isolate(int_vrv$error())
  expect_s3_class(error, "captured-stbl-error-outside_range")
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

test_that("vrv_int_scalar() handles NULL initialization", {
  # allow_null = TRUE (default)
  int_vrv_null_ok <- vrv_int_scalar(
    value = NULL,
    default = 99L
  )
  expect_null(isolate(int_vrv_null_ok()))
  expect_false(isolate(int_vrv_null_ok$is_default()))

  # allow_null = FALSE
  int_vrv_null_bad <- vrv_int_scalar(
    value = NULL,
    default = 99L,
    allow_null = FALSE
  )
  expect_equal(isolate(int_vrv_null_bad()), 99L)
  expect_true(isolate(int_vrv_null_bad$is_default()))
  error <- isolate(int_vrv_null_bad$error())
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

test_that("vrv_int_scalar() handles zero-length integer vector", {
  # allow_zero_length = TRUE (default)
  int_vrv_zero_ok <- vrv_int_scalar(
    value = 1L,
    default = 99L
  )
  int_vrv_zero_ok(integer())
  expect_equal(isolate(int_vrv_zero_ok()), integer())
  expect_false(isolate(int_vrv_zero_ok$is_default()))

  # allow_zero_length = FALSE
  int_vrv_zero_bad <- vrv_int_scalar(
    value = 1L,
    default = 99L,
    allow_zero_length = FALSE
  )
  int_vrv_zero_bad(integer())
  expect_equal(isolate(int_vrv_zero_bad()), 99L)
  expect_true(isolate(int_vrv_zero_bad$is_default()))
  error <- isolate(int_vrv_zero_bad$error())
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

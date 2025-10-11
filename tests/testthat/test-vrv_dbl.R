# vrv_dbl ----

test_that("vrv_dbl() initializes as expected", {
  dbl_vrv <- vrv_dbl(value = 1.1)
  expect_equal(isolate(dbl_vrv()), 1.1)
  expect_false(isolate(dbl_vrv$is_default()))
  expect_null(isolate(dbl_vrv$error()))
})

test_that("vrv_dbl() allows setting a valid value", {
  dbl_vrv <- vrv_dbl(value = 1.1)
  dbl_vrv(2.2)
  expect_equal(isolate(dbl_vrv()), 2.2)
  expect_false(isolate(dbl_vrv$is_default()))
  expect_null(isolate(dbl_vrv$error()))
})

test_that("vrv_dbl() sets value to default when invalid (size)", {
  dbl_vrv <- vrv_dbl(
    value = 1.1,
    default = 9.9,
    max_size = 1
  )
  dbl_vrv(c(1.1, 2.2))
  expect_equal(isolate(dbl_vrv()), 9.9)
  expect_true(isolate(dbl_vrv$is_default()))
  error <- isolate(dbl_vrv$error())
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

test_that("vrv_dbl() sets value to default when invalid (min_value)", {
  dbl_vrv <- vrv_dbl(
    value = 1.1,
    default = 9.9,
    min_value = 5
  )
  dbl_vrv(4)
  expect_equal(isolate(dbl_vrv()), 9.9)
  expect_true(isolate(dbl_vrv$is_default()))
  error <- isolate(dbl_vrv$error())
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

test_that("vrv_dbl() sets value to default when invalid (max_value)", {
  dbl_vrv <- vrv_dbl(
    value = 1.1,
    default = 9.9,
    max_value = 15
  )
  dbl_vrv(16)
  expect_equal(isolate(dbl_vrv()), 9.9)
  expect_true(isolate(dbl_vrv$is_default()))
  error <- isolate(dbl_vrv$error())
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

test_that("vrv_dbl() coerces character", {
  dbl_vrv <- vrv_dbl(value = "1.1")
  expect_equal(isolate(dbl_vrv()), 1.1)

  dbl_vrv("2.2")
  expect_equal(isolate(dbl_vrv()), 2.2)

  dbl_vrv_no_coerce <- vrv_dbl(
    value = 1.1,
    default = 9.9,
    coerce_character = FALSE
  )
  dbl_vrv_no_coerce("2")
  expect_equal(isolate(dbl_vrv_no_coerce()), 9.9)
})

test_that("vrv_dbl() coerces factor", {
  dbl_vrv <- vrv_dbl(value = factor("1.1"))
  expect_equal(isolate(dbl_vrv()), 1.1)

  dbl_vrv(factor("2.2"))
  expect_equal(isolate(dbl_vrv()), 2.2)

  dbl_vrv_no_coerce <- vrv_dbl(
    value = 1.1,
    default = 9.9,
    coerce_factor = FALSE
  )
  dbl_vrv_no_coerce(factor("2"))
  expect_equal(isolate(dbl_vrv_no_coerce()), 9.9)
})


test_that("vrv_dbl() handles NULL initialization", {
  # allow_null = TRUE (default)
  dbl_vrv_null_ok <- vrv_dbl(
    value = NULL,
    default = 9.9
  )
  expect_null(isolate(dbl_vrv_null_ok()))
  expect_false(isolate(dbl_vrv_null_ok$is_default()))

  # allow_null = FALSE
  dbl_vrv_null_bad <- vrv_dbl(
    value = NULL,
    default = 9.9,
    allow_null = FALSE
  )
  expect_equal(isolate(dbl_vrv_null_bad()), 9.9)
  expect_true(isolate(dbl_vrv_null_bad$is_default()))
  error <- isolate(dbl_vrv_null_bad$error())
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

test_that("vrv_dbl() handles being set to NULL", {
  # allow_null = TRUE (default)
  dbl_vrv_null_ok <- vrv_dbl(
    value = 1.1,
    default = 9.9
  )
  dbl_vrv_null_ok(NULL)
  expect_null(isolate(dbl_vrv_null_ok()))
  expect_false(isolate(dbl_vrv_null_ok$is_default()))

  # allow_null = FALSE
  dbl_vrv_null_bad <- vrv_dbl(
    value = 1.1,
    default = 9.9,
    allow_null = FALSE
  )
  dbl_vrv_null_bad(NULL)
  expect_equal(isolate(dbl_vrv_null_bad()), 9.9)
  expect_true(isolate(dbl_vrv_null_bad$is_default()))
  error <- isolate(dbl_vrv_null_bad$error())
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

# vrv_dbl_scalar ----

test_that("vrv_dbl_scalar() initializes as expected", {
  dbl_vrv <- vrv_dbl_scalar(value = 1.1)
  expect_equal(isolate(dbl_vrv()), 1.1)
  expect_false(isolate(dbl_vrv$is_default()))
  expect_null(isolate(dbl_vrv$error()))
})

test_that("vrv_dbl_scalar() allows setting a valid value", {
  dbl_vrv <- vrv_dbl_scalar(value = 1.1)
  dbl_vrv(2.2)
  expect_equal(isolate(dbl_vrv()), 2.2)
  expect_false(isolate(dbl_vrv$is_default()))
  expect_null(isolate(dbl_vrv$error()))
})

test_that("vrv_dbl_scalar() sets value to default when invalid (size)", {
  dbl_vrv <- vrv_dbl_scalar(
    value = 1.1,
    default = 9.9
  )
  dbl_vrv(c(1.1, 2.2))
  expect_equal(isolate(dbl_vrv()), 9.9)
  expect_true(isolate(dbl_vrv$is_default()))
  error <- isolate(dbl_vrv$error())
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

test_that("vrv_dbl_scalar() sets value to default when invalid (max_value)", {
  dbl_vrv <- vrv_dbl_scalar(
    value = 1.1,
    default = 9.9,
    max_value = 15
  )
  dbl_vrv(16)
  expect_equal(isolate(dbl_vrv()), 9.9)
  expect_true(isolate(dbl_vrv$is_default()))
  error <- isolate(dbl_vrv$error())
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

test_that("vrv_dbl_scalar() handles NULL initialization", {
  # allow_null = TRUE (default)
  dbl_vrv_null_ok <- vrv_dbl_scalar(
    value = NULL,
    default = 9.9
  )
  expect_null(isolate(dbl_vrv_null_ok()))
  expect_false(isolate(dbl_vrv_null_ok$is_default()))

  # allow_null = FALSE
  dbl_vrv_null_bad <- vrv_dbl_scalar(
    value = NULL,
    default = 9.9,
    allow_null = FALSE
  )
  expect_equal(isolate(dbl_vrv_null_bad()), 9.9)
  expect_true(isolate(dbl_vrv_null_bad$is_default()))
  error <- isolate(dbl_vrv_null_bad$error())
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

test_that("vrv_dbl_scalar() handles zero-length double vector", {
  # allow_zero_length = TRUE (default)
  dbl_vrv_zero_ok <- vrv_dbl_scalar(
    value = 1.1,
    default = 9.9
  )
  dbl_vrv_zero_ok(double())
  expect_equal(isolate(dbl_vrv_zero_ok()), double())
  expect_false(isolate(dbl_vrv_zero_ok$is_default()))

  # allow_zero_length = FALSE
  dbl_vrv_zero_bad <- vrv_dbl_scalar(
    value = 1.1,
    default = 9.9,
    allow_zero_length = FALSE
  )
  dbl_vrv_zero_bad(double())
  expect_equal(isolate(dbl_vrv_zero_bad()), 9.9)
  expect_true(isolate(dbl_vrv_zero_bad$is_default()))
  error <- isolate(dbl_vrv_zero_bad$error())
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

# vrv_character ----

test_that("vrv_character() initializes as expected", {
  char_vrv <- vrv_character(value = "a")
  expect_equal(isolate(char_vrv()), "a")
  expect_false(isolate(char_vrv$is_default()))
  expect_null(isolate(char_vrv$error()))
})

test_that("vrv_character() allows setting a valid value", {
  char_vrv <- vrv_character(value = "a")
  char_vrv("b")
  expect_equal(isolate(char_vrv()), "b")
  expect_false(isolate(char_vrv$is_default()))
  expect_null(isolate(char_vrv$error()))
})

test_that("vrv_character() sets value to default when invalid (size)", {
  char_vrv <- vrv_character(
    value = "a",
    default = "default",
    max_size = 1
  )
  char_vrv(c("a", "b"))
  expect_equal(isolate(char_vrv()), "default")
  expect_true(isolate(char_vrv$is_default()))
  error <- isolate(char_vrv$error())
  expect_s3_class(error, "captured-stbl_error_size_too_large")
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

test_that("vrv_character() sets value to default when invalid (regex)", {
  char_vrv <- vrv_character(
    value = "a",
    default = "default",
    regex = "^[a-z]+$"
  )
  char_vrv("123")
  expect_equal(isolate(char_vrv()), "default")
  expect_true(isolate(char_vrv$is_default()))
  error <- isolate(char_vrv$error())
  expect_s3_class(error, "captured-stbl_error_must")
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

test_that("vrv_character() handles NULL initialization", {
  # allow_null = TRUE (default)
  char_vrv_null_ok <- vrv_character(
    value = NULL,
    default = "default"
  )
  expect_null(isolate(char_vrv_null_ok()))
  expect_false(isolate(char_vrv_null_ok$is_default()))

  # allow_null = FALSE
  char_vrv_null_bad <- vrv_character(
    value = NULL,
    default = "default",
    allow_null = FALSE
  )
  expect_equal(isolate(char_vrv_null_bad()), "default")
  expect_true(isolate(char_vrv_null_bad$is_default()))
  error <- isolate(char_vrv_null_bad$error())
  expect_s3_class(error, "captured-stbl_error_bad_null")
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

test_that("vrv_character() handles being set to NULL", {
  # allow_null = TRUE (default)
  char_vrv_null_ok <- vrv_character(
    value = "a",
    default = "default"
  )
  char_vrv_null_ok(NULL)
  expect_null(isolate(char_vrv_null_ok()))
  expect_false(isolate(char_vrv_null_ok$is_default()))

  # allow_null = FALSE
  char_vrv_null_bad <- vrv_character(
    value = "a",
    default = "default",
    allow_null = FALSE
  )
  char_vrv_null_bad(NULL)
  expect_equal(isolate(char_vrv_null_bad()), "default")
  expect_true(isolate(char_vrv_null_bad$is_default()))
  error <- isolate(char_vrv_null_bad$error())
  expect_s3_class(error, "captured-stbl_error_bad_null")
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


# vrv_character_scalar ----

test_that("vrv_character_scalar() initializes as expected", {
  char_vrv <- vrv_character_scalar(value = "a")
  expect_equal(isolate(char_vrv()), "a")
  expect_false(isolate(char_vrv$is_default()))
  expect_null(isolate(char_vrv$error()))
})

test_that("vrv_character_scalar() allows setting a valid value", {
  char_vrv <- vrv_character_scalar(value = "a")
  char_vrv("b")
  expect_equal(isolate(char_vrv()), "b")
  expect_false(isolate(char_vrv$is_default()))
  expect_null(isolate(char_vrv$error()))
})

test_that("vrv_character_scalar() sets value to default when invalid (size)", {
  char_vrv <- vrv_character_scalar(
    value = "a",
    default = "default"
  )
  char_vrv(c("a", "b"))
  expect_equal(isolate(char_vrv()), "default")
  expect_true(isolate(char_vrv$is_default()))
  error <- isolate(char_vrv$error())
  expect_s3_class(error, "captured-stbl_error_non_scalar")
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

test_that("vrv_character_scalar() sets value to default when invalid (regex)", {
  char_vrv <- vrv_character_scalar(
    value = "a",
    default = "default",
    regex = "^[a-z]+$"
  )
  char_vrv("123")
  expect_equal(isolate(char_vrv()), "default")
  expect_true(isolate(char_vrv$is_default()))
  error <- isolate(char_vrv$error())
  expect_s3_class(error, "captured-stbl_error_must")
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

test_that("vrv_character_scalar() handles NULL initialization", {
  # allow_null = TRUE (default)
  char_vrv_null_ok <- vrv_character_scalar(
    value = NULL,
    default = "default"
  )
  expect_null(isolate(char_vrv_null_ok()))
  expect_false(isolate(char_vrv_null_ok$is_default()))

  # allow_null = FALSE
  char_vrv_null_bad <- vrv_character_scalar(
    value = NULL,
    default = "default",
    allow_null = FALSE
  )
  expect_equal(isolate(char_vrv_null_bad()), "default")
  expect_true(isolate(char_vrv_null_bad$is_default()))
  error <- isolate(char_vrv_null_bad$error())
  expect_s3_class(error, "captured-stbl_error_bad_null")
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

test_that("vrv_character_scalar() handles zero-length character vector", {
  # allow_zero_length = TRUE (default)
  char_vrv_zero_ok <- vrv_character_scalar(
    value = "a",
    default = "default"
  )
  char_vrv_zero_ok(character())
  expect_equal(isolate(char_vrv_zero_ok()), character())
  expect_false(isolate(char_vrv_zero_ok$is_default()))

  # allow_zero_length = FALSE
  char_vrv_zero_bad <- vrv_character_scalar(
    value = "a",
    default = "default",
    allow_zero_length = FALSE
  )
  char_vrv_zero_bad(character())
  expect_equal(isolate(char_vrv_zero_bad()), "default")
  expect_true(isolate(char_vrv_zero_bad$is_default()))
  error <- isolate(char_vrv_zero_bad$error())
  expect_s3_class(error, "captured-stbl_error_bad_empty")
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

test_that("vrv_character() initializes as expected", {
  char_vrv <- vrv_character(value = "a")
  expect_equal(isolate(char_vrv()), "a")
})

test_that("vrv_character() allows setting a valid value", {
  char_vrv <- vrv_character(value = "a")
  char_vrv("b")
  expect_equal(isolate(char_vrv()), "b")
})

test_that("vrv_character() sets value to default when invalid (size)", {
  char_vrv <- vrv_character(
    value = "a",
    default = "default",
    max_size = 1
  )
  char_vrv(c("a", "b"))
  expect_equal(isolate(char_vrv()), "default")
})

test_that("vrv_character() sets value to default when invalid (regex)", {
  char_vrv <- vrv_character(
    value = "a",
    default = "default",
    regex = "^[a-z]+$"
  )
  char_vrv("123")
  expect_equal(isolate(char_vrv()), "default")
})

test_that("vrv_character() handles NULL initialization", {
  # allow_null = TRUE (default)
  char_vrv_null_ok <- vrv_character(
    value = NULL,
    default = "default"
  )
  expect_null(isolate(char_vrv_null_ok()))

  # allow_null = FALSE
  char_vrv_null_bad <- vrv_character(
    value = NULL,
    default = "default",
    allow_null = FALSE
  )
  expect_equal(isolate(char_vrv_null_bad()), "default")
})

test_that("vrv_character() handles being set to NULL", {
  # allow_null = TRUE (default)
  char_vrv_null_ok <- vrv_character(
    value = "a",
    default = "default"
  )
  char_vrv_null_ok(NULL)
  expect_null(isolate(char_vrv_null_ok()))

  # allow_null = FALSE
  char_vrv_null_bad <- vrv_character(
    value = "a",
    default = "default",
    allow_null = FALSE
  )
  char_vrv_null_bad(NULL)
  expect_equal(isolate(char_vrv_null_bad()), "default")
})


# vrv_character_scalar ----

test_that("vrv_character_scalar() initializes as expected", {
  char_vrv <- vrv_character_scalar(value = "a")
  expect_equal(isolate(char_vrv()), "a")
})

test_that("vrv_character_scalar() allows setting a valid value", {
  char_vrv <- vrv_character_scalar(value = "a")
  char_vrv("b")
  expect_equal(isolate(char_vrv()), "b")
})

test_that("vrv_character_scalar() sets value to default when invalid (size)", {
  char_vrv <- vrv_character_scalar(
    value = "a",
    default = "default"
  )
  char_vrv(c("a", "b"))
  expect_equal(isolate(char_vrv()), "default")
})

test_that("vrv_character_scalar() sets value to default when invalid (regex)", {
  char_vrv <- vrv_character_scalar(
    value = "a",
    default = "default",
    regex = "^[a-z]+$"
  )
  char_vrv("123")
  expect_equal(isolate(char_vrv()), "default")
})

test_that("vrv_character_scalar() handles NULL initialization", {
  # allow_null = TRUE (default)
  char_vrv_null_ok <- vrv_character_scalar(
    value = NULL,
    default = "default"
  )
  expect_null(isolate(char_vrv_null_ok()))

  # allow_null = FALSE
  char_vrv_null_bad <- vrv_character_scalar(
    value = NULL,
    default = "default",
    allow_null = FALSE
  )
  expect_equal(isolate(char_vrv_null_bad()), "default")
})

test_that("vrv_character_scalar() handles being set to NULL", {
  # allow_null = TRUE (default)
  char_vrv_null_ok <- vrv_character_scalar(
    value = "a",
    default = "default"
  )
  char_vrv_null_ok(NULL)
  expect_null(isolate(char_vrv_null_ok()))

  # allow_null = FALSE
  char_vrv_null_bad <- vrv_character_scalar(
    value = "a",
    default = "default",
    allow_null = FALSE
  )
  char_vrv_null_bad(NULL)
  expect_equal(isolate(char_vrv_null_bad()), "default")
})

test_that("vrv_character_scalar() handles zero-length character vector", {
  # allow_zero_length = TRUE (default)
  char_vrv_zero_ok <- vrv_character_scalar(
    value = "a",
    default = "default"
  )
  char_vrv_zero_ok("")
  expect_equal(isolate(char_vrv_zero_ok()), "")

  # allow_zero_length = FALSE
  char_vrv_zero_bad <- vrv_character_scalar(
    value = "a",
    default = "default",
    allow_zero_length = FALSE
  )
  char_vrv_zero_bad(character())
  expect_equal(isolate(char_vrv_zero_bad()), "default")
})

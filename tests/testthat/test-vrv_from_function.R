test_that("vrv_from_function works with a simple function", {
  my_chr_rctv <- vrv_from_function(
    validation_fn = as.character,
    value = "a"
  )
  expect_equal(shiny::isolate(my_chr_rctv()), "a")
  expect_false(isolate(my_chr_rctv$is_default()))
  expect_null(isolate(my_chr_rctv$error()))

  my_chr_rctv("b")
  expect_equal(shiny::isolate(my_chr_rctv()), "b")
  expect_false(isolate(my_chr_rctv$is_default()))
  expect_null(isolate(my_chr_rctv$error()))
})

test_that("vrv_from_function generates reactives that generate errors as expected", {
  my_chr_rctv <- vrv_from_function(
    validation_fn = as.character,
    value = "a"
  )
  my_chr_rctv(mean)
  expect_null(shiny::isolate(my_chr_rctv()))
  expect_true(isolate(my_chr_rctv$is_default()))
  error <- isolate(my_chr_rctv$error())
  expect_s3_class(error, "captured-error")
  class(error) <- sub("captured-", "", class(error))
  expect_error(
    {
      signalCondition(error)
    },
    "cannot coerce"
  )
  expect_snapshot(
    {
      signalCondition(error)
    },
    error = TRUE
  )
})

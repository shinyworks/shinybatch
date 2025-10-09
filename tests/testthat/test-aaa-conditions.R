test_that(".chains_abort throws classed errors", {
  stbl::expect_pkg_error_classes(
    .chains_abort("Test message", subclass = "test"),
    "chains",
    "test"
  )
  stbl::expect_pkg_error_classes(
    .chains_abort("Test message", subclass = c("test", "test2")),
    "chains",
    "test",
    "test2"
  )
})

test_that(".with_error_handling returns value on success", {
  result <- .with_error_handling(
    expr = {
      1 + 1
    },
    message = "Should not see this",
    subclass = "success"
  )
  expect_equal(result, 2)
})

test_that(".with_error_handling throws classed error on failure", {
  expect_error(
    {
      .with_error_handling(
        expr = {
          stop("Original error")
        },
        message = "New message",
        subclass = "failure"
      )
    },
    class = "chains-error-failure"
  )
})

test_that(".with_error_handling throws an error that looks how we expect", {
  expect_snapshot(
    {
      .with_error_handling(
        expr = {
          stop("Original error")
        },
        message = "New message",
        subclass = "failure"
      )
    },
    error = TRUE
  )
})

test_that(".with_error_handling executes before_error expression", {
  x <- 1
  expect_error({
    .with_error_handling(
      expr = {
        stop("Original error")
      },
      message = "New message",
      subclass = "before_error",
      before_error = {
        x <- 2
      }
    )
  })
  expect_equal(x, 2)
})

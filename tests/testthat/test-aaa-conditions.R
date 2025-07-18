test_that("shinybatch_abort throws classed errors", {
  expect_error(
    {
      shinybatch_abort("Test message", subclass = "test")
    },
    class = c("shinybatch-error-test")
  )

  expect_error(
    {
      shinybatch_abort("Test message", subclass = c("test", "test2"))
    },
    class = c("shinybatch-error-test")
  )

  expect_error(
    {
      shinybatch_abort("Test message", subclass = c("test", "test2"))
    },
    class = c("shinybatch-error-test2")
  )

  expect_error(
    {
      shinybatch_abort("Test message", subclass = "test")
    },
    class = "shinybatch-error"
  )

  expect_error(
    {
      shinybatch_abort("Test message", subclass = "test")
    },
    class = "shinybatch-condition"
  )
})

test_that("shinybatch_abort message formatting is correct", {
  expect_snapshot(
    {
      shinybatch_abort("This is a test message.", subclass = "snapshot")
    },
    error = TRUE
  )

  expect_snapshot(
    {
      bad_variable <- "bad"
      shinybatch_abort(
        c(
          "This is a test message with a variable: {bad_variable}",
          i = "This is some additional info."
        ),
        subclass = "snapshot_complex"
      )
    },
    error = TRUE
  )
})

test_that("shinybatch_abort messages mention the parent function", {
  wrapper <- function() {
    shinybatch_abort("This is a test message.", subclass = "snapshot")
  }
  expect_snapshot(
    {
      wrapper()
    },
    error = TRUE
  )
})

test_that("with_error_handling returns value on success", {
  result <- with_error_handling(
    expr = {
      1 + 1
    },
    message = "Should not see this",
    subclass = "success"
  )
  expect_equal(result, 2)
})

test_that("with_error_handling throws classed error on failure", {
  expect_error(
    {
      with_error_handling(
        expr = {
          stop("Original error")
        },
        message = "New message",
        subclass = "failure"
      )
    },
    class = "shinybatch-error-failure"
  )
})

test_that("with_error_handling throws an error that looks how we expect", {
  expect_snapshot(
    {
      with_error_handling(
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

test_that("with_error_handling executes before_error expression", {
  x <- 1
  expect_error({
    with_error_handling(
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

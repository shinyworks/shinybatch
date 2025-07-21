test_that("Categorical dependency works as expected", {
  mock_data <- list(
    "A" = list("A1" = 1, "A2" = 2),
    "B" = list("B1" = 3, "B2" = 4),
    "C" = list("A1" = 5, "B2" = 6)
  )

  testServer(
    function(input, output, session) {
      level <- shiny::reactiveVal("A")
      group <- validated_reactive_val(
        value = "A1",
        validation_expr = {
          valid_groups <- names(mock_data[[level()]])
          if (self() %in% valid_groups) {
            self()
          } else {
            valid_groups[[1]]
          }
        }
      )
    },
    {
      # Test 1: Change level, group should be reset
      level("B")
      expect_equal(level(), "B")
      expect_equal(group(), "B1")

      # Test 2: Change level, group should remain valid
      level("C")
      expect_equal(level(), "C")
      expect_equal(group(), "A1")

      # Test 3: Set group to a valid value
      group("B2")
      expect_equal(level(), "C")
      expect_equal(group(), "B2")

      # Test 4: Attempt to set group to an invalid value
      level("A")
      expect_equal(group(), "A1")
      group("B1")
      expect_equal(group(), "A1")
    }
  )
})

test_that("Range-based dependency works as expected", {
  testServer(
    function(input, output, session) {
      min_val <- shiny::reactiveVal(0)
      max_val <- shiny::reactiveVal(10)
      value_in_range <- validated_reactive_val(
        value = 5,
        validation_expr = {
          min_v <- min_val()
          max_v <- max_val()
          if (is.numeric(self()) && self() >= min_v && self() <= max_v) {
            self()
          } else {
            min_v
          }
        }
      )
    },
    {
      expect_equal(value_in_range(), 5)

      max_val(4)
      expect_equal(value_in_range(), 0)

      min_val(6)
      max_val(10)
      value_in_range(5)
      min_val(6)
      expect_equal(value_in_range(), 6)

      min_val(0)
      max_val(10)
      value_in_range(12)
      expect_equal(value_in_range(), 0)
    }
  )
})

test_that("validated_reactive_val enforces consistency", {
  mock_data <- list(
    "A" = list("A1" = 1),
    "B" = list("B1" = 2)
  )

  testServer(
    function(input, output, session) {
      level <- shiny::reactiveVal("A")
      group <- validated_reactive_val(
        value = "A1",
        validation_expr = {
          valid_groups <- names(mock_data[[level()]])
          if (self() %in% valid_groups) self() else valid_groups[[1]]
        }
      )
      consistency_check <- shiny::reactiveVal()

      # This "sensor" observer should now return TRUE. The reactive system
      # will ensure the `validated_reactive_val` is consistent before this
      # observer runs.
      observeEvent(
        level(),
        {
          consistency_check(group() %in% names(mock_data[[level()]]))
        },
        priority = 1000
      )
    },
    {
      # Trigger the change
      level("B")
      session$flushReact()
      # The sensor now returns the success value instead of erroring.
      expect_true(consistency_check())
    }
  )
})

test_that("`env` parameter is respected", {
  test_env <- rlang::env(sentinel = 10)
  testServer(
    function(input, output, session) {
      vrv <- validated_reactive_val(
        value = 5,
        validation_expr = {
          self() + sentinel
        },
        env = test_env
      )
    },
    {
      expect_equal(vrv(), 15)
    }
  )
})

test_that("`value` defaults to NULL", {
  testServer(
    function(input, output, session) {
      vrv <- validated_reactive_val(
        validation_expr = {
          if (is.null(self())) "was null" else "not null"
        }
      )
    },
    {
      expect_equal(vrv(), "was null")
    }
  )
})

test_that("`label` parameter does not cause errors", {
  testServer(
    function(input, output, session) {
      vrv <- validated_reactive_val(
        value = 1,
        label = "my_vrv",
        validation_expr = {
          self()
        }
      )
    },
    {
      expect_equal(vrv(), 1)
      vrv(2)
      expect_equal(vrv(), 2)
    }
  )
})

test_that("`label` appears in error messages", {
  vrv <- validated_reactive_val(
    label = "my-label-test",
    validation_expr = {
      stop("force error")
    }
  )
  error_call <- tryCatch(
    isolate(vrv()),
    error = function(cnd) {
      rlang::call_name(cnd$call)
    }
  )
  expect_equal(error_call, "<reactive:my-label-test-validation>")
})

test_that("validation errors are informative", {
  vrv <- validated_reactive_val(
    validation_expr = {
      stop("original error")
    }
  )
  expect_error(
    isolate(vrv()),
    class = "shinybatch-error-validation"
  )
  expect_snapshot(
    isolate(vrv()),
    error = TRUE
  )
})

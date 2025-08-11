# Categorical Dependency Tests ----

test_that("vrv with categorical dependency initializes correctly", {
  mock_data <- list("A" = c("A1", "A2"))
  level <- shiny::reactiveVal("A")
  group <- validated_reactive_val(value = "A1", validation_expr = {
    valid_groups <- mock_data[[level()]]
    if (.vrv() %in% valid_groups) .vrv() else valid_groups[[1]]
  })
  expect_equal(isolate(group()), "A1")
})

test_that("vrv with categorical dependency can be set to a valid value", {
  mock_data <- list("A" = c("A1", "A2"))
  level <- shiny::reactiveVal("A")
  group <- validated_reactive_val(value = "A1", validation_expr = {
    valid_groups <- mock_data[[level()]]
    if (.vrv() %in% valid_groups) .vrv() else valid_groups[[1]]
  })
  group("A2")
  expect_equal(isolate(group()), "A2")
})

test_that("vrv with categorical dependency resets when invalid", {
  mock_data <- list("A" = "A1", "B" = "B1")
  level <- shiny::reactiveVal("A")
  group <- validated_reactive_val(value = "A1", validation_expr = {
    valid_groups <- mock_data[[level()]]
    if (.vrv() %in% valid_groups) .vrv() else valid_groups[[1]]
  })
  level("B")
  expect_equal(isolate(group()), "B1")
})

test_that("vrv with categorical dependency remains valid with overlapping groups", {
  mock_data <- list("A" = "A1", "B" = c("A1", "B1"))
  level <- shiny::reactiveVal("A")
  group <- validated_reactive_val(value = "A1", validation_expr = {
    valid_groups <- mock_data[[level()]]
    if (.vrv() %in% valid_groups) .vrv() else valid_groups[[1]]
  })
  level("B")
  expect_equal(isolate(group()), "A1")
})

test_that("vrv with categorical dependency corrects an invalid imperative set", {
  mock_data <- list("A" = "A1", "B" = "B1")
  level <- shiny::reactiveVal("A")
  group <- validated_reactive_val(value = "A1", validation_expr = {
    valid_groups <- mock_data[[level()]]
    if (.vrv() %in% valid_groups) .vrv() else valid_groups[[1]]
  })
  group("B1") # Invalid for level "A"
  expect_equal(isolate(group()), "A1")
})

# Range-based Dependency Tests ----

test_that("vrv with range dependency initializes correctly", {
  min_val <- shiny::reactiveVal(0)
  max_val <- shiny::reactiveVal(10)
  value_in_range <- validated_reactive_val(
    value = 5,
    validation_expr = {
      min_v <- min_val()
      max_v <- max_val()
      if (is.numeric(.vrv()) && .vrv() >= min_v && .vrv() <= max_v) {
        .vrv()
      } else {
        min_v
      }
    }
  )
  expect_equal(isolate(value_in_range()), 5)
})

test_that("vrv with range dependency resets when max changes", {
  min_val <- shiny::reactiveVal(0)
  max_val <- shiny::reactiveVal(10)
  value_in_range <- validated_reactive_val(
    value = 5,
    validation_expr = {
      min_v <- min_val()
      max_v <- max_val()
      if (is.numeric(.vrv()) && .vrv() >= min_v && .vrv() <= max_v) {
        .vrv()
      } else {
        min_v
      }
    }
  )
  max_val(4)
  expect_equal(isolate(value_in_range()), 0)
})

test_that("vrv with range dependency resets when min changes", {
  min_val <- shiny::reactiveVal(0)
  max_val <- shiny::reactiveVal(10)
  value_in_range <- validated_reactive_val(
    value = 5,
    validation_expr = {
      min_v <- min_val()
      max_v <- max_val()
      if (is.numeric(.vrv()) && .vrv() >= min_v && .vrv() <= max_v) {
        .vrv()
      } else {
        min_v
      }
    }
  )
  min_val(6)
  expect_equal(isolate(value_in_range()), 6)
})

test_that("vrv with range dependency corrects an invalid imperative set", {
  min_val <- shiny::reactiveVal(0)
  max_val <- shiny::reactiveVal(10)
  value_in_range <- validated_reactive_val(
    value = 5,
    validation_expr = {
      min_v <- min_val()
      max_v <- max_val()
      if (is.numeric(.vrv()) && .vrv() >= min_v && .vrv() <= max_v) {
        .vrv()
      } else {
        min_v
      }
    }
  )
  value_in_range(12)
  expect_equal(isolate(value_in_range()), 0)
})


# Other Tests ----

test_that("validated_reactive_val enforces consistency", {
  mock_data <- list(
    "A" = list("A1" = 1),
    "B" = list("B1" = 2)
  )

  testServer(
    function(input, output, session) {
      level <- shiny::reactiveVal("A")
      group <- validated_reactive_val(value = "A1", validation_expr = {
        valid_groups <- names(mock_data[[level()]])
        if (.vrv() %in% valid_groups) .vrv() else valid_groups[[1]]
      })
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
  vrv <- validated_reactive_val(
    value = 5,
    validation_expr = {
      .vrv() + sentinel
    },
    env = test_env
  )
  expect_equal(isolate(vrv()), 15)
})

test_that("`value` defaults to NULL", {
  vrv <- validated_reactive_val(
    validation_expr = {
      if (is.null(.vrv())) "was null" else "not null"
    }
  )
  expect_equal(isolate(vrv()), "was null")
})

test_that("`label` parameter does not cause errors", {
  vrv <- validated_reactive_val(
    value = 1,
    label = "my_vrv",
    validation_expr = {
      .vrv()
    }
  )
  expect_equal(isolate(vrv()), 1)
  vrv(2)
  expect_equal(isolate(vrv()), 2)
})

test_that("`label` appears in error messages", {
  vrv <- validated_reactive_val(label = "my-label-test", validation_expr = {
    stop("force error")
  })
  cnd <- expect_error(isolate(vrv()))
  expect_equal(
    rlang::call_name(cnd$call),
    "<reactive:my-label-test-validation>"
  )
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

test_that("pronoun doesn't conflict with user-defined variables", {
  # User names their validated reactive value `.vrv`, which could
  # conflict with the pronoun.
  .vrv <- validated_reactive_val(
    value = 1,
    validation_expr = {
      # Inside the validation expression, `.vrv()` should refer to the
      # pronoun, not the object itself, preventing an infinite loop.
      .vrv() + 1
    }
  )
  # The initial value is 1. The validation expression adds 1.
  # So the first reactive read should be 2.
  expect_equal(isolate(.vrv()), 2)

  # Now, let's set it imperatively.
  .vrv(5)
  # The next reactive read should be 5 + 1 = 6.
  expect_equal(isolate(.vrv()), 6)
})

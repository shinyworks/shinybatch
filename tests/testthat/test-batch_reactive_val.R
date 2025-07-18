test_that("Categorical dependency works as expected", {
  mock_data <- list(
    "A" = list("A1" = 1, "A2" = 2),
    "B" = list("B1" = 3, "B2" = 4),
    "C" = list("A1" = 5, "B2" = 6)
  )

  testServer(
    function(input, output, session) {
      level <- batch_reactive_val("level", "A")
      group <- batch_reactive_val(
        "group",
        "A1",
        orchestrator = level,
        validation_fun = function(my_value, all_values) {
          valid_groups <- names(mock_data[[all_values$level]])
          if (my_value %in% valid_groups) {
            return(my_value)
          }
          return(valid_groups[[1]])
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
      min_val <- batch_reactive_val("min_val", 0)
      max_val <- batch_reactive_val("max_val", 10, orchestrator = min_val)
      value_in_range <- batch_reactive_val(
        "value_in_range",
        5,
        orchestrator = min_val,
        validation_fun = function(my_value, all_values) {
          min_v <- all_values$min_val
          max_v <- all_values$max_val
          if (is.numeric(my_value) && my_value >= min_v && my_value <= max_v) {
            my_value
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

test_that("Normal reactiveVals fail in the way we're watching", {
  # This test is here to make sure this package is still doing something useful.
  mock_data <- list(
    "A" = list("A1" = 1),
    "B" = list("B1" = 2)
  )

  testServer(
    function(input, output, session) {
      level <- reactiveVal("A")
      group <- reactiveVal("A1")
      consistency_check <- reactiveVal()

      # This observer updates group with normal priority
      observeEvent(
        level(),
        {
          new_level <- level()
          valid_groups <- names(mock_data[[new_level]])
          group(valid_groups[1])
        }
      )

      # This observer acts as a "sensor". It runs at a higher priority than the
      # observer above. It will see the inconsistent state.
      observeEvent(
        level(),
        {
          consistency_check(group() %in% names(mock_data[[level()]]))
        },
        priority = 1000
      )
    },
    {
      level("B")
      session$flushReact()
      # Try to read the sensor. It will fail because its `req()` condition
      # was not met due to the inconsistent state.
      expect_false(consistency_check())
    }
  )
})

test_that("batch_reactive_vals enforce consistency", {
  mock_data <- list(
    "A" = list("A1" = 1),
    "B" = list("B1" = 2)
  )

  testServer(
    function(input, output, session) {
      level <- batch_reactive_val("level", "A")
      group <- batch_reactive_val(
        "group", "A1",
        orchestrator = level,
        validation_fun = function(v, vv) {
          valid_groups <- names(mock_data[[vv$level]])
          if (v %in% valid_groups) v else valid_groups[1]
        }
      )
      consistency_check <- reactiveVal()

      # This "sensor" observer should now return TRUE. The `batch_reactive_val`
      # system will pause until the transaction is complete. Once it runs, the
      # state is guaranteed to be consistent.
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

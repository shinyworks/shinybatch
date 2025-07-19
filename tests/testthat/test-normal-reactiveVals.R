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
